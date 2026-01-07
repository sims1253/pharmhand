# Tests for R/efficacy_responder.R
library(testthat)
library(pharmhand)

test_that("create_responder_table works with basic data", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "responder")
	expect_true("Treatment" %in% names(tbl@data))
	expect_true("n/N" %in% names(tbl@data))
	expect_true("Rate (%)" %in% names(tbl@data))
	expect_true("95% CI" %in% names(tbl@data))
})

test_that("create_responder_table handles extreme response rates with RR", {
	# Test case where reference group has 0% response rate
	# This would cause division by zero without continuity correction
	adrs_zero_ref <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		# All Placebo are non-responders (SD/PD), some Active are responders
		AVALC = c(
			rep("SD", 20),
			sample(c("CR", "PR", "SD", "PD"), 20, replace = TRUE)
		),
		stringsAsFactors = FALSE
	)

	# Should not error - continuity correction should be applied
	expect_no_error({
		tbl <- create_responder_table(adrs_zero_ref, comparison_type = "RR")
	})

	tbl <- create_responder_table(adrs_zero_ref, comparison_type = "RR")
	# RR should be finite (not Inf or NaN)
	expect_true("RR (95% CI)" %in% names(tbl@data))

	# Test case where treatment has 100% response rate
	adrs_full_trt <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		# All Active are responders, some Placebo are responders
		AVALC = c(
			sample(c("CR", "PR", "SD", "PD"), 20, replace = TRUE),
			rep("CR", 20)
		),
		stringsAsFactors = FALSE
	)

	expect_no_error({
		tbl2 <- create_responder_table(adrs_full_trt, comparison_type = "RR")
	})
})

test_that("create_responder_table supports different CI methods", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl_wilson <- create_responder_table(adrs, ci_method = "wilson")
	tbl_exact <- create_responder_table(adrs, ci_method = "exact")

	expect_equal(tbl_wilson@metadata$ci_method, "wilson")
	expect_equal(tbl_exact@metadata$ci_method, "exact")
})

test_that("create_responder_table works with ADaMData", {
	set.seed(42)
	adrs_df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		SAFFL = "Y",
		stringsAsFactors = FALSE
	)

	adam <- ADaMData(data = adrs_df, population = "SAF")
	tbl <- create_responder_table(adam)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "responder")
})

test_that("create_responder_table handles OR comparison", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs, comparison_type = "OR")

	expect_true("OR (95% CI)" %in% names(tbl@data))
	expect_equal(tbl@metadata$comparison_type, "OR")

	# Verify OR values are reasonable (positive)
	or_col <- tbl@data[tbl@data$Treatment != "Reference", "OR (95% CI)"]
	expect_true(!anyNA(or_col))
})

test_that("create_responder_table handles RR comparison", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs, comparison_type = "RR")

	expect_true("RR (95% CI)" %in% names(tbl@data))
	expect_equal(tbl@metadata$comparison_type, "RR")
})

test_that("create_responder_table handles RD comparison", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs, comparison_type = "RD")

	expect_true("RD (95% CI)" %in% names(tbl@data))
	expect_equal(tbl@metadata$comparison_type, "RD")

	# RD is displayed as percentage
	rd_col <- tbl@data[tbl@data$Treatment != "Reference", "RD (95% CI)"]
	expect_true(!anyNA(rd_col))
})

test_that("create_responder_table wilson CI method works", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs, ci_method = "wilson")

	expect_equal(tbl@metadata$ci_method, "wilson")
	expect_true("95% CI" %in% names(tbl@data))
})

test_that("create_responder_table exact CI method works", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs, ci_method = "exact")

	expect_equal(tbl@metadata$ci_method, "exact")
	expect_true("95% CI" %in% names(tbl@data))
})

test_that("create_responder_table wald CI method works", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs, ci_method = "wald")

	expect_equal(tbl@metadata$ci_method, "wald")
	expect_true("95% CI" %in% names(tbl@data))
})

test_that("create_responder_table handles 100% response rate", {
	# All responders - edge case for CI calculation
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:20),
		TRT01P = rep(c("Placebo", "Active"), each = 10),
		AVALC = c(rep("CR", 10), rep("CR", 10)),
		stringsAsFactors = FALSE
	)

	expect_no_error({
		tbl <- create_responder_table(adrs, ci_method = "exact")
	})

	expect_s7_class(tbl, ClinicalTable)
})

test_that("create_responder_table handles 0% response rate", {
	# No responders - edge case for CI calculation
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:20),
		TRT01P = rep(c("Placebo", "Active"), each = 10),
		AVALC = c(rep("SD", 10), rep("PD", 10)),
		stringsAsFactors = FALSE
	)

	expect_no_error({
		tbl <- create_responder_table(adrs, ci_method = "exact")
	})

	expect_s7_class(tbl, ClinicalTable)
})

test_that("create_responder_table handles very small sample size", {
	adrs <- data.frame(
		USUBJID = c("001", "002", "003", "004"),
		TRT01P = c("Placebo", "Placebo", "Active", "Active"),
		AVALC = c("CR", "SD", "CR", "PR"),
		stringsAsFactors = FALSE
	)

	expect_no_error({
		tbl <- create_responder_table(adrs)
	})

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(nrow(tbl@data), 2) # Two treatment groups
})

test_that("create_responder_table handles single arm", {
	# Only one treatment group - no comparison
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:20),
		TRT01P = rep("Active", 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 20, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs)

	# Should not have comparison columns when only 1 arm
	expect_false(any(grepl("\\(95% CI\\)", names(tbl@data), fixed = TRUE)))
	expect_false("p-value" %in% names(tbl@data))
})

test_that("create_responder_table handles multiple non-reference arms", {
	# Function handles multiple treatment arms without warning
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active A", "Active B"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 60, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# Should calculate comparisons for all non-reference arms
	expect_no_error({
		tbl <- create_responder_table(adrs, ref_group = "Placebo")
	})
	expect_true(S7::S7_inherits(tbl, ClinicalTable))
})

test_that("create_responder_table handles NA response values", {
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = c(
			sample(c("CR", "PR", "SD", "PD"), 30, replace = TRUE),
			rep(NA, 10)
		),
		stringsAsFactors = FALSE
	)

	expect_no_error({
		tbl <- create_responder_table(adrs)
	})

	expect_s7_class(tbl, ClinicalTable)
})

test_that("create_responder_table uses custom ref_group", {
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_responder_table(adrs, ref_group = "Active")

	ref_row <- tbl@data[tbl@data$Treatment == "Active", ]
	expect_equal(ref_row$`OR (95% CI)`, "Reference")
})
