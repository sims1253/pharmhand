# Tests for R/efficacy_subgroup.R
library(testthat)
library(pharmhand)

test_that("create_subgroup_analysis_table works", {
	# Not heavily used in function but passed
	adsl <- data.frame(USUBJID = c("01", "02"))
	advs <- data.frame(
		USUBJID = c("01", "02", "03", "04"),
		TRT01P = c("A", "A", "B", "B"),
		PARAMCD = c("SYSBP", "SYSBP", "SYSBP", "SYSBP"),
		AVISIT = c(
			"End of Treatment",
			"End of Treatment",
			"End of Treatment",
			"End of Treatment"
		),
		AVAL = c(120, 130, 140, 150),
		AGEGR1 = c("<65", ">=65", "<65", ">=65"),
		SEX = c("M", "F", "M", "F")
	)

	tbl <- create_subgroup_analysis_table(adsl, advs, min_subgroup_size = NULL)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "subgroup")
	expect_true(all(c("Subgroup", "Category") %in% names(tbl@data)))
	expect_true(any(tbl@data$Subgroup == "Age Group"))
	expect_true(any(tbl@data$Subgroup == "Sex"))
})

test_that("create_subgroup_analysis_table warns for small subgroups", {
	adsl <- data.frame(USUBJID = sprintf("SUBJ%02d", 1:8))
	advs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:8),
		TRT01P = rep(c("A", "B"), each = 4),
		PARAMCD = rep("SYSBP", 8),
		AVISIT = rep("End of Treatment", 8),
		AVAL = c(120, 122, 118, 130, 125, 128, 135, 140),
		SEX = rep(c("M", "F"), times = 4),
		stringsAsFactors = FALSE
	)

	expect_warning(
		create_subgroup_analysis_table(
			adsl,
			advs,
			subgroups = list(SEX = "Sex")
		),
		"Small subgroup warning"
	)

	expect_no_warning(
		create_subgroup_analysis_table(
			adsl,
			advs,
			subgroups = list(SEX = "Sex"),
			min_subgroup_size = NULL
		)
	)
})

test_that("create_subgroup_analysis_table pulls subgroup vars from adsl", {
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		SEX = c("M", "F")
	)
	advs <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		AVAL = c(120, 130),
		AGEGR1 = c("<65", ">=65")
	)

	tbl <- create_subgroup_analysis_table(
		adsl,
		advs,
		subgroups = list(AGEGR1 = "Age Group", SEX = "Sex"),
		min_subgroup_size = NULL
	)

	expect_true(any(tbl@data$Subgroup == "Sex"))
})

test_that("create_subgroup_analysis_table errors on duplicate adsl USUBJID", {
	adsl <- data.frame(
		USUBJID = c("01", "01"),
		SEX = c("M", "F")
	)
	advs <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		AVAL = c(120, 130),
		AGEGR1 = c("<65", ">=65")
	)

	expect_error(
		create_subgroup_analysis_table(
			adsl,
			advs,
			subgroups = list(AGEGR1 = "Age Group", SEX = "Sex")
		),
		"'adsl' must have unique USUBJID"
	)
})

test_that("create_subgroup_analysis_table errors on missing subgroup vars", {
	adsl <- data.frame(USUBJID = c("01", "02"))
	advs <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		AVAL = c(120, 130)
	)

	expect_error(
		create_subgroup_analysis_table(adsl, advs, subgroups = list(FOO = "Foo")),
		"Subgroup variables not found"
	)
})

test_that("create_subgroup_analysis_table validates inputs", {
	adsl <- data.frame(USUBJID = c("01", "02"))

	expect_error(
		create_subgroup_analysis_table(NULL, data.frame()),
		"must be a data frame"
	)
	expect_error(
		create_subgroup_analysis_table(adsl, NULL),
		"must be a data frame"
	)
})

test_that("create_subgroup_table works with TTE endpoint", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		AGEGR1 = sample(c("<65", ">=65"), 60, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adtte,
		subgroups = list(SEX = "Sex", AGEGR1 = "Age Group"),
		endpoint_type = "tte"
	)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "subgroup")
	expect_true("Subgroup" %in% names(tbl@data))
	expect_true("Category" %in% names(tbl@data))
	expect_true(any(grepl("HR", names(tbl@data), fixed = TRUE)))
	expect_true(any(tbl@data$Subgroup == "Overall"))
	expect_true(any(tbl@data$Subgroup == "Sex"))
	expect_true(any(tbl@data$Subgroup == "Age Group"))
})

test_that("create_subgroup_table works with binary endpoint", {
	set.seed(42)
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 60, replace = TRUE),
		SEX = rep(c("M", "F"), 30),
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adrs,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "binary",
		response_values = c("CR", "PR")
	)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "subgroup")
	expect_true(any(grepl("OR", names(tbl@data), fixed = TRUE)))
})

test_that("create_subgroup_table includes interaction p-values", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adtte,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte",
		show_interaction = TRUE
	)

	expect_true("Interaction p" %in% names(tbl@data))
})

test_that("create_subgroup_table can disable interaction p-values", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adtte,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte",
		show_interaction = FALSE
	)

	expect_false("Interaction p" %in% names(tbl@data))
})

test_that("create_subgroup_table warns for small subgroups", {
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:30),
		TRT01P = rep(c("Placebo", "Active"), each = 15),
		SEX = c(
			rep(c("F", "M"), times = c(5, 10)),
			rep(c("F", "M"), times = c(5, 10))
		),
		AVALC = rep(c("CR", "SD", "PR", "PD", "SD", "CR"), length.out = 30),
		stringsAsFactors = FALSE
	)

	expect_warning(
		create_subgroup_table(
			adrs,
			subgroups = list(SEX = "Sex"),
			endpoint_type = "binary",
			show_interaction = FALSE,
			min_subgroup_size = 15
		),
		"Small subgroup warning"
	)

	expect_no_warning(
		create_subgroup_table(
			adrs,
			subgroups = list(SEX = "Sex"),
			endpoint_type = "binary",
			show_interaction = FALSE,
			min_subgroup_size = 5
		)
	)
})

test_that("create_subgroup_table warns for missing subgroup variables", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	expect_warning(
		create_subgroup_table(
			adtte,
			subgroups = list(NONEXISTENT = "Missing Variable"),
			endpoint_type = "tte"
		),
		"not found"
	)
})

test_that("create_subgroup_table handles missing subgroup variable", {
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE),
		stringsAsFactors = FALSE
	)

	expect_warning(
		create_subgroup_table(
			adtte,
			subgroups = list(NONEXISTENT = "Missing"),
			endpoint_type = "tte"
		),
		"not found"
	)
})

test_that("create_subgroup_table with empty subgroup levels", {
	# Use larger sample and balanced treatment assignment per sex to avoid
	# "Loglik converged before variable 1" warnings
	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), 30), # Alternate for balance
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(rep(c("M", "F"), each = 15), 2), # 15 each per treatment
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adtte,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte"
	)

	expect_s7_class(tbl, ClinicalTable)
	# Should have Overall + Male + Female rows
	expect_equal(nrow(tbl@data), 3)
})

test_that("create_subgroup_table with multiplicity adjustment", {
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:80),
		TRT01P = rep(c("Placebo", "Active"), each = 40),
		AVAL = c(rexp(40, 0.05), rexp(40, 0.03)),
		CNSR = sample(0:1, 80, replace = TRUE),
		SEX = rep(c("M", "F"), 40),
		AGEGR1 = sample(c("<65", ">=65"), 80, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adtte,
		subgroups = list(SEX = "Sex", AGEGR1 = "Age Group"),
		endpoint_type = "tte",
		adjust_method = "holm"
	)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@metadata$adjust_method, "holm")
	# Check full results metadata for interaction_p
	expect_true("full_results" %in% names(tbl@metadata))
	expect_true("interaction_p" %in% names(tbl@metadata$full_results))
})

test_that("create_subgroup_table disables multiplicity adjustment", {
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE),
		SEX = rep(c("M", "F"), 30),
		AGEGR1 = sample(c("<65", ">=65"), 60, replace = TRUE),
		stringsAsFactors = FALSE
	)

	tbl_none <- create_subgroup_table(
		adtte,
		subgroups = list(SEX = "Sex", AGEGR1 = "Age Group"),
		endpoint_type = "tte",
		adjust_method = "none"
	)

	tbl_holm <- create_subgroup_table(
		adtte,
		subgroups = list(SEX = "Sex", AGEGR1 = "Age Group"),
		endpoint_type = "tte",
		adjust_method = "holm"
	)

	expect_equal(tbl_none@metadata$adjust_method, "none")
	expect_equal(tbl_holm@metadata$adjust_method, "holm")
})

test_that("create_subgroup_table handles small subgroup with binary endpoint", {
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:30),
		TRT01P = rep(c("Placebo", "Active"), each = 15),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 30, replace = TRUE),
		SEX = c(
			rep(c("M", "F"), times = c(5, 10)),
			rep(c("M", "F"), times = c(10, 5))
		),
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adrs,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "binary",
		response_values = c("CR", "PR"),
		min_subgroup_size = NULL
	)

	expect_s7_class(tbl, ClinicalTable)
	# One subgroup may have NE (Not Estimable) due to small size
})

test_that("create_subgroup_table with binary endpoint handles no events", {
	# Use 50 subjects (25 per subgroup) to avoid small subgroup warning
	adrs <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:50),
		TRT01P = rep(c("Placebo", "Active"), each = 25),
		AVALC = c(rep("SD", 25), rep("PD", 25)), # No responders
		SEX = rep(c("M", "F"), 25),
		stringsAsFactors = FALSE
	)

	tbl <- create_subgroup_table(
		adrs,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "binary",
		response_values = c("CR", "PR")
	)

	expect_s7_class(tbl, ClinicalTable)
	# Should still create table even with no events
})
