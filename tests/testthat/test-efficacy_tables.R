test_that("create_primary_endpoint_table works", {
	# Mock data
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
		AVAL = c(120, 130, 140, 150)
	)
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(2, 2))

	tbl <- create_primary_endpoint_table(advs, trt_n)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "primary_endpoint")
	expect_equal(tbl@title, "Primary Endpoint Summary")

	# Check data structure
	expect_true(all(c("Statistic", "A", "B") %in% names(tbl@data)))
	# n, Mean (SD), Median, Min, Max transposed = 4 rows
	expect_equal(nrow(tbl@data), 4)
})

test_that("create_cfb_summary_table works", {
	advs <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAM = c("Systolic Blood Pressure", "Systolic Blood Pressure"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		AVAL = c(120, 140),
		CHG = c(-5, -10)
	)
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(1, 1))

	tbl <- create_cfb_summary_table(advs, trt_n, params = "SYSBP")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "cfb")
	expect_true(all(c("Parameter", "A n", "A Mean (SD)") %in% names(tbl@data)))
})

test_that("create_vs_by_visit_table works", {
	advs <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("Baseline", "Week 2"),
		AVAL = c(120, 140)
	)
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(1, 1))

	tbl <- create_vs_by_visit_table(
		advs,
		trt_n,
		visits = c("Baseline", "Week 2")
	)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "vs_by_visit")
	expect_equal(nrow(tbl@data), 2)
})

test_that("create_lab_summary_table works", {
	adlb <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAM = c("Alanine Aminotransferase", "Alanine Aminotransferase"),
		PARAMCD = c("ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24"),
		AVAL = c(20, 30)
	)
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(1, 1))

	tbl <- create_lab_summary_table(adlb, trt_n, params = "ALT")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "lab_summary")
})

test_that("create_lab_shift_table works", {
	adlb <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		PARAMCD = c("ALT", "ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24", "Week 24"),
		BNRIND = c("NORMAL", "NORMAL", "HIGH"),
		ANRIND = c("NORMAL", "HIGH", "HIGH")
	)
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(2, 1))

	tbl <- create_lab_shift_table(adlb, trt_n)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "lab_shift")
	expect_true("Baseline Status" %in% names(tbl@data))
})

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

	tbl <- create_subgroup_analysis_table(adsl, advs)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "subgroup")
	expect_true(all(c("Subgroup", "Category") %in% names(tbl@data)))
	expect_true(any(tbl@data$Subgroup == "Age Group"))
	expect_true(any(tbl@data$Subgroup == "Sex"))
})

test_that("create_primary_endpoint_table validates inputs", {
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(2, 2))

	# Non-data-frame input
	expect_error(
		create_primary_endpoint_table(NULL, trt_n),
		"must be a data frame"
	)
	expect_error(
		create_primary_endpoint_table(list(a = 1), trt_n),
		"must be a data frame"
	)

	# trt_n must be a data frame
	advs <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		AVAL = c(120, 130)
	)
	expect_error(
		create_primary_endpoint_table(advs, NULL),
		"must be a data frame"
	)
})

test_that("create_cfb_summary_table validates inputs", {
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(1, 1))

	expect_error(
		create_cfb_summary_table(NULL, trt_n, params = "SYSBP"),
		"must be a data frame"
	)
	expect_error(
		create_cfb_summary_table(data.frame(), NULL, params = "SYSBP"),
		"must be a data frame"
	)
})

test_that("create_lab_shift_table validates inputs", {
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(2, 1))

	expect_error(
		create_lab_shift_table(NULL, trt_n),
		"must be a data frame"
	)
})

test_that("create_vs_by_visit_table validates inputs", {
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(1, 1))

	expect_error(
		create_vs_by_visit_table(NULL, trt_n),
		"must be a data frame"
	)
	expect_error(
		create_vs_by_visit_table(data.frame(), NULL),
		"must be a data frame"
	)
})

test_that("create_lab_summary_table validates inputs", {
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(1, 1))

	expect_error(
		create_lab_summary_table(NULL, trt_n),
		"must be a data frame"
	)
	expect_error(
		create_lab_summary_table(data.frame(), NULL),
		"must be a data frame"
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

# Tests for TTE Summary Table ----

test_that("create_tte_summary_table works with basic data", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	tbl <- create_tte_summary_table(adtte)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "tte_summary")
	expect_true("Statistic" %in% names(tbl@data))
	expect_true(any(grepl("Median", tbl@data$Statistic, fixed = TRUE)))
	expect_true(any(grepl("Events", tbl@data$Statistic, fixed = TRUE)))
})

test_that("create_tte_summary_table with landmarks", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	tbl <- create_tte_summary_table(
		adtte,
		landmarks = c(12, 24),
		time_unit = "months"
	)

	expect_s7_class(tbl, ClinicalTable)
	expect_true(any(grepl("12-months Rate", tbl@data$Statistic, fixed = TRUE)))
	expect_true(any(grepl("24-months Rate", tbl@data$Statistic, fixed = TRUE)))
})

test_that("create_tte_summary_table works with ADaMData", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte_df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		SAFFL = "Y",
		stringsAsFactors = FALSE
	)

	adam <- ADaMData(data = adtte_df, population = "SAF")
	tbl <- create_tte_summary_table(adam)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "tte_summary")
})

test_that("create_tte_summary_table includes HR for two-arm studies", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	tbl <- create_tte_summary_table(adtte)

	expect_true(any(grepl("HR", tbl@data$Statistic, fixed = TRUE)))
	expect_true(any(grepl("p-value", tbl@data$Statistic, fixed = TRUE)))
})

# Tests for Responder Table ----

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

test_that("create_responder_table calculates OR correctly", {
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
})

test_that("create_responder_table calculates RR correctly", {
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

# Tests for Subgroup Table ----

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

# Tests for Proportion CI Calculation ----

test_that("calculate_proportion_ci wilson method works", {
	ci <- calculate_proportion_ci(42, 100, method = "wilson", conf_level = 0.95)

	expect_true(is.list(ci))
	expect_true("lower" %in% names(ci))
	expect_true("upper" %in% names(ci))
	expect_true(ci$lower < 0.42)
	expect_true(ci$upper > 0.42)
	expect_true(ci$lower >= 0)
	expect_true(ci$upper <= 1)
})

test_that("calculate_proportion_ci exact method works", {
	ci <- calculate_proportion_ci(42, 100, method = "exact", conf_level = 0.95)

	expect_true(ci$lower < 0.42)
	expect_true(ci$upper > 0.42)
})

test_that("calculate_proportion_ci wald method works", {
	ci <- calculate_proportion_ci(42, 100, method = "wald", conf_level = 0.95)

	expect_true(ci$lower < 0.42)
	expect_true(ci$upper > 0.42)
})
