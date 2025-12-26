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

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
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

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
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

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
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

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
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

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
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

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
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
