# Tests for R/efficacy_lab.R

test_that("create_lab_summary_table returns ClinicalTable", {
	adlb <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAM = c("Alanine Aminotransferase", "Alanine Aminotransferase"),
		PARAMCD = c("ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24"),
		AVAL = c(20, 30)
	)

	tbl <- create_lab_summary_table(adlb, params = "ALT")

	expect_s7_class(tbl, ClinicalTable)
})

test_that("create_lab_summary_table has correct type", {
	adlb <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAM = c("Alanine Aminotransferase", "Alanine Aminotransferase"),
		PARAMCD = c("ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24"),
		AVAL = c(20, 30)
	)

	tbl <- create_lab_summary_table(adlb, params = "ALT")

	expect_equal(tbl@type, "lab_summary")
})

test_that("create_lab_shift_table returns ClinicalTable", {
	adlb <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		PARAMCD = c("ALT", "ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24", "Week 24"),
		BNRIND = c("NORMAL", "NORMAL", "HIGH"),
		ANRIND = c("NORMAL", "HIGH", "HIGH")
	)

	tbl <- create_lab_shift_table(adlb)

	expect_s7_class(tbl, ClinicalTable)
})

test_that("create_lab_shift_table has correct type", {
	adlb <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		PARAMCD = c("ALT", "ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24", "Week 24"),
		BNRIND = c("NORMAL", "NORMAL", "HIGH"),
		ANRIND = c("NORMAL", "HIGH", "HIGH")
	)

	tbl <- create_lab_shift_table(adlb)

	expect_equal(tbl@type, "lab_shift")
})

test_that("create_lab_shift_table has expected columns", {
	adlb <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		PARAMCD = c("ALT", "ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24", "Week 24"),
		BNRIND = c("NORMAL", "NORMAL", "HIGH"),
		ANRIND = c("NORMAL", "HIGH", "HIGH")
	)

	tbl <- create_lab_shift_table(adlb)

	expect_true("Baseline Status" %in% names(tbl@data))
})

test_that("create_lab_shift_table errors on NULL input", {
	expect_error(
		create_lab_shift_table(NULL),
		"must be a data frame"
	)
})

test_that("create_lab_summary_table errors on NULL input", {
	expect_error(
		create_lab_summary_table(NULL),
		"must be a data frame"
	)
})

test_that("create_lab_summary_table errors when required columns missing", {
	expect_error(
		create_lab_summary_table(data.frame()),
		"missing required columns"
	)
})
