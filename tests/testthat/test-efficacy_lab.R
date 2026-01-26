# Tests for R/efficacy_lab.R

test_that("create_lab_summary_table returns ClinicalTable", {
	adlb <- fixture_adlb_lab_summary()

	tbl <- create_lab_summary_table(adlb, params = "ALT")

	expect_s7_class(tbl, ClinicalTable)
})

test_that("create_lab_summary_table has correct type", {
	adlb <- fixture_adlb_lab_summary()

	tbl <- create_lab_summary_table(adlb, params = "ALT")

	expect_equal(tbl@type, "lab_summary")
})

test_that("create_lab_shift_table returns ClinicalTable", {
	adlb <- fixture_adlb_lab_shift()

	tbl <- create_lab_shift_table(adlb)

	expect_s7_class(tbl, ClinicalTable)
})

test_that("create_lab_shift_table has correct type", {
	adlb <- fixture_adlb_lab_shift()

	tbl <- create_lab_shift_table(adlb)

	expect_equal(tbl@type, "lab_shift")
})

test_that("create_lab_shift_table has expected columns", {
	adlb <- fixture_adlb_lab_shift()

	tbl <- create_lab_shift_table(adlb)

	expect_true("Baseline Status" %in% names(tbl@data))
})

test_that("create_lab_shift_table errors on NULL input", {
	expect_error(
		create_lab_shift_table(NULL),
		"must be an ADaMData object or data.frame"
	)
})

test_that("create_lab_summary_table errors on NULL input", {
	expect_error(
		create_lab_summary_table(NULL),
		"must be an ADaMData object or data.frame"
	)
})

test_that("create_lab_summary_table errors when required columns missing", {
	expect_error(
		create_lab_summary_table(data.frame()),
		"'data' is missing required columns"
	)
})
