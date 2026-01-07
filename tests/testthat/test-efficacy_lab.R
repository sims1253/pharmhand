# Tests for R/efficacy_lab.R
library(testthat)
library(pharmhand)

test_that("create_lab_summary_table works", {
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

	tbl <- create_lab_shift_table(adlb)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "lab_shift")
	expect_true("Baseline Status" %in% names(tbl@data))
})

test_that("create_lab_shift_table validates inputs", {
	expect_error(
		create_lab_shift_table(NULL),
		"must be a data frame"
	)
})

test_that("create_lab_summary_table validates inputs", {
	expect_error(
		create_lab_summary_table(NULL),
		"must be a data frame"
	)
	expect_error(
		create_lab_summary_table(data.frame()),
		"missing required columns"
	)
})
