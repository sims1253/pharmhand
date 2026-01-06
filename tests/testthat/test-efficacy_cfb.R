# Tests for R/efficacy_cfb.R
library(testthat)
library(pharmhand)

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

	tbl <- create_cfb_summary_table(advs, params = "SYSBP")

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
		visits = c("Baseline", "Week 2")
	)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "vs_by_visit")
	expect_equal(nrow(tbl@data), 2)
})

test_that("create_cfb_summary_table validates inputs", {
	expect_error(
		create_cfb_summary_table(NULL, params = "SYSBP"),
		"must be a data frame"
	)

	advs_missing <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		PARAM = c("SYSBP", "SYSBP")
	)
	expect_error(
		create_cfb_summary_table(advs_missing, params = "SYSBP"),
		"missing required columns"
	)
})

test_that("create_vs_by_visit_table validates inputs", {
	expect_error(
		create_vs_by_visit_table(NULL),
		"must be a data frame"
	)
	expect_error(
		create_vs_by_visit_table(data.frame()),
		"missing required columns"
	)
})
