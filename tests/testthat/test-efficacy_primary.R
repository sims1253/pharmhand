# Tests for R/efficacy_primary.R
library(testthat)
library(pharmhand)

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

test_that("create_primary_endpoint_table validates inputs", {
	advs <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		AVAL = c(120, 130)
	)
	trt_n <- data.frame(TRT01P = c("A", "B"), N = c(1, 1))

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
	expect_error(
		create_primary_endpoint_table(advs, NULL),
		"must be a data frame"
	)

	advs_missing <- advs[, setdiff(names(advs), "AVAL")]
	expect_error(
		create_primary_endpoint_table(advs_missing, trt_n),
		"missing required columns"
	)
})
