# Extracted from test-imputation.R:78

# test -------------------------------------------------------------------------
it("returns an ImputationResult object", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5),
		y = c(10, NA, 30, 40, 50),
		z = c("A", "B", "A", "B", NA)
	)

	result <- perform_multiple_imputation(data, m = 3, maxit = 2)

	expect_true(S7::S7_inherits(result, ImputationResult))
})
it("validates that data is a data frame", {
	skip_if_not_installed("mice")

	expect_error(
		perform_multiple_imputation("not_a_dataframe"),
		"data.*data frame"
	)
})
it("validates that m is a positive integer", {
	skip_if_not_installed("mice")

	data <- data.frame(x = c(1, NA, 3))

	expect_error(
		perform_multiple_imputation(data, m = 0),
		"m.*positive"
	)

	expect_error(
		perform_multiple_imputation(data, m = -5),
		"m.*positive"
	)
})
it("supports different imputation methods", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5),
		y = c(10, NA, 30, 40, 50)
	)

	result <- perform_multiple_imputation(
		data,
		m = 3,
		maxit = 2,
		method = "pmm"
	)

	expect_true(S7::S7_inherits(result, ImputationResult))
	expect_equal(result@method, "pmm")
})
it("stores the mice object for diagnostics", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5),
		y = c(10, NA, 30, 40, 50)
	)

	result <- perform_multiple_imputation(data, m = 3, maxit = 2)

	expect_true("mids" %in% class(result@mice_object))
})
