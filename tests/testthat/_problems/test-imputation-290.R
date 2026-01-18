# Extracted from test-imputation.R:290

# test -------------------------------------------------------------------------
it("returns a list of completed datasets", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5),
		y = c(10, NA, 30, 40, 50)
	)

	imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)
	completed <- get_complete_data(imp_result)

	expect_true(is.list(completed))
	expect_equal(length(completed), 3)
	expect_true(all(vapply(completed, is.data.frame, logical(1))))
})
it("returns a single dataset when action = 'long'", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5),
		y = c(10, NA, 30, 40, 50)
	)

	imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)
	completed <- get_complete_data(imp_result, action = "long")

	expect_true(is.data.frame(completed))
	expect_true(".imp" %in% names(completed))
})
