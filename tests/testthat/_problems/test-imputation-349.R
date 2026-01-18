# Extracted from test-imputation.R:349

# test -------------------------------------------------------------------------
it("has expected properties", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5),
		y = c(10, NA, 30, 40, 50)
	)

	result <- perform_multiple_imputation(data, m = 3, maxit = 2)

	# Check all expected properties exist
	expect_true("mice_object" %in% names(S7::props(result)))
	expect_true("m" %in% names(S7::props(result)))
	expect_true("method" %in% names(S7::props(result)))
	expect_true("imputed_vars" %in% names(S7::props(result)))
	expect_true("n_missing" %in% names(S7::props(result)))
	expect_true("original_data" %in% names(S7::props(result)))
})
it("stores missing data summary", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5), # 1 missing
		y = c(10, NA, 30, NA, 50) # 2 missing
	)

	result <- perform_multiple_imputation(data, m = 3, maxit = 2)

	expect_true(is.list(result@n_missing))
	expect_equal(result@n_missing$x, 1L)
	expect_equal(result@n_missing$y, 2L)
})
