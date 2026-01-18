# Extracted from test-imputation.R:349

describe("imputation result properties", {
	it("has expected properties", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 123)
		result <- perform_multiple_imputation(data, m = 3, maxit = 2)

		# Check all expected properties exist
		expected_props <- c(
			"mice_object",
			"m",
			"method",
			"imputed_vars",
			"n_missing",
			"original_data"
		)
		expect_true(all(expected_props %in% names(S7::props(result))))
	})

	it("stores missing data summary", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 123)
		result <- perform_multiple_imputation(data, m = 3, maxit = 2)

		expect_true(is.list(result@n_missing))
	})
})
