# Extracted from test-imputation.R:306

describe("get_complete_data", {
	it("returns a list of completed datasets", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 123)
		imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)
		completed <- get_complete_data(imp_result)

		expect_true(is.list(completed))
		expect_equal(length(completed), 3)
		expect_true(all(vapply(completed, is.data.frame, logical(1))))
	})

	it("returns a single dataset when action = 'long'", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 123)
		imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)
		completed <- get_complete_data(imp_result, action = "long")

		expect_true(is.data.frame(completed))
		expect_true(".imp" %in% names(completed))
	})

	it("completed datasets have no missing values", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 123)
		imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)
		completed <- get_complete_data(imp_result)

		for (df in completed) {
			expect_false(anyNA(df))
		}
	})
})
