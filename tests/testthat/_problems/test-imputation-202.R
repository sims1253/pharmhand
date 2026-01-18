# Extracted from test-imputation.R:202

# test -------------------------------------------------------------------------
it("applies analysis function to imputed datasets and pools results", {
	skip_if_not_installed("mice")

	imputation_data <- create_imputation_test_data(n = 30, seed = 123)
	imp_result <- perform_multiple_imputation(imputation_data, m = 3, maxit = 2)

	# Simple analysis: mean of x
	result <- analyze_with_imputation(
		imp_result,
		analysis_fun = function(data) {
			list(
				estimate = mean(data$x),
				variance = var(data$x) / nrow(data)
			)
		}
	)

	expect_true(is.list(result))
	expect_true("pooled_estimate" %in% names(result))
})
