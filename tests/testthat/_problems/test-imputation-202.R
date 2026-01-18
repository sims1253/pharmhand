# Extracted from test-imputation.R:202

# test -------------------------------------------------------------------------
it("applies analysis function to imputed datasets and pools results", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5, 6, 7, NA, 9, 10),
		y = c(2, 4, 6, 8, 10, NA, 14, 16, NA, 20)
	)

	imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)

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
