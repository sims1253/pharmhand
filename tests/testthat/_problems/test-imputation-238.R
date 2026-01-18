# Extracted from test-imputation.R:238

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
it("validates ImputationResult input", {
	skip_if_not_installed("mice")

	expect_error(
		analyze_with_imputation("not_imputation_result", function(x) x),
		"ImputationResult"
	)
})
it("works with regression models", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		outcome = c(10, 20, NA, 40, 50, 60, NA, 80, 90, 100),
		predictor = c(1, 2, 3, 4, NA, 6, 7, 8, 9, 10),
		group = rep(c("A", "B"), 5)
	)

	imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)

	# Linear regression analysis
	result <- analyze_with_imputation(
		imp_result,
		analysis_fun = function(data) {
			fit <- lm(outcome ~ predictor + group, data = data)
			coefs <- summary(fit)$coefficients
			# Return coefficient for predictor
			list(
				estimate = coefs["predictor", "Estimate"],
				variance = coefs["predictor", "Std. Error"]^2
			)
		}
	)

	expect_true("pooled_estimate" %in% names(result))
	expect_true("pooled_se" %in% names(result))
})
