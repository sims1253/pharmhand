# Extracted from test-imputation.R:238

# test -------------------------------------------------------------------------
it("applies analysis function to imputed datasets and pools results", {
	skip_if_not_installed("mice")

	set.seed(123)
	n <- 30
	data <- data.frame(
		x = c(rnorm(n - 3), rep(NA, 3)),
		y = c(rep(NA, 4), rnorm(n - 4))
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
	n <- 40
	predictor <- rnorm(n, mean = 5, sd = 2)
	outcome <- 10 + 2 * predictor + rnorm(n, sd = 3)
	# Add some missing values
	outcome[sample.int(n, 5)] <- NA
	predictor[sample.int(n, 3)] <- NA

	data <- data.frame(
		outcome = outcome,
		predictor = predictor,
		group = factor(rep(c("A", "B"), length.out = n))
	)

	imp_result <- perform_multiple_imputation(data, m = 3, maxit = 3)

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
