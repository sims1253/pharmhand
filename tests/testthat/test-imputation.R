# Tests for multiple imputation functions (R/imputation.R)
# Issue #169: Multiple imputation with mice + Rubin pooling

# =============================================================================
# perform_multiple_imputation tests
# =============================================================================

describe("perform_multiple_imputation", {
	it("returns an ImputationResult object", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 123)
		result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)

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

		data <- create_imputation_test_data()

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

		data <- create_imputation_test_data(n = 30, seed = 456)

		result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2,
			method = "pmm"
		)

		expect_true(S7::S7_inherits(result, ImputationResult))
		# Method should contain pmm for numeric vars
		expect_true(any(grepl("pmm", result@method, fixed = TRUE)))
	})

	it("stores the mice object for diagnostics", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 789)
		result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)

		expect_true("mids" %in% class(result@mice_object))
	})

	it("returns correct number of imputations", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 111)
		result <- perform_multiple_imputation(
			data,
			m = 5,
			maxit = 2
		)

		expect_equal(result@m, 5L)
	})

	it("records variables with missing data", {
		skip_if_not_installed("mice")

		set.seed(222)
		n <- 30
		data <- data.frame(
			x = c(rnorm(n - 3), rep(NA, 3)),
			y = c(rep(NA, 5), rnorm(n - 5)),
			z = rnorm(n) # No missing
		)

		result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)

		expect_true("x" %in% result@imputed_vars)
		expect_true("y" %in% result@imputed_vars)
		expect_false("z" %in% result@imputed_vars)
	})

	it("warns when data has no missing values", {
		skip_if_not_installed("mice")

		set.seed(333)
		data <- data.frame(
			x = rnorm(20),
			y = rnorm(20)
		)

		expect_warning(
			perform_multiple_imputation(data, m = 3),
			"no missing"
		)
	})
})

# =============================================================================
# pool_rubin tests
# =============================================================================

describe("pool_rubin", {
	it("pools estimates using Rubin's rules", {
		# Create mock imputation result with known values
		estimates <- c(0.5, 0.6, 0.4, 0.55, 0.45)
		variances <- c(0.01, 0.012, 0.011, 0.009, 0.013)

		result <- pool_rubin(estimates, variances)

		expect_true(is.list(result))
		expect_true("pooled_estimate" %in% names(result))
		expect_true("pooled_se" %in% names(result))
		expect_true("ci" %in% names(result))
		expect_true("within_var" %in% names(result))
		expect_true("between_var" %in% names(result))
		expect_true("total_var" %in% names(result))
		expect_true("fmi" %in% names(result))
		expect_true("df" %in% names(result))
	})

	it("validates that estimates and variances have same length", {
		expect_error(
			pool_rubin(c(0.5, 0.6), c(0.01)),
			"same length"
		)
	})

	it("validates that variances are non-negative", {
		expect_error(
			pool_rubin(c(0.5, 0.6), c(0.01, -0.02)),
			"non-negative"
		)
	})

	it("returns correct pooled estimate (mean of estimates)", {
		estimates <- c(0.5, 0.6, 0.4, 0.55, 0.45)
		variances <- c(0.01, 0.012, 0.011, 0.009, 0.013)

		result <- pool_rubin(estimates, variances)

		expect_equal(result$pooled_estimate, mean(estimates))
	})

	it("calculates fraction of missing information", {
		estimates <- c(0.5, 0.6, 0.4, 0.55, 0.45)
		variances <- c(0.01, 0.012, 0.011, 0.009, 0.013)

		result <- pool_rubin(estimates, variances)

		# FMI should be between 0 and 1
		expect_true(result$fmi >= 0 && result$fmi <= 1)
	})

	it("handles zero within-imputation variance stably", {
		estimates <- c(0.5, 0.6)
		variances <- c(0, 0)
		result <- pool_rubin(estimates, variances)
		expect_equal(result$within_var, 0)
		expect_true(is.infinite(result$total_var) || result$total_var > 0)
		# total_var = 0 + (1+1/2)*var(estimates)
		expect_equal(result$fmi, 1) # r is Inf
	})

	it("handles zero between-imputation variance stably", {
		estimates <- c(0.5, 0.5)
		variances <- c(0.01, 0.01)
		result <- pool_rubin(estimates, variances)
		expect_equal(result$between_var, 0)
		expect_equal(result$fmi, 0) # r is 0
	})
})

# =============================================================================
# analyze_with_imputation tests
# =============================================================================

describe("analyze_with_imputation", {
	it("applies analysis function to imputed datasets and pools results", {
		skip_if_not_installed("mice")

		set.seed(444)
		n <- 40
		data <- data.frame(
			x = c(rnorm(n - 5, mean = 10, sd = 2), rep(NA, 5)),
			y = c(rep(NA, 4), rnorm(n - 4, mean = 20, sd = 4))
		)

		imp_result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)

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

		set.seed(555)
		n <- 50
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

		imp_result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 3
		)

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

	it("is resilient to partial analysis failures", {
		skip_if_not_installed("mice")
		data <- create_imputation_test_data(n = 30)
		imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)

		# Use a counter to ensure controlled failure
		counter <- 0
		analysis_fun_fail <- function(d) {
			counter <<- counter + 1
			if (counter == 1) {
				stop("First one fails")
			}
			list(
				estimate = mean(d$x, na.rm = TRUE),
				variance = var(d$x, na.rm = TRUE) / nrow(d)
			)
		}

		expect_warning(
			result <- analyze_with_imputation(imp_result, analysis_fun_fail),
			"Analysis function failed"
		)
		expect_true("pooled_estimate" %in% names(result))
		expect_equal(result$m, 2) # Only 2 pooled
	})

	it("aborts if all analysis functions fail", {
		skip_if_not_installed("mice")
		data <- create_imputation_test_data(n = 30)
		imp_result <- perform_multiple_imputation(data, m = 3, maxit = 2)
		suppressWarnings(
			expect_error(
				analyze_with_imputation(imp_result, function(d) stop("Failure")),
				"Analysis function failed for all imputed datasets"
			)
		)
	})
})

# =============================================================================
# get_complete_data tests
# =============================================================================

describe("get_complete_data", {
	it("returns a list of completed datasets", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 666)
		imp_result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)
		completed <- get_complete_data(imp_result)

		expect_true(is.list(completed))
		expect_equal(length(completed), 3)
		expect_true(all(vapply(completed, is.data.frame, logical(1))))
	})

	it("returns a single dataset when action = 'long'", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 777)
		imp_result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)
		completed <- get_complete_data(imp_result, action = "long")

		expect_true(is.data.frame(completed))
		expect_true(".imp" %in% names(completed))
	})

	it("completed datasets have no missing values", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 888)
		imp_result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)
		completed <- get_complete_data(imp_result)

		for (df in completed) {
			expect_false(anyNA(df))
		}
	})
})

# =============================================================================
# ImputationResult class tests
# =============================================================================

describe("ImputationResult class", {
	it("has expected properties", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 999)
		result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)

		expected_props <- c(
			"mice_object",
			"m",
			"method",
			"imputed_vars",
			"n_missing",
			"original_data",
			"metadata"
		)
		expect_setequal(names(S7::props(result)), expected_props)
	})

	it("stores missing data summary", {
		skip_if_not_installed("mice")

		set.seed(1010)
		n <- 30
		data <- data.frame(
			x = c(rnorm(n - 1), NA), # 1 missing
			y = c(rep(NA, 2), rnorm(n - 2)), # 2 missing
			z = rnorm(n) # No missing values
		)

		result <- perform_multiple_imputation(
			data,
			m = 3,
			maxit = 2
		)

		expect_true(is.list(result@n_missing))
		expect_equal(result@n_missing$x, 1L)
		expect_equal(result@n_missing$y, 2L)
		expect_equal(result@n_missing$z, 0L)
	})
})

# =============================================================================
# summarize_missing tests
# =============================================================================

describe("summarize_missing", {
	it("returns summary of missing data patterns", {
		data <- data.frame(
			x = c(1, 2, NA, 4, 5),
			y = c(10, NA, 30, NA, 50),
			z = c(1, 2, 3, 4, 5)
		)

		summary <- summarize_missing(data)

		expect_true(is.data.frame(summary))
		expect_true("variable" %in% names(summary))
		expect_true("n_missing" %in% names(summary))
		expect_true("pct_missing" %in% names(summary))
	})

	it("calculates correct percentages", {
		data <- data.frame(
			x = c(1, 2, NA, 4, NA), # 2/5 = 40% missing
			y = c(NA, NA, NA, NA, NA), # 100% missing
			z = c(1, 2, 3, 4, 5) # 0% missing
		)

		summary <- summarize_missing(data)

		x_row <- summary[summary$variable == "x", ]
		y_row <- summary[summary$variable == "y", ]
		z_row <- summary[summary$variable == "z", ]

		expect_equal(x_row$pct_missing, 40)
		expect_equal(y_row$pct_missing, 100)
		expect_equal(z_row$pct_missing, 0)
	})
})
