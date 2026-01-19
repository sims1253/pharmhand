# Tests for MMRM functions (R/mmrm.R)
# Issue #176: MMRM for longitudinal PRO data

# =============================================================================
# mmrm_analysis tests
# =============================================================================

describe("mmrm_analysis", {
	it("returns an MMRMResult object", {
		skip_if_not_installed("mmrm")

		data <- create_mmrm_test_data(n_subjects = 30, seed = 123)
		result <- mmrm_analysis(
			data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN",
			covariates = c("BASE")
		)

		expect_true(S7::S7_inherits(result, MMRMResult))
	})

	it("validates input data", {
		skip_if_not_installed("mmrm")

		expect_error(
			mmrm_analysis("not_dataframe"),
			"data frame"
		)
	})

	it("validates required variables exist", {
		skip_if_not_installed("mmrm")

		data <- create_mmrm_test_data()

		expect_error(
			mmrm_analysis(
				data,
				response_var = "MISSING_VAR",
				subject_var = "USUBJID",
				trt_var = "TRT01P",
				time_var = "AVISITN"
			),
			"MISSING_VAR"
		)
	})

	it("supports different covariance structures", {
		skip_if_not_installed("mmrm")

		data <- create_mmrm_test_data(n_subjects = 30, seed = 456)

		# Compound symmetry
		result_cs <- mmrm_analysis(
			data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN",
			cov_covariance = "cs"
		)

		expect_true(S7::S7_inherits(result_cs, MMRMResult))

		# Unstructured
		result_un <- mmrm_analysis(
			data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN",
			cov_covariance = "us"
		)

		expect_true(S7::S7_inherits(result_un, MMRMResult))
	})

	it("handles treatment by time interaction", {
		skip_if_not_installed("mmrm")

		data <- create_mmrm_test_data(n_subjects = 30, seed = 789)
		result <- mmrm_analysis(
			data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN",
			interaction = TRUE
		)

		expect_true(S7::S7_inherits(result, MMRMResult))
		# Should have interaction terms in coefficients
		coef_names <- names(result@coefficients)
		expect_true(any(grepl(":", coef_names, fixed = TRUE)))
	})

	it("applies Kenward-Roger adjustment", {
		skip_if_not_installed("mmrm")

		data <- create_mmrm_test_data(n_subjects = 30, seed = 111)
		result <- mmrm_analysis(
			data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN",
			df_adjustment = "Kenward-Roger"
		)

		expect_true(S7::S7_inherits(result, MMRMResult))
	})

	it("handles missing data appropriately", {
		skip_if_not_installed("mmrm")

		# Create data with missing values
		set.seed(222)
		n <- 30
		base_vals <- rnorm(n, mean = 50, sd = 10)
		data <- data.frame(
			USUBJID = rep(1:n, each = 4),
			TRT01P = rep(sample(c("A", "B"), n, replace = TRUE), each = 4),
			AVISITN = rep(0:3, n),
			BASE = rep(base_vals, each = 4), # ADD THIS LINE
			AVAL = c(base_vals, base_vals + 5, base_vals + 8, base_vals + 10) +
				rnorm(n * 4, 0, 3)
		)

		# Add missing values
		data$AVAL[sample.int(nrow(data), 10)] <- NA

		result <- mmrm_analysis(
			data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN",
			covariates = "BASE"
		)

		expect_true(S7::S7_inherits(result, MMRMResult))
	})
})

# =============================================================================
# MMRMResult class tests
# =============================================================================

describe("MMRMResult class", {
	it("has expected properties", {
		skip_if_not_installed("mmrm")

		data <- create_mmrm_test_data(n_subjects = 30, seed = 333)
		result <- mmrm_analysis(
			data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN"
		)

		# Check all expected properties exist
		expect_true("model" %in% names(S7::props(result)))
		expect_true("coefficients" %in% names(S7::props(result)))
		expect_true("ci" %in% names(S7::props(result)))
		expect_true("p_values" %in% names(S7::props(result)))
		expect_true("df" %in% names(S7::props(result)))
		expect_true("sigma" %in% names(S7::props(result)))
		expect_true("log_likelihood" %in% names(S7::props(result)))
		expect_true("aic" %in% names(S7::props(result)))
		expect_true("bic" %in% names(S7::props(result)))
	})
})
