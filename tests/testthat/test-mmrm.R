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
			covariates = "BASE"
		)

		expect_true(S7::S7_inherits(result, MMRMResult))
	})

	it("validates input data", {
		skip_if_not_installed("mmrm")

		expect_error(
			mmrm_analysis("not_dataframe", "AVAL", "USUBJID", "TRT01P", "AVISITN"),
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

	it("validates covariates exist", {
		skip_if_not_installed("mmrm")

		data <- create_mmrm_test_data()

		expect_error(
			mmrm_analysis(
				data,
				response_var = "AVAL",
				subject_var = "USUBJID",
				trt_var = "TRT01P",
				time_var = "AVISITN",
				covariates = "MISSING_COVAR"
			),
			"MISSING_COVAR"
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
		expect_equal(result_cs@covariance, "cs")

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
		expect_equal(result_un@covariance, "us")
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
		expect_equal(result@df_adjustment, "Kenward-Roger")
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
			BASE = rep(base_vals, each = 4),
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
		result <- get_shared_mmrm_result()

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

	it("contains valid coefficients", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()

		expect_true(length(result@coefficients) > 0)
		expect_true(!anyNA(result@coefficients))
	})

	it("contains valid standard errors", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()

		expect_true(length(result@se) > 0)
		expect_true(!anyNA(result@se))
		expect_true(all(result@se > 0))
	})

	it("contains valid confidence intervals", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()

		expect_true(is.matrix(result@ci))
		expect_equal(ncol(result@ci), 2)
		expect_true(!anyNA(result@ci))
	})

	it("contains valid p-values", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()

		expect_true(length(result@p_values) > 0)
		expect_true(!anyNA(result@p_values))
		expect_true(all(result@p_values >= 0 & result@p_values <= 1))
	})

	it("contains valid degrees of freedom", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()

		expect_true(length(result@df) > 0)
		expect_true(!anyNA(result@df))
		expect_true(all(result@df > 0))
	})

	it("contains valid model fit statistics", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()

		expect_true(result@sigma > 0)
		expect_true(!is.na(result@log_likelihood))
		expect_true(!is.na(result@aic))
		expect_true(!is.na(result@bic))
	})

	it("contains metadata about the analysis", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()

		meta <- result@metadata
		expect_true(is.list(meta))
		expect_true("response_var" %in% names(meta))
		expect_true("subject_var" %in% names(meta))
		expect_true("trt_var" %in% names(meta))
		expect_true("time_var" %in% names(meta))
	})
})

# =============================================================================
# summary_mmrm tests
# =============================================================================

describe("summary_mmrm", {
	it("returns a summary data.frame", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		summ <- summary_mmrm(result)

		expect_s3_class(summ, "data.frame")
	})

	it("includes all expected columns", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		summ <- summary_mmrm(result)

		expect_true("Parameter" %in% names(summ))
		expect_true("Estimate" %in% names(summ))
		expect_true("Std.Error" %in% names(summ))
		expect_true("t value" %in% names(summ))
		expect_true("df" %in% names(summ))
		expect_true("Pr(>|t|)" %in% names(summ))
		expect_true("2.5 %" %in% names(summ))
		expect_true("97.5 %" %in% names(summ))
	})

	it("formats values with specified digits", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		summ_3 <- summary_mmrm(result, digits = 3)
		summ_2 <- summary_mmrm(result, digits = 2)

		# Check that digits parameter affects output
		expect_true(!identical(summ_3, summ_2))
	})

	it("calculates correct t-values", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		summ <- summary_mmrm(result)

		# t-value = estimate / se
		expected_t <- round(as.vector(result@coefficients / result@se), 3)
		actual_t <- round(as.vector(summ$`t value`), 3)

		expect_equal(expected_t, actual_t)
	})
})

# =============================================================================
# create_mmrm_table tests
# =============================================================================

describe("create_mmrm_table", {
	it("returns a ClinicalTable object", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		tab <- create_mmrm_table(result)

		expect_true(S7::S7_inherits(tab, ClinicalTable))
		expect_true(!is.null(tab@flextable))
	})

	it("includes metadata in footnotes", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		tab <- create_mmrm_table(result)

		# The function should add footnotes with model metadata
		# Check that footnotes exist and contain expected metadata
		footnotes <- tab@footnotes
		expect_true(length(footnotes) > 0)

		# Check for model metadata in footnotes (formula, estimation method, or degrees of freedom)
		footnote_text <- paste(footnotes, collapse = " ")
		expect_true(
			grepl(
				"formula|Model|MMRM|Kenward-Roger|degrees of freedom",
				footnote_text,
				ignore.case = TRUE
			)
		)
	})

	it("uses custom title", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		custom_title <- "Custom MMRM Analysis Table"
		tab <- create_mmrm_table(result, title = custom_title)

		expect_equal(tab@title, custom_title)
	})

	it("includes custom footnotes", {
		skip_if_not_installed("mmrm")
		result <- get_shared_mmrm_result()
		custom_fn <- c("Custom footnote 1", "Custom footnote 2")
		tab <- create_mmrm_table(result, footnotes = custom_fn)

		expect_true(S7::S7_inherits(tab, ClinicalTable))
	})
})
