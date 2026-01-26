# Helper for caching brms compiled models in tests
# This dramatically speeds up tests by reusing compiled Stan code

# Cache directory for compiled models
.brms_cache_dir <- function() {
	cache_dir <- file.path(tempdir(), "pharmhand_brms_cache")
	if (!dir.exists(cache_dir)) {
		dir.create(cache_dir, recursive = TRUE)
	}
	cache_dir
}

options(mc.cores = 1)

# Capture all warnings raised by an expression, while muffling them.
# Useful for tests that need to assert warnings without producing WARN output.
capture_muffled_warnings <- function(expr) {
	warnings <- character()
	value <- withCallingHandlers(
		force(expr),
		warning = function(w) {
			warnings <<- c(warnings, conditionMessage(w))
			invokeRestart("muffleWarning")
		}
	)
	list(value = value, warnings = warnings)
}

#' Skip tests if brms is not available or functional
#'
#' Checks if brms is installed and can actually be loaded.
#' This is necessary because brms depends on Stan which requires proper
#' toolchains.
skip_if_brms_unavailable <- function() {
	skip_if_not_installed("brms")

	# Check if brms/Stan backend can actually work
	tryCatch(
		{
			if (!requireNamespace("brms", quietly = TRUE)) {
				skip("brms not available")
			}
		},
		error = function(e) {
			skip(paste("brms not functional:", conditionMessage(e)))
		}
	)
}

# ==============================================================================
# Shared Test Fixtures - Reuse models across tests
# ==============================================================================

# Standard test data used across multiple tests
.test_yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
.test_sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

# Cached shared result for tests that don't need specific configurations
.shared_bayesian_result <- NULL
.shared_bayesian_result_smd <- NULL

#' Get shared Bayesian result for HR (fit once, reuse everywhere)
#'
#' Returns a cached bayesian_meta_result that is computed once per test session.
#' This dramatically speeds up tests by avoiding redundant MCMC sampling.
#'
#' @return A bayesian_meta_result object
get_shared_bayesian_result <- function() {
	if (is.null(.shared_bayesian_result)) {
		message("Fitting shared Bayesian model (HR, will be cached)...")
		.shared_bayesian_result <<- bayesian_meta_analysis(
			yi = .test_yi,
			sei = .test_sei,
			effect_measure = "hr",
			backend = "cmdstanr",
			warn_convergence = "never",
			posterior_predictive = FALSE,
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 42,
			adapt_delta = 0.99
		)
	}
	.shared_bayesian_result
}

#' Get shared Bayesian result for SMD (fit once, reuse everywhere)
#'
#' Returns a cached bayesian_meta_result for SMD effect measure.
#'
#' @return A bayesian_meta_result object
get_shared_bayesian_result_smd <- function() {
	if (is.null(.shared_bayesian_result_smd)) {
		message("Fitting shared Bayesian model (SMD, will be cached)...")
		.shared_bayesian_result_smd <<- bayesian_meta_analysis(
			yi = .test_yi,
			sei = .test_sei,
			effect_measure = "smd",
			backend = "cmdstanr",
			warn_convergence = "never",
			posterior_predictive = FALSE,
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 43,
			adapt_delta = 0.99
		)
	}
	.shared_bayesian_result_smd
}

# Get a cached brms fit or create one
# Uses file-based caching so compiled Stan code is reused
get_cached_bayesian_result <- function(
	yi = log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81)),
	sei = c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12),
	effect_measure = "hr",
	seed = 42
) {
	# Create cache key from all parameters that affect results
	cache_key <- digest::digest(list(yi, sei, effect_measure, seed))
	cache_file <- file.path(
		.brms_cache_dir(),
		paste0("brms_meta_", cache_key, ".rds")
	)

	# Return cached result if exists
	if (file.exists(cache_file)) {
		return(readRDS(cache_file))
	}

	# Fit and cache
	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = effect_measure,
		backend = "cmdstanr",
		warn_convergence = "never",
		posterior_predictive = FALSE,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = seed,
		adapt_delta = 0.95
	)

	saveRDS(result, cache_file)
	result
}

# ==============================================================================
# Shared Bayesian Meta-Analysis for Few Studies Result
# ==============================================================================

# Cached shared result for Bayesian meta-analysis for few studies
.shared_bayesian_few_result <- NULL

#' Get shared Bayesian meta-analysis for few studies result
#'
#' Returns a cached BayesianMetaFewResult that is computed once per test
#' session.
#' This dramatically speeds up tests by avoiding redundant MCMC sampling.
#'
#' @return A BayesianMetaFewResult object
get_shared_bayesian_few_result <- function() {
	if (is.null(.shared_bayesian_few_result)) {
		message(
			"Fitting shared Bayesian meta-analysis for few studies model ",
			"(will be cached)..."
		)
		.shared_bayesian_few_result <<- bayesian_meta_analysis_few(
			yi = log(c(0.75, 0.82, 0.68)),
			sei = c(0.12, 0.15, 0.18),
			effect_measure = "hr",
			prior_sensitivity = FALSE,
			chains = 1,
			iter = 400,
			warmup = 200,
			seed = 42
		)
	}
	.shared_bayesian_few_result
}

# ==============================================================================
# Shared MMRM Result
# ==============================================================================

# Cached shared result for MMRM analysis
.shared_mmrm_result <- NULL

#' Get shared MMRM result
#'
#' Returns a cached MMRMResult that is computed once per test session.
#' This speeds up tests by avoiding redundant model fitting.
#'
#' @return An MMRMResult object
get_shared_mmrm_result <- function() {
	skip_if_not_installed("mmrm")

	if (is.null(.shared_mmrm_result)) {
		message("Fitting shared MMRM model (will be cached)...")
		data <- create_mmrm_test_data(n_subjects = 30, seed = 123)
		.shared_mmrm_result <<- mmrm_analysis(
			data = data,
			response_var = "AVAL",
			subject_var = "USUBJID",
			trt_var = "TRT01P",
			time_var = "AVISITN",
			covariates = NULL
		)
	}
	.shared_mmrm_result
}
