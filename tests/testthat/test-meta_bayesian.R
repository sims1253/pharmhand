# Tests for Bayesian meta-analysis functions (R/meta_bayesian.R)

library(pharmhand)

# =============================================================================
# bayesian_meta_analysis validation tests
# =============================================================================

test_that("bayesian_meta_analysis validates numeric yi", {
	sei <- c(0.12, 0.15, 0.18)

	expect_error(
		bayesian_meta_analysis(yi = character(3), sei = sei, effect_measure = "hr"),
		"'yi' must be numeric"
	)
})

test_that("bayesian_meta_analysis validates numeric sei", {
	yi <- log(c(0.75, 0.82, 0.68))

	expect_error(
		bayesian_meta_analysis(yi = yi, sei = character(3), effect_measure = "hr"),
		"'sei' must be numeric"
	)
})

test_that("bayesian_meta_analysis validates yi and sei length match", {
	yi <- log(c(0.75, 0.82))
	sei <- c(0.12, 0.15, 0.18)

	expect_error(
		bayesian_meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
		"same length"
	)
})

test_that("bayesian_meta_analysis validates positive sei values", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, -0.15, 0.18)

	expect_error(
		bayesian_meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
		"positive"
	)

	sei <- c(0.12, 0, 0.18)

	expect_error(
		bayesian_meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
		"positive"
	)
})

test_that("bayesian_meta_analysis validates study_labels length", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)
	study_labels <- c("Study 1", "Study 2") # Wrong length

	expect_error(
		bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			effect_measure = "hr"
		),
		"must have length"
	)
})

# =============================================================================
# bayesian_meta_analysis with brms backend tests
# =============================================================================

test_that("bayesian_meta_analysis with brms returns bayesian_meta_result", {
	skip_if_not_installed("brms")

	# Use 8 studies with consistent effect sizes for stable MCMC
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2, # Use fewer chains for faster tests
		cores = 2,
		iter = 2000, # Use more iterations for better ESS
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Should return bayesian_meta_result class
	expect_true("bayesian_meta_result" %in% class(result))

	# Check expected structure
	expect_true("posterior_mean" %in% names(result))
	expect_true("posterior_median" %in% names(result))
	expect_true("ci_95" %in% names(result))
	expect_true("ci_80" %in% names(result))
	expect_true("tau_mean" %in% names(result))
	expect_true("tau_ci_95" %in% names(result))
	expect_true("prob_positive" %in% names(result))
	expect_true("prob_negative" %in% names(result))
	expect_true("n_studies" %in% names(result))
	expect_true("effect_measure" %in% names(result))
	expect_true("model_type" %in% names(result))
	expect_true("interpretation" %in% names(result))
})

test_that("bayesian_meta_analysis with brms stores fit object", {
	skip_if_not_installed("brms")

	# Use 8 studies with consistent effect sizes for stable MCMC
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 2,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Should contain the brms fit object
	expect_true("fit" %in% names(result))
	expect_true("brmsfit" %in% class(result$fit))
})

test_that("bayesian_meta_analysis with brms respects custom priors", {
	skip_if_not_installed("brms")

	# Use 8 studies with consistent effect sizes for stable MCMC
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		prior_mu = list(mean = -0.5, sd = 2),
		prior_tau = list(type = "half_cauchy", scale = 0.3),
		chains = 2,
		cores = 2,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Should return bayesian_meta_result
	expect_true("bayesian_meta_result" %in% class(result))
	expect_true("posterior_mean" %in% names(result))
})

test_that("bayesian_meta_analysis with brms generates correct study labels", {
	skip_if_not_installed("brms")

	# Use 8 studies with consistent effect sizes for stable MCMC
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)
	custom_labels <- c(
		"Trial A",
		"Trial B",
		"Trial C",
		"Trial D",
		"Trial E",
		"Trial F",
		"Trial G",
		"Trial H"
	)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		study_labels = custom_labels,
		effect_measure = "hr",
		chains = 2,
		cores = 2,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Should use custom labels
	expect_true("bayesian_meta_result" %in% class(result))
})
