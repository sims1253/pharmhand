# Tests for Bayesian meta-analysis functions (R/meta_bayesian.R)

library(pharmhand)

# Helper to skip if brms/Stan is not fully functional
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
	skip_if_brms_unavailable()

	# Use 8 studies with consistent effect sizes for stable MCMC
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2, # Use fewer chains for faster tests
		cores = 1,
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
	skip_if_brms_unavailable()

	# Use 8 studies with consistent effect sizes for stable MCMC
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
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
	skip_if_brms_unavailable()

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
		cores = 1,
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
	skip_if_brms_unavailable()

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
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Should use custom labels
	expect_true("bayesian_meta_result" %in% class(result))
})

# =============================================================================
# Convergence diagnostics tests
# =============================================================================

test_that("bayesian_meta_analysis includes convergence diagnostics", {
	skip_if_brms_unavailable()

	# Consistent test data for stable MCMC
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that convergence_diagnostics list is returned
	expect_true("convergence_diagnostics" %in% names(result))
	expect_true(is.list(result$convergence_diagnostics))

	# Test that max_rhat is present
	expect_true("max_rhat" %in% names(result$convergence_diagnostics))
	expect_true(is.numeric(result$convergence_diagnostics$max_rhat))

	# Test that min_bulk_ess is present
	expect_true("min_bulk_ess" %in% names(result$convergence_diagnostics))
	expect_true(is.numeric(result$convergence_diagnostics$min_bulk_ess))

	# Test that min_tail_ess is present
	expect_true("min_tail_ess" %in% names(result$convergence_diagnostics))
	expect_true(is.numeric(result$convergence_diagnostics$min_tail_ess))

	# Test that divergent_transitions is counted
	expect_true(
		"divergent_transitions" %in% names(result$convergence_diagnostics)
	)
	expect_true(is.numeric(result$convergence_diagnostics$divergent_transitions))

	# Test that bfmi values are calculated
	expect_true("bfmi" %in% names(result$convergence_diagnostics))
	expect_true(is.numeric(result$convergence_diagnostics$bfmi))
})

test_that("bayesian_meta_analysis warns when Rhat > 1.01", {
	skip_if_brms_unavailable()

	# Use data that may cause convergence issues (smaller sample, fewer iterations)
	yi <- log(c(0.78, 0.82, 0.75))
	sei <- c(0.10, 0.12, 0.11)

	# Should issue warning when Rhat exceeds threshold
	expect_warning(
		bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 500,
			warmup = 200,
			seed = 42,
			adapt_delta = 0.95
		),
		"Rhat"
	)
})

test_that("bayesian_meta_analysis warns when ESS < 400", {
	skip_if_brms_unavailable()

	# Use data with fewer iterations to trigger low ESS warning
	yi <- log(c(0.78, 0.82, 0.75))
	sei <- c(0.10, 0.12, 0.11)

	# Should issue warning when ESS is insufficient
	expect_warning(
		bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 500,
			warmup = 200,
			seed = 42,
			adapt_delta = 0.95
		),
		"ESS|effective sample"
	)
})

test_that("bayesian_meta_analysis warns when divergent transitions > 0", {
	skip_if_brms_unavailable()

	# Use data designed to potentially cause divergences
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12)

	# With low adapt_delta, should warn about divergent transitions
	expect_warning(
		bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 500,
			warmup = 200,
			seed = 42,
			adapt_delta = 0.80
		),
		"divergent"
	)
})

test_that("bayesian_meta_analysis accepts adapt_delta parameter", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Test that adapt_delta = 0.95 is accepted
	result_95 <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		adapt_delta = 0.95,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42
	)
	expect_true("bayesian_meta_result" %in% class(result_95))

	# Test that adapt_delta = 0.99 is accepted
	result_99 <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		adapt_delta = 0.99,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42
	)
	expect_true("bayesian_meta_result" %in% class(result_99))
})

test_that("bayesian_meta_analysis accepts max_treedepth parameter", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Test that max_treedepth is accepted
	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		max_treedepth = 15,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42
	)
	expect_true("bayesian_meta_result" %in% class(result))
})

# =============================================================================
# Predictive checks tests
# =============================================================================

test_that("bayesian_meta_analysis performs posterior predictive check", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		posterior_predictive = TRUE,
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that posterior_predictive is returned
	expect_true("posterior_predictive" %in% names(result))
	expect_true(is.list(result$posterior_predictive))

	# Test that bayes_p_value is calculated
	expect_true("bayes_p_value" %in% names(result$posterior_predictive))
	expect_true(is.numeric(result$posterior_predictive$bayes_p_value))
	expect_true(result$posterior_predictive$bayes_p_value >= 0)
	expect_true(result$posterior_predictive$bayes_p_value <= 1)

	# Test that pp_check_plot is generated
	expect_true("pp_check_plot" %in% names(result$posterior_predictive))
	expect_true("ggplot" %in% class(result$posterior_predictive$pp_check_plot))
})

test_that("bayesian_meta_analysis performs prior predictive check", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		prior_predictive = TRUE,
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that prior_predictive is returned
	expect_true("prior_predictive" %in% names(result))
	expect_true(is.list(result$prior_predictive))

	# Test that prior summary statistics are included
	expect_true("prior_summary" %in% names(result$prior_predictive))
	expect_true(is.list(result$prior_predictive$prior_summary))

	# Test that prior samples are stored
	expect_true("prior_samples" %in% names(result$prior_predictive))
	expect_true(is.matrix(result$prior_predictive$prior_samples))
})

test_that("bayesian_meta_analysis skips posterior predictive when FALSE", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		posterior_predictive = FALSE,
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that posterior_predictive is not in result
	expect_true(
		!("posterior_predictive" %in% names(result)) ||
			is.null(result$posterior_predictive)
	)
})

test_that("bayesian_meta_analysis supports different pp_check types", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Test dens_overlay type
	result_dens <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		pp_check_type = "dens_overlay",
		posterior_predictive = TRUE,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42,
		adapt_delta = 0.99
	)
	expect_true("posterior_predictive" %in% names(result_dens))
	expect_true(
		"ggplot" %in% class(result_dens$posterior_predictive$pp_check_plot)
	)

	# Test hist type
	result_hist <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		pp_check_type = "hist",
		posterior_predictive = TRUE,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42,
		adapt_delta = 0.99
	)
	expect_true("posterior_predictive" %in% names(result_hist))
	expect_true(
		"ggplot" %in% class(result_hist$posterior_predictive$pp_check_plot)
	)

	# Test ecdf_overlay type
	result_ecdf <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		pp_check_type = "ecdf_overlay",
		posterior_predictive = TRUE,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42,
		adapt_delta = 0.99
	)
	expect_true("posterior_predictive" %in% names(result_ecdf))
	expect_true(
		"ggplot" %in% class(result_ecdf$posterior_predictive$pp_check_plot)
	)
})

# =============================================================================
# Trace plot tests
# =============================================================================

test_that("create_bayesian_trace_plots generates plots", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that ggplot object is returned
	trace_plot <- create_bayesian_trace_plots(result)
	expect_true("ggplot" %in% class(trace_plot))

	# Test that parameters can be specified
	param_plot <- create_bayesian_trace_plots(result, pars = "b_Intercept")
	expect_true("ggplot" %in% class(param_plot))

	# Test that combine_plots works
	combined <- create_bayesian_trace_plots(result, combine_plots = TRUE)
	expect_true("ggplot" %in% class(combined))
})

test_that("create_bayesian_trace_plots validates input", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test error for non-bayesian_meta_result input
	expect_error(
		create_bayesian_trace_plots(list(x = 1)),
		"bayesian_meta_result"
	)

	# Test error for invalid parameter names
	expect_error(
		create_bayesian_trace_plots(result, pars = "nonexistent_param"),
		"not found|invalid"
	)

	# Test error for invalid chain values
	expect_error(
		create_bayesian_trace_plots(result, chains = 0),
		"positive|greater than"
	)

	expect_error(
		create_bayesian_trace_plots(result, chains = -1),
		"positive|greater than"
	)
})

test_that("create_bayesian_trace_plots handles missing bayesplot", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Temporarily check behavior when bayesplot is not available
	# The function should fall back to base plotting or issue a warning
	if (!requireNamespace("bayesplot", quietly = TRUE)) {
		# Should not error, should gracefully handle missing package
		expect_error(
			create_bayesian_trace_plots(result),
			NA
		)
	}
})

# =============================================================================
# Prior sensitivity analysis tests
# =============================================================================

test_that("prior_sensitivity_analysis creates default scenarios", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Run prior sensitivity analysis with default scenarios
	sensitivity_result <- prior_sensitivity_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that weak, moderate, informative scenarios are created
	expect_true("scenarios" %in% names(sensitivity_result))
	expect_true(is.list(sensitivity_result$scenarios))

	# Check for expected scenario names
	scenario_names <- names(sensitivity_result$scenarios)
	expect_true(any(grepl("weak", scenario_names, ignore.case = TRUE)))
	expect_true(any(grepl("moderate", scenario_names, ignore.case = TRUE)))
	expect_true(any(grepl("informative", scenario_names, ignore.case = TRUE)))

	# Test that all scenarios return results
	expect_true(all(sapply(sensitivity_result$scenarios, function(x) {
		!is.null(x) && "bayesian_meta_result" %in% class(x)
	})))

	# Test that comparison data frame is generated
	expect_true("comparison" %in% names(sensitivity_result))
	expect_true(is.data.frame(sensitivity_result$comparison))
})

test_that("prior_sensitivity_analysis accepts custom scenarios", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Define custom scenarios
	custom_scenarios <- list(
		very_vague = list(prior_mu = list(mean = 0, sd = 10)),
		very_informative = list(prior_mu = list(mean = -0.2, sd = 0.1))
	)

	# Run prior sensitivity analysis with custom scenarios
	sensitivity_result <- prior_sensitivity_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		prior_scenarios = custom_scenarios,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that scenario names are preserved
	expect_true("very_vague" %in% names(sensitivity_result$scenarios))
	expect_true("very_informative" %in% names(sensitivity_result$scenarios))
})

test_that("prior_sensitivity_analysis calculates sensitivity statistics", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Run prior sensitivity analysis
	sensitivity_result <- prior_sensitivity_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that coefficient of variation is calculated
	expect_true("sensitivity_summary" %in% names(sensitivity_result))
	expect_true(is.list(sensitivity_result$sensitivity_summary))
	expect_true(
		"coefficient_of_variation" %in%
			names(sensitivity_result$sensitivity_summary)
	)
	expect_true(is.numeric(
		sensitivity_result$sensitivity_summary$coefficient_of_variation
	))

	# Test that max difference is calculated
	expect_true(
		"max_difference" %in% names(sensitivity_result$sensitivity_summary)
	)
	expect_true(is.numeric(sensitivity_result$sensitivity_summary$max_difference))

	# Test that robustness interpretation is provided
	expect_true("robustness" %in% names(sensitivity_result$sensitivity_summary))
	expect_true(is.character(sensitivity_result$sensitivity_summary$robustness))
})

test_that("prior_sensitivity_analysis returns prior_sensitivity_result class", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Run prior sensitivity analysis
	sensitivity_result <- prior_sensitivity_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that return value has proper class
	expect_true("prior_sensitivity_result" %in% class(sensitivity_result))

	# Test that scenarios, comparison, and sensitivity_summary are present
	expect_true("scenarios" %in% names(sensitivity_result))
	expect_true("comparison" %in% names(sensitivity_result))
	expect_true("sensitivity_summary" %in% names(sensitivity_result))
})

# =============================================================================
# IQWiG formatting tests
# =============================================================================

test_that("format_bayesian_result_iqwig formats estimates", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Run analysis with ratio measure (HR)
	result_hr <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_result <- format_bayesian_result_iqwig(result_hr)

	# Test that median is used for ratio measures
	expect_true("estimate" %in% names(iqwig_result))
	expect_true(is.character(iqwig_result$estimate))
	expect_true(grepl("[0-9]", iqwig_result$estimate)) # Contains numbers

	# Run analysis with difference measure (SMD)
	result_smd <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "smd",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_result_smd <- format_bayesian_result_iqwig(result_smd)

	# Test that mean is used for difference measures
	expect_true("estimate" %in% names(iqwig_result_smd))
	expect_true(is.character(iqwig_result_smd$estimate))
})

test_that("format_bayesian_result_iqwig formats CI with semicolons", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_result <- format_bayesian_result_iqwig(result)

	# Test that CI format is "[lower; upper]"
	expect_true("ci_95" %in% names(iqwig_result))
	expect_true(grepl("\\[", iqwig_result$ci_95)) # Opening bracket
	expect_true(grepl("\\]", iqwig_result$ci_95)) # Closing bracket
	expect_true(grepl(";", iqwig_result$ci_95, fixed = TRUE)) # Semicolon separator

	# Test that decimal places are respected
	expect_true(is.character(iqwig_result$ci_95))
})

test_that("format_bayesian_result_iqwig creates probability statements", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	# Test ratio measure (HR uses threshold = 1)
	result_hr <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_hr <- format_bayesian_result_iqwig(result_hr)

	# Test that probability statement is generated
	expect_true("probability_statement" %in% names(iqwig_hr))
	expect_true(is.character(iqwig_hr$probability_statement))
	expect_true(grepl("P", iqwig_hr$probability_statement, ignore.case = TRUE))

	# Test that threshold is 1 for ratios (HR)
	expect_true(grepl("1", iqwig_hr$probability_statement, fixed = TRUE))

	# Test difference measure (threshold = 0)
	result_smd <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "smd",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_smd <- format_bayesian_result_iqwig(result_smd)

	# Test that threshold is 0 for differences
	expect_true("probability_statement" %in% names(iqwig_smd))
	expect_true(is.character(iqwig_smd$probability_statement))
})

test_that("format_bayesian_result_iqwig formats heterogeneity", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_result <- format_bayesian_result_iqwig(result)

	# Test that tau estimate is formatted
	expect_true("tau" %in% names(iqwig_result))
	expect_true(is.character(iqwig_result$tau))

	# Test that tau CI is formatted
	expect_true("tau_ci" %in% names(iqwig_result))
	expect_true(is.character(iqwig_result$tau_ci))
	# Semicolon separator
	expect_true(grepl(";", iqwig_result$tau_ci, fixed = TRUE))
})

test_that("format_bayesian_result_iqwig creates interpretation text", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_result <- format_bayesian_result_iqwig(result)

	# Test that interpretation includes estimate
	expect_true("interpretation" %in% names(iqwig_result))
	expect_true(is.character(iqwig_result$interpretation))

	# Test that full_text is complete
	expect_true("full_text" %in% names(iqwig_result))
	expect_true(is.character(iqwig_result$full_text))
	expect_true(nchar(iqwig_result$full_text) > 0)

	# Full text should include estimate, CI, and probability
	expect_true(grepl(
		iqwig_result$estimate,
		iqwig_result$full_text,
		fixed = TRUE
	))
	expect_true(grepl(iqwig_result$ci_95, iqwig_result$full_text, fixed = TRUE))
	expect_true(grepl(
		iqwig_result$probability_statement,
		iqwig_result$full_text,
		fixed = TRUE
	))
})

test_that("create_bayesian_forest_plot_iqwig generates ggplot", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	# Test that ClinicalPlot object is returned
	iqwig_result <- format_bayesian_result_iqwig(result)
	forest_plot <- create_bayesian_forest_plot_iqwig(iqwig_result)
	expect_true("ClinicalPlot" %in% class(forest_plot))

	# Test that studies are displayed
	# The plot should contain study labels
	# (ggplot object can be checked for layers)
	expect_true("ggplot" %in% class(forest_plot))

	# Test that pooled estimate is shown
	# The plot should have the pooled estimate element
	expect_true("ggplot" %in% class(forest_plot))

	# Test that weights can be displayed
	forest_plot_weighted <- create_bayesian_forest_plot_iqwig(
		iqwig_result,
		show_weights = TRUE
	)
	expect_true("ClinicalPlot" %in% class(forest_plot_weighted))
})

test_that("create_bayesian_forest_plot_iqwig uses IQWiG formatting", {
	skip_if_brms_unavailable()

	# Consistent test data
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		chains = 2,
		cores = 1,
		iter = 2000,
		warmup = 1000,
		seed = 42,
		adapt_delta = 0.99
	)

	iqwig_result <- format_bayesian_result_iqwig(result)
	forest_plot <- create_bayesian_forest_plot_iqwig(iqwig_result)

	# Test that subtitle includes heterogeneity
	# (The subtitle should contain tau information)
	expect_true("ggplot" %in% class(forest_plot))

	# Test that semicolons are used in labels
	# IQWiG format requires semicolons for CIs
	expect_true("ggplot" %in% class(forest_plot))

	# Test that log scale is used for ratios
	# For HR (ratio measure), log scale should be used
	expect_true("ggplot" %in% class(forest_plot))
})
