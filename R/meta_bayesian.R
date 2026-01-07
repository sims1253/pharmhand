#' @title Bayesian Meta-Analysis Functions
#' @name meta_bayesian
#' @description Interface for Bayesian meta-analysis using brms/rstan.
NULL

#' Bayesian Meta-Analysis
#'
#' Interface for Bayesian meta-analysis using brms/rstan when available.
#' Provides guidance when dependencies are not installed.
#'
#' @details
#' This function requires the brms and rstan packages for full Bayesian
#' inference. If these are not installed, the function returns guidance
#' on installation and falls back to frequentist meta-analysis via
#' \code{\link{meta_analysis}}.
#'
#' Install dependencies with:
#' \code{install.packages(c("brms", "rstan"))}
#'
#' Note: rstan may require additional setup. See
#' \url{https://mc-stan.org/users/interfaces/rstan} for details.
#'
#' @param yi Numeric vector of effect estimates
#' @param sei Numeric vector of standard errors
#' @param study_labels Character vector of study names (optional, defaults to
#'   "Study 1", "Study 2", etc.)
#' @param effect_measure Character. Effect type: "hr" (hazard ratio),
#'   "or" (odds ratio), "rr" (risk ratio), "rd" (risk difference),
#'   "md" (mean difference), "smd" (standardized mean difference)
#' @param prior_mu Prior for overall effect: list(mean, sd). Controls the
#'   normal prior on the overall pooled effect. Default: list(mean = 0, sd = 10)
#' @param prior_tau Prior for heterogeneity: list(type, scale). Valid types:
#'   "half_cauchy", "half_normal", "exponential". Scale controls expected
#'   heterogeneity magnitude. Default: list(type = "half_cauchy", scale = 0.5)
#' @param prior_predictive Logical. Whether to perform prior predictive check.
#'   When TRUE, samples from the prior predictive distribution are generated
#'   and returned. Default: FALSE
#' @param posterior_predictive Logical. Whether to perform posterior predictive
#'   check. Generates posterior predictive checks using pp_check().
#'   Default: TRUE
#' @param pp_check_type Character. Type of posterior predictive plot/check.
#'   See ?brms::pp_check for options. Common values: "dens_overlay",
#'   "hist", "scatter", "stat". Default: "dens_overlay"
#' @param pp_ndraws Integer. Number of draws to use for posterior predictive
#'   checks. Default: 100
#' @param chains Integer. Number of MCMC chains. Default: 4
#' @param iter Integer. Total iterations per chain. Default: 4000
#' @param warmup Integer. Warmup iterations. Default: 2000
#' @param seed Integer. Random seed
#' @param adapt_delta Numeric. MCMC sampler tuning parameter (0-1).
#'   Default: 0.95. Higher values reduce divergent transitions but slow
#'   sampling.
#' @param max_treedepth Integer. Maximum tree depth for NUTS sampler.
#'   Default: 12. Higher values allow more complex posterior geometry but
#'   may indicate issues.
#' @param ... Additional arguments passed to brms::brm
#'
#' @return A list with class "bayesian_meta_result" containing posterior_mean,
#'   ci_95, tau_mean, tau_ci_95, n_studies, effect_measure, model_type, fit,
#'   convergence_diagnostics, prior_predictive, posterior_predictive, and
#'   pp_check_plot. See Details for full descriptions. Returns installation
#'   guidance if brms is not installed.
#'
#' @examples
#' \dontrun{
#' # Basic Bayesian random-effects meta-analysis of hazard ratios
#' # Effect estimates must be on log scale for ratio measures
#' yi <- log(c(0.75, 0.82, 0.68, 0.91))  # log(HR) from 4 studies
#' sei <- c(0.12, 0.15, 0.18, 0.14)       # standard errors
#'
#' result <- bayesian_meta_analysis(
#'   yi = yi,
#'   sei = sei,
#'   effect_measure = "hr",  # Hazard ratio (requires log-transformed yi)
#'   chains = 2,
#'   iter = 2000
#' )
#'
#' # View posterior summary
#' result$posterior_mean
#' result$ci_95
#' result$interpretation
#'
#' # With predictive checking enabled
#' result_with_pp <- bayesian_meta_analysis(
#'   yi = yi,
#'   sei = sei,
#'   effect_measure = "hr",
#'   prior_predictive = TRUE,
#'   posterior_predictive = TRUE,
#'   pp_check_type = "dens_overlay",
#'   pp_ndraws = 100
#' )
#'
#' # Access prior predictive results
#' result_with_pp$prior_predictive$summary
#'
#' # Access posterior predictive results
#' result_with_pp$posterior_predictive$bayes_p_value
#'
#' # Get the pp_check plot
#' print(result_with_pp$pp_check_plot)
#' }
#' @export
bayesian_meta_analysis <- function(
	yi,
	sei,
	study_labels = NULL,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	prior_mu = list(mean = 0, sd = 10),
	prior_tau = list(type = "half_cauchy", scale = 0.5),
	prior_predictive = FALSE,
	posterior_predictive = TRUE,
	pp_check_type = "dens_overlay",
	pp_ndraws = 100,
	chains = 4,
	iter = 4000,
	warmup = 2000,
	seed = NULL,
	adapt_delta = 0.95,
	max_treedepth = 12,
	...
) {
	effect_measure <- match.arg(effect_measure)

	# Check for brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_inform(paste(
			"Bayesian meta-analysis requires the 'brms' package.",
			"Install with: install.packages('brms')",
			"",
			"brms also requires rstan or cmdstanr. See:",
			"https://mc-stan.org/users/interfaces/rstan",
			sep = "\n"
		))

		# Return frequentist result as fallback with guidance
		ph_inform("Returning frequentist random-effects estimate as fallback...")

		return(meta_analysis(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			effect_measure = effect_measure,
			model = "random",
			method = "REML"
		))
	}

	k <- length(yi)

	if (is.null(study_labels)) {
		study_labels <- paste("Study", seq_len(k))
	}

	if (!is.numeric(yi)) {
		ph_abort("'yi' must be numeric", call. = FALSE)
	}
	if (!is.numeric(sei)) {
		ph_abort("'sei' must be numeric", call. = FALSE)
	}
	if (length(yi) != length(sei)) {
		ph_abort("'yi' and 'sei' must have the same length", call. = FALSE)
	}
	if (any(sei <= 0, na.rm = TRUE)) {
		ph_abort("'sei' must contain only positive values", call. = FALSE)
	}
	if (!is.null(study_labels) && length(study_labels) != k) {
		ph_abort(
			sprintf("'study_labels' must have length %d to match 'yi'", k),
			call. = FALSE
		)
	}

	# Prepare data for brms
	data <- data.frame(
		yi = yi,
		sei = sei,
		study = study_labels
	)

	# Build brms formula
	# Random-effects meta-analysis: yi ~ 1 + (1|study), with known SE
	# This is equivalent to a Bayesian random-effects model

	# Use brms with se() to specify known standard errors
	formula <- brms::bf(yi | se(sei) ~ 1 + (1 | study))

	# Build priors
	# Validate prior_tau$type
	valid_tau_types <- c("half_cauchy", "half_normal", "exponential")
	if (!prior_tau$type %in% valid_tau_types) {
		ph_abort(sprintf(
			"prior_tau$type must be one of: %s",
			paste(valid_tau_types, collapse = ", ")
		))
	}

	# Build tau prior string based on type
	# Note: brms uses lb (lower bound) to enforce non-negative constraints
	tau_prior_str <- switch(
		prior_tau$type,
		"half_cauchy" = sprintf("cauchy(0, %f)", prior_tau$scale),
		"half_normal" = sprintf("normal(0, %f)", prior_tau$scale),
		"exponential" = sprintf("exponential(%f)", 1 / prior_tau$scale)
	)

	priors <- c(
		brms::prior_string(
			sprintf("normal(%f, %f)", prior_mu$mean, prior_mu$sd),
			class = "Intercept"
		),
		brms::prior_string(tau_prior_str, class = "sd", lb = 0)
	)

	# Initialize prior predictive check results
	prior_predictive_result <- NULL

	# Handle prior predictive check
	# If prior_predictive = TRUE, fit model with sample_prior = "only"
	# to get prior samples (no likelihood, just prior)
	if (prior_predictive) {
		ph_inform("Generating prior predictive check...")

		# Fit model with only prior samples (no likelihood)
		fit_prior <- tryCatch(
			brms::brm(
				formula = formula,
				data = data,
				prior = priors,
				chains = chains,
				iter = iter,
				warmup = warmup,
				seed = seed,
				refresh = 0,
				sample_prior = "only",
				control = list(
					adapt_delta = adapt_delta,
					max_treedepth = max_treedepth
				),
				...
			),
			error = function(e) {
				ph_abort(sprintf(
					"Prior predictive sampling failed. Stan/brms error: %s",
					conditionMessage(e)
				))
			}
		)

		# Extract prior samples
		prior_samples <- brms::as_draws_df(fit_prior)

		# Calculate prior summary statistics for intercept (overall effect)
		prior_intercept <- prior_samples$b_Intercept

		# Create prior predictive summary
		prior_predictive_result <- list(
			summary = list(
				intercept_mean = mean(prior_intercept),
				intercept_sd = sd(prior_intercept),
				intercept_median = stats::median(prior_intercept),
				intercept_quantiles = stats::quantile(
					prior_intercept,
					c(0.025, 0.25, 0.5, 0.75, 0.975)
				)
			),
			samples = prior_samples,
			n_studies = k,
			effect_measure = effect_measure
		)

		ph_inform(sprintf(
			"Prior predictive check complete. Prior mean for intercept: %.3f (SD: %.3f)",
			prior_predictive_result$summary$intercept_mean,
			prior_predictive_result$summary$intercept_sd
		))
	}

	# Fit model for posterior inference
	fit <- tryCatch(
		brms::brm(
			formula = formula,
			data = data,
			prior = priors,
			chains = chains,
			iter = iter,
			warmup = warmup,
			seed = seed,
			refresh = 0,
			control = list(
				adapt_delta = adapt_delta,
				max_treedepth = max_treedepth
			),
			...
		),
		error = function(e) {
			ph_abort(paste0(
				"Bayesian meta-analysis sampling failed. Stan/brms error: ",
				conditionMessage(e),
				". Consider using cores = 1 or checking Stan installation."
			))
		}
	)

	# Extract posteriors
	posterior_samples <- brms::as_draws_df(fit)

	# Extract convergence diagnostics
	rhat_values <- brms::rhat(fit)
	max_rhat <- max(rhat_values, na.rm = TRUE)

	# Get ESS from neff_ratio (ratio of effective to total samples)
	# Total samples = (iter - warmup) * chains
	total_samples <- (iter - warmup) * chains
	neff_ratios <- brms::neff_ratio(fit)
	bulk_ess_values <- neff_ratios * total_samples
	min_bulk_ess <- min(bulk_ess_values, na.rm = TRUE)
	# Use same for tail ESS (neff_ratio is overall ESS ratio)
	min_tail_ess <- min_bulk_ess

	# Extract BFMI from nuts_params energy values
	np <- brms::nuts_params(fit)
	# BFMI calculation: var(E - E_prev) / var(E) per chain
	# If energy not available, set to NA
	energy_vals <- np$Value[np$Parameter == "energy__"]
	if (length(energy_vals) > 0) {
		# Group by chain and compute BFMI
		n_chains <- chains
		samples_per_chain <- length(energy_vals) / n_chains
		bfmi_values <- vapply(
			seq_len(n_chains),
			function(i) {
				idx <- ((i - 1) * samples_per_chain + 1):(i * samples_per_chain)
				e <- energy_vals[idx]
				if (length(e) < 2) {
					return(NA_real_)
				}
				var(diff(e)) / var(e)
			},
			numeric(1)
		)
		min_bfmi <- min(bfmi_values, na.rm = TRUE)
	} else {
		bfmi_values <- NA_real_
		min_bfmi <- NA_real_
	}

	# Extract divergent transitions
	divergent_transitions <- sum(
		np$Value[np$Parameter == "divergent__"],
		na.rm = TRUE
	)

	# Generate convergence warnings
	if (max_rhat > 1.01) {
		ph_inform(sprintf(
			paste0(
				"WARNING: Max Rhat = %.3f > 1.01. ",
				"This indicates potential convergence issues."
			),
			max_rhat
		))
	}

	if (min_bulk_ess < 400) {
		ph_inform(sprintf(
			"WARNING: Min bulk ESS = %d < 400. Consider running more iterations.",
			min_bulk_ess
		))
	}

	if (min_tail_ess < 400) {
		ph_inform(sprintf(
			"WARNING: Min tail ESS = %d < 400. Consider running more iterations.",
			min_tail_ess
		))
	}

	if (divergent_transitions > 0) {
		ph_inform(sprintf(
			paste0(
				"WARNING: %d divergent transitions detected. ",
				"Consider increasing adapt_delta."
			),
			divergent_transitions
		))
	}

	if (any(bfmi_values < 0.3, na.rm = TRUE)) {
		ph_inform(sprintf(
			paste0(
				"WARNING: Low BFMI detected (min = %.3f). ",
				"This may indicate computational issues."
			),
			min_bfmi
		))
	}

	# Create convergence diagnostics list
	convergence_diagnostics <- list(
		max_rhat = max_rhat,
		min_bulk_ess = min_bulk_ess,
		min_tail_ess = min_tail_ess,
		divergent_transitions = divergent_transitions,
		bfmi = bfmi_values,
		all_rhat_ok = max_rhat <= 1.01,
		bulk_ess_ok = min_bulk_ess >= 400,
		tail_ess_ok = min_tail_ess >= 400,
		no_divergences = divergent_transitions == 0,
		bfmi_ok = all(bfmi_values >= 0.3, na.rm = TRUE)
	)

	# Summarize
	mu_samples <- posterior_samples$b_Intercept
	tau_samples <- posterior_samples$sd_study__Intercept

	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	if (is_ratio) {
		mu_display <- exp(mu_samples)
	} else {
		mu_display <- mu_samples
	}

	# Initialize posterior predictive check results
	posterior_predictive_result <- NULL
	pp_check_plot <- NULL

	# Handle posterior predictive check
	if (posterior_predictive) {
		ph_inform("Generating posterior predictive check...")

		# Generate pp_check plot
		pp_check_plot <- brms::pp_check(
			fit,
			type = pp_check_type,
			ndraws = pp_ndraws
		)

		# Calculate Bayesian p-value using a simple discrepancy measure
		# Using the mean as the discrepancy function
		y_rep <- brms::posterior_predict(fit, draw_ids = 1:pp_ndraws)

		# Calculate Bayesian p-value: proportion of y_rep >= y
		# This is a one-sided p-value for the discrepancy measure
		if (is.matrix(y_rep) || is.data.frame(y_rep)) {
			# y_rep is a matrix with ndraws x n observations
			# For each observation, calculate proportion of y_rep >= observed y
			y_obs <- data$yi
			bayes_p_value <- numeric(length(y_obs))

			for (i in seq_along(y_obs)) {
				if (is.matrix(y_rep)) {
					bayes_p_value[i] <- mean(y_rep[, i] >= y_obs[i])
				} else {
					bayes_p_value[i] <- mean(as.numeric(y_rep[, i]) >= y_obs[i])
				}
			}

			# Overall Bayesian p-value is the mean of individual p-values
			overall_bayes_p <- mean(bayes_p_value)
		} else {
			# Fallback if y_rep format is unexpected
			overall_bayes_p <- NA
			ph_inform(
				"Warning: Could not compute Bayesian p-value due to unexpected y_rep format"
			)
		}

		posterior_predictive_result <- list(
			bayes_p_value = overall_bayes_p,
			type = pp_check_type,
			ndraws = pp_ndraws,
			individual_p_values = if (exists("bayes_p_value")) bayes_p_value else NULL
		)

		ph_inform(sprintf(
			"Posterior predictive check complete. Bayesian p-value: %.3f",
			overall_bayes_p
		))
	}

	# Posterior summaries
	result <- list(
		posterior_mean = mean(mu_display),
		posterior_median = stats::median(mu_display),
		ci_95 = stats::quantile(mu_display, c(0.025, 0.975)),
		ci_80 = stats::quantile(mu_display, c(0.10, 0.90)),
		tau_mean = mean(tau_samples),
		tau_median = stats::median(tau_samples),
		tau_ci_95 = stats::quantile(tau_samples, c(0.025, 0.975)),
		prob_positive = if (is_ratio) {
			mean(mu_display > 1)
		} else {
			mean(mu_display > 0)
		},
		prob_negative = if (is_ratio) {
			mean(mu_display < 1)
		} else {
			mean(mu_display < 0)
		},
		n_studies = k,
		effect_measure = effect_measure,
		model_type = "bayesian_random_effects",
		fit = fit, # Full brms fit object
		convergence_diagnostics = convergence_diagnostics,
		interpretation = sprintf(
			"Posterior %s: %.3f (95%% CrI: %.3f to %.3f). P(%s < %s) = %.1f%%",
			if (is_ratio) "median" else "mean",
			if (is_ratio) stats::median(mu_display) else mean(mu_display),
			stats::quantile(mu_display, 0.025),
			stats::quantile(mu_display, 0.975),
			"effect",
			if (is_ratio) "1" else "0",
			100 * if (is_ratio) mean(mu_display < 1) else mean(mu_display < 0)
		)
	)

	# Add predictive checking results if enabled
	if (prior_predictive) {
		result$prior_predictive <- prior_predictive_result
	}

	if (posterior_predictive) {
		result$posterior_predictive <- posterior_predictive_result
		result$pp_check_plot <- pp_check_plot
	}

	class(result) <- c("bayesian_meta_result", class(result))
	result
}


#' @title Create Trace Plots for Bayesian Meta-Analysis
#' @name create_bayesian_trace_plots
#' @description Generate trace plots to visualize MCMC chain convergence
#'
#' @param bayesian_result A bayesian_meta_result object from
#'   bayesian_meta_analysis()
#' @param parameters Character vector of parameters to plot. Default:
#'   c("b_Intercept", "sd_study__Intercept")
#' @param chains Integer. Number of chains to plot. Default: NULL (all chains)
#' @param combine_plots Logical. If TRUE, combine all parameters into one plot.
#'   Default: TRUE
#' @param ... Additional arguments passed to brms::plot()
#'
#' @return A ggplot object or list of ggplot objects
#'
#' @examples
#' \dontrun{
#' result <- bayesian_meta_analysis(yi = yi, sei = sei)
#' trace_plot <- create_bayesian_trace_plots(result)
#' print(trace_plot)
#' }
#' @export
create_bayesian_trace_plots <- function(
	bayesian_result,
	parameters = c("b_Intercept", "sd_study__Intercept"),
	chains = NULL,
	combine_plots = TRUE,
	...
) {
	# Validate input is a bayesian_meta_result object
	if (!inherits(bayesian_result, "bayesian_meta_result")) {
		ph_abort(
			"Input must be a bayesian_meta_result object from bayesian_meta_analysis()",
			call. = FALSE
		)
	}

	# Check for brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_abort(
			"Creating trace plots requires the 'brms' package.",
			call. = FALSE
		)
	}

	# Extract the brms fit object
	fit <- bayesian_result$fit

	if (is.null(fit)) {
		ph_abort(
			"The bayesian_result object does not contain a fit object.",
			call. = FALSE
		)
	}

	# Validate parameters exist in the model
	available_params <- brms::parnames(fit)
	invalid_params <- parameters[!parameters %in% available_params]

	if (length(invalid_params) > 0) {
		ph_inform(sprintf(
			"The following parameters are not in the model and will be skipped: %s",
			paste(invalid_params, collapse = ", ")
		))
		parameters <- parameters[parameters %in% available_params]

		if (length(parameters) == 0) {
			ph_abort(
				"None of the specified parameters are available in the model.",
				call. = FALSE
			)
		}
	}

	# Handle chains parameter
	n_chains <- brms::nchains(fit)

	if (!is.null(chains)) {
		if (!is.numeric(chains) || length(chains) != 1 || chains < 1) {
			ph_abort("'chains' must be a positive integer", call. = FALSE)
		}
		if (chains > n_chains) {
			ph_abort(
				sprintf(
					"Requested %d chains but model only has %d chains",
					chains,
					n_chains
				),
				call. = FALSE
			)
		}
	}

	ph_inform(sprintf(
		"Generating trace plots for %d parameter(s): %s",
		length(parameters),
		paste(parameters, collapse = ", ")
	))

	# Generate trace plots
	if (combine_plots) {
		# Combine all parameters into one plot
		# brms::plot with ask = FALSE will return a combined plot
		old_ask <- graphics::par(ask = FALSE)
		on.exit(graphics::par(ask = old_ask), add = TRUE)

		plot_result <- brms::plot(
			fit,
			parameters = parameters,
			...
		)

		# plot() returns NULL invisibly when combine_plots = TRUE (default behavior)
		# The plot is rendered directly to the graphics device
		# We need to return a ggplot object, so we use bayesplot::mcmc_trace instead

		# Try using mcmc_trace from bayesplot for better control
		if (requireNamespace("bayesplot", quietly = TRUE)) {
			# Get posterior draws
			posterior <- brms::as_draws_df(fit, variable = parameters)

			# Filter chains if specified
			if (!is.null(chains)) {
				# bayesplot mcmc_trace can handle subsetting
				chain_ids <- seq_len(chains)
			} else {
				chain_ids <- NULL
			}

			# Create combined trace plot using bayesplot
			if (is.null(chain_ids)) {
				trace_plot <- bayesplot::mcmc_trace(posterior, pars = parameters)
			} else {
				trace_plot <- bayesplot::mcmc_trace(
					posterior,
					pars = parameters,
					facet_args = list(nrow = length(parameters))
				)
			}

			ph_inform("Trace plots generated successfully")
			return(trace_plot)
		} else {
			# Fallback to brms::plot if bayesplot not available
			# This will print directly to the graphics device
			brms::plot(fit, parameters = parameters, ask = FALSE, ...)

			ph_inform(
				"Trace plots rendered using brms. Consider installing 'bayesplot' ",
				"for ggplot objects."
			)

			# Return invisible NULL since brms::plot doesn't return a ggplot
			return(invisible(NULL))
		}
	} else {
		# Return individual plots for each parameter

		if (!requireNamespace("bayesplot", quietly = TRUE)) {
			ph_abort(
				"Returning individual plots requires the 'bayesplot' package. ",
				"Install with: install.packages('bayesplot')",
				call. = FALSE
			)
		}

		# Get posterior draws
		posterior <- brms::as_draws_df(fit, variable = parameters)

		# Create individual plots for each parameter
		plot_list <- lapply(parameters, function(param) {
			bayesplot::mcmc_trace(posterior, pars = param)
		})

		names(plot_list) <- parameters

		ph_inform(sprintf(
			"Generated %d individual trace plots",
			length(parameters)
		))

		return(plot_list)
	}
}


#' @title Prior Sensitivity Analysis for Bayesian Meta-Analysis
#' @name prior_sensitivity_analysis
#' @description
#' Assess how sensitive results are to different prior specifications
#'
#' @param yi Numeric vector of effect estimates
#' @param sei Numeric vector of standard errors
#' @param study_labels Character vector of study names (optional)
#' @param effect_measure Character. Effect type
#' @param prior_scenarios List of prior specification scenarios. Each scenario
#'   should have:
#'   `name`: Character name for scenario
#'   `prior_mu`: List with `mean` and `sd`
#'   `prior_tau`: List with `type` and `scale`
#' @param chains Integer. Number of MCMC chains per scenario. Default: 2
#' @param iter Integer. Total iterations per chain. Default: 2000
#' @param seed Integer. Random seed for reproducibility
#' @param ... Additional arguments passed to bayesian_meta_analysis()
#'
#' @return A list containing:
#'   \item{scenarios}{List of bayesian_meta_result objects for each scenario}
#'   \item{comparison}{Data frame comparing estimates across scenarios}
#'   \item{sensitivity_summary}{Summary of how results change across priors}
#'
#' @examples
#' \dontrun{
#' scenarios <- list(
#'   weak = list(
#'     prior_mu = list(mean = 0, sd = 10),
#'     prior_tau = list(type = "half_cauchy", scale = 0.5)
#'   ),
#'   informative = list(
#'     prior_mu = list(mean = 0, sd = 1),
#'     prior_tau = list(type = "half_cauchy", scale = 0.25)
#'   )
#' )
#'
#' sensitivity <- prior_sensitivity_analysis(
#'   yi = yi,
#'   sei = sei,
#'   effect_measure = "hr",
#'   prior_scenarios = scenarios
#' )
#' print(sensitivity$comparison)
#' }
#' @export
prior_sensitivity_analysis <- function(
	yi,
	sei,
	study_labels = NULL,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	prior_scenarios = NULL,
	chains = 2,
	iter = 2000,
	seed = 42,
	...
) {
	effect_measure <- match.arg(effect_measure)

	# Validate inputs
	if (!is.numeric(yi) || !is.numeric(sei)) {
		ph_abort("'yi' and 'sei' must be numeric vectors", call. = FALSE)
	}
	if (length(yi) != length(sei)) {
		ph_abort("'yi' and 'sei' must have the same length", call. = FALSE)
	}

	# Create default scenarios if none provided
	if (is.null(prior_scenarios)) {
		ph_inform("No prior scenarios specified. Using default scenarios...")
		prior_scenarios <- list(
			weak = list(
				prior_mu = list(mean = 0, sd = 10),
				prior_tau = list(type = "half_cauchy", scale = 0.5)
			),
			moderate = list(
				prior_mu = list(mean = 0, sd = 2),
				prior_tau = list(type = "half_cauchy", scale = 0.3)
			),
			informative = list(
				prior_mu = list(mean = 0, sd = 1),
				prior_tau = list(type = "half_cauchy", scale = 0.2)
			)
		)
	}

	# Ensure prior_scenarios is a list
	if (!is.list(prior_scenarios)) {
		ph_abort(
			"'prior_scenarios' must be a list of prior specifications",
			call. = FALSE
		)
	}

	n_scenarios <- length(prior_scenarios)
	scenario_names <- names(prior_scenarios)

	if (is.null(scenario_names)) {
		scenario_names <- paste0("scenario_", seq_len(n_scenarios))
		names(prior_scenarios) <- scenario_names
	}

	# Check brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_inform(paste(
			"Prior sensitivity analysis requires the 'brms' package.",
			"Install with: install.packages('brms')",
			sep = "\n"
		))
		return(NULL)
	}

	ph_inform(sprintf(
		"Running prior sensitivity analysis with %d scenario(s)...",
		n_scenarios
	))

	# Run bayesian_meta_analysis for each scenario
	results_list <- list()
	comparison_df <- data.frame(
		scenario = character(),
		posterior_mean = numeric(),
		posterior_median = numeric(),
		ci_95_lower = numeric(),
		ci_95_upper = numeric(),
		tau_mean = numeric(),
		prob_positive = numeric(),
		stringsAsFactors = FALSE
	)

	for (i in seq_along(prior_scenarios)) {
		scenario_name <- scenario_names[i]
		scenario <- prior_scenarios[[i]]

		ph_inform(sprintf("  Running scenario: %s", scenario_name))

		# Validate scenario structure
		if (
			!is.list(scenario) ||
				is.null(scenario$prior_mu) ||
				is.null(scenario$prior_tau)
		) {
			ph_inform(sprintf(
				"    Warning: Skipping '%s' - invalid prior specification",
				scenario_name
			))
			next
		}

		# Set defaults for prior_mu if missing components
		if (is.null(scenario$prior_mu$mean)) {
			scenario$prior_mu$mean <- 0
		}
		if (is.null(scenario$prior_mu$sd)) {
			scenario$prior_mu$sd <- 10
		}

		# Set defaults for prior_tau if missing components
		if (is.null(scenario$prior_tau$type)) {
			scenario$prior_tau$type <- "half_cauchy"
		}
		if (is.null(scenario$prior_tau$scale)) {
			scenario$prior_tau$scale <- 0.5
		}

		# Run the Bayesian analysis for this scenario
		result <- tryCatch(
			{
				bayesian_meta_analysis(
					yi = yi,
					sei = sei,
					study_labels = study_labels,
					effect_measure = effect_measure,
					prior_mu = scenario$prior_mu,
					prior_tau = scenario$prior_tau,
					chains = chains,
					iter = iter,
					seed = seed + i, # Different seed for each scenario
					posterior_predictive = FALSE, # Disable for speed
					...
				)
			},
			error = function(e) {
				ph_inform(sprintf(
					"    Error in scenario '%s': %s",
					scenario_name,
					e$message
				))
				NULL
			}
		)

		if (!is.null(result)) {
			results_list[[scenario_name]] <- result

			# Add to comparison dataframe
			comparison_df <- rbind(
				comparison_df,
				data.frame(
					scenario = scenario_name,
					posterior_mean = result$posterior_mean,
					posterior_median = result$posterior_median,
					ci_95_lower = result$ci_95[1],
					ci_95_upper = result$ci_95[2],
					tau_mean = result$tau_mean,
					prob_positive = result$prob_positive,
					stringsAsFactors = FALSE
				)
			)
		}
	}

	# Calculate sensitivity statistics
	if (nrow(comparison_df) < 2) {
		sensitivity_summary <- list(
			message = paste0(
				"Insufficient scenarios (need at least 2) for sensitivity analysis"
			),
			coefficient_of_variation = NA_real_,
			max_difference = NA_real_,
			ci_coverage_consistency = NA_character_
		)
	} else {
		# Coefficient of variation across scenario estimates
		mean_estimates <- comparison_df$posterior_mean
		cv <- (sd(mean_estimates) / abs(mean(mean_estimates))) * 100

		# Maximum difference between any two scenarios
		max_diff <- max(comparison_df$posterior_mean) -
			min(comparison_df$posterior_mean)

		# Percentage of scenarios where CI includes the same threshold (0 or 1)
		is_ratio <- effect_measure %in% c("hr", "or", "rr")
		threshold <- if (is_ratio) 1 else 0

		ci_includes_threshold <- sapply(seq_len(nrow(comparison_df)), function(i) {
			ci_lower <- comparison_df$ci_95_lower[i]
			ci_upper <- comparison_df$ci_95_upper[i]
			ci_lower <= threshold && ci_upper >= threshold
		})

		ci_coverage_pct <- mean(ci_includes_threshold) * 100

		# Determine robustness interpretation
		if (cv < 5) {
			robustness_interpretation <- "highly robust"
		} else if (cv < 10) {
			robustness_interpretation <- "moderately robust"
		} else if (cv < 20) {
			robustness_interpretation <- "somewhat sensitive to priors"
		} else {
			robustness_interpretation <- "highly sensitive to priors"
		}

		# CI consistency interpretation
		if (ci_coverage_pct == 100) {
			ci_interpretation <- paste0(
				"All scenarios yield consistent inference (same conclusion)"
			)
		} else if (ci_coverage_pct >= 50) {
			ci_interpretation <- sprintf(
				"Mixed inference across scenarios (%d%% include threshold %s)",
				round(ci_coverage_pct),
				threshold
			)
		} else {
			ci_interpretation <- sprintf(
				"Consistent inference across scenarios (%d%% exclude threshold %s)",
				round(100 - ci_coverage_pct),
				threshold
			)
		}

		# Create summary text
		summary_text <- sprintf(
			paste(
				"Prior sensitivity analysis conducted across %d scenarios.",
				"Coefficient of variation: %.1f%% (%s).",
				"Maximum difference in posterior mean: %.3f.",
				"%s",
				sep = "\n"
			),
			nrow(comparison_df),
			cv,
			robustness_interpretation,
			max_diff,
			ci_interpretation
		)

		sensitivity_summary <- list(
			coefficient_of_variation = cv,
			max_difference = max_diff,
			ci_coverage_consistency = ci_coverage_pct,
			ci_includes_threshold_count = sum(ci_includes_threshold),
			total_scenarios = nrow(comparison_df),
			robustness_interpretation = robustness_interpretation,
			summary_text = summary_text
		)
	}

	# Prepare output
	output <- list(
		scenarios = results_list,
		comparison = comparison_df,
		sensitivity_summary = sensitivity_summary,
		effect_measure = effect_measure,
		n_studies = length(yi),
		call_info = list(
			chains = chains,
			iter = iter,
			seed = seed
		)
	)

	class(output) <- c("prior_sensitivity_result", class(output))

	ph_inform("Prior sensitivity analysis complete.")
	ph_inform(sprintf("  Scenarios analyzed: %d", length(results_list)))
	if (!is.na(sensitivity_summary$coefficient_of_variation)) {
		ph_inform(sprintf(
			"  Coefficient of variation: %.1f%%",
			sensitivity_summary$coefficient_of_variation
		))
	}

	output
}


# =============================================================================
# IQWiG-Compliant Bayesian Meta-Analysis Formatting Functions
# =============================================================================

#' Format Bayesian Meta-Analysis Result for IQWiG Submission
#'
#' Formats a bayesian_meta_result object according to IQWiG Methods v8.0
#' requirements. Includes effect estimate, credible interval, heterogeneity,
#' and probability statements with proper formatting (semicolons in CIs,
#' specific decimal places, etc.)
#'
#' @param bayesian_result A bayesian_meta_result object from
#'   bayesian_meta_analysis()
#' @param digits_estimate Integer. Decimal places for estimate (default: 3)
#' @param digits_ci Integer. Decimal places for CI bounds (default: 3)
#' @param digits_tau Integer. Decimal places for heterogeneity (default: 3)
#' @param locale Character. Locale for decimal separator: "en" or "de"
#'   (default: current pharmhand locale)
#' @param include_prob Logical. Include probability statements (default: TRUE)
#' @param include_interpretation Logical. Include text interpretation
#'   (default: TRUE)
#' @param ci_brackets Character vector of length 2 for CI brackets
#'   (default: `c("[", "]")` per IQWiG)
#'
#' @return A list with formatted strings for:
#'   \item{estimate}{Formatted point estimate}
#'   \item{ci}{Formatted credible interval as "lower; upper" with brackets}
#'   \item{tau}{Formatted heterogeneity estimate}
#'   \item{tau_ci}{Formatted heterogeneity CI}
#'   \item{probability}{Probability statement string}
#'   \item{interpretation}{Text interpretation}
#'   \item{full_text}{Complete formatted summary text}
#'
#' @examples
#' \dontrun{
#' yi <- log(c(0.75, 0.82))
#' sei <- c(0.12, 0.15)
#' result <- bayesian_meta_analysis(
#'   yi = yi, sei = sei, effect_measure = "hr"
#' )
#' formatted <- format_bayesian_result_iqwig(result)
#' print(formatted$full_text)
#' }
#' @export
format_bayesian_result_iqwig <- function(
	bayesian_result,
	digits_estimate = 3L,
	digits_ci = 3L,
	digits_tau = 3L,
	locale = get_locale(),
	include_prob = TRUE,
	include_interpretation = TRUE,
	ci_brackets = c("[", "]")
) {
	# Validate input is a bayesian_meta_result object
	if (!inherits(bayesian_result, "bayesian_meta_result")) {
		ph_abort(
			"Input must be a bayesian_meta_result object from bayesian_meta_analysis()",
			call. = FALSE
		)
	}

	# Validate locale
	if (!locale %in% c("en", "de")) {
		ph_abort("'locale' must be 'en' or 'de'", call. = FALSE)
	}

	# Extract relevant components from bayesian_result
	effect_measure <- bayesian_result$effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Determine point estimate based on effect measure
	# For ratios: use posterior_median (more robust, transformation-invariant)
	# For differences: use posterior_mean
	if (is_ratio) {
		point_estimate <- bayesian_result$posterior_median
		measure_label <- switch(
			effect_measure,
			"hr" = "HR",
			"or" = "OR",
			"rr" = "RR"
		)
	} else {
		point_estimate <- bayesian_result$posterior_mean
		measure_label <- switch(
			effect_measure,
			"rd" = "RD",
			"md" = "MD",
			"smd" = "SMD"
		)
	}

	# Format point estimate
	estimate_formatted <- format_number(point_estimate, digits_estimate, locale)

	# Format credible interval using existing format_ci function
	ci_formatted <- format_ci(
		bayesian_result$ci_95[1],
		bayesian_result$ci_95[2],
		digits = digits_ci,
		locale = locale,
		brackets = ci_brackets
	)

	# Format heterogeneity estimate
	tau_formatted <- format_number(bayesian_result$tau_mean, digits_tau, locale)

	# Format heterogeneity CI
	tau_ci_formatted <- format_ci(
		bayesian_result$tau_ci_95[1],
		bayesian_result$tau_ci_95[2],
		digits = digits_tau,
		locale = locale,
		brackets = ci_brackets
	)

	# Create probability statement
	# For ratios: P(HR < 1) = XX.X% (probability effect is beneficial)
	# For differences: P(effect < 0) = XX.X% (probability of negative effect)
	if (include_prob) {
		prob_beneficial <- if (is_ratio) {
			bayesian_result$prob_negative * 100
		} else {
			bayesian_result$prob_negative * 100
		}

		prob_beneficial_fmt <- format_percentage(
			prob_beneficial / 100,
			digits = 1,
			locale = locale,
			is_proportion = TRUE,
			symbol = TRUE
		)

		if (is_ratio) {
			probability_text <- sprintf(
				"P(%s < 1) = %s",
				measure_label,
				prob_beneficial_fmt
			)
		} else {
			probability_text <- sprintf("P(effect < 0) = %s", prob_beneficial_fmt)
		}
	} else {
		probability_text <- NULL
	}

	# Create interpretation text
	if (include_interpretation) {
		# Build interpretation components
		interpretation_parts <- c(
			sprintf(
				"Bayesian %s: %s %s",
				measure_label,
				estimate_formatted,
				ci_formatted
			),
			sprintf("tau = %s %s", tau_formatted, tau_ci_formatted),
			if (include_prob) probability_text else NULL
		)

		interpretation_text <- paste(
			interpretation_parts[!sapply(interpretation_parts, is.null)],
			collapse = "; "
		)
	} else {
		interpretation_text <- NULL
	}

	# Create full text combining all elements
	full_text_parts <- c(
		sprintf("%s: %s %s", measure_label, estimate_formatted, ci_formatted),
		sprintf("tau = %s %s", tau_formatted, tau_ci_formatted),
		if (include_prob) probability_text else NULL,
		sprintf("n = %d", bayesian_result$n_studies)
	)

	full_text <- paste(
		full_text_parts[!sapply(full_text_parts, is.null)],
		collapse = "; "
	)

	# Return formatted result list
	result <- list(
		estimate = estimate_formatted,
		ci = ci_formatted,
		tau = tau_formatted,
		tau_ci = tau_ci_formatted,
		probability = probability_text,
		interpretation = interpretation_text,
		full_text = full_text,
		# Include raw values for further use
		raw = list(
			point_estimate = point_estimate,
			ci_lower = bayesian_result$ci_95[1],
			ci_upper = bayesian_result$ci_95[2],
			tau = bayesian_result$tau_mean,
			tau_ci_lower = bayesian_result$tau_ci_95[1],
			tau_ci_upper = bayesian_result$tau_ci_95[2],
			effect_measure = effect_measure,
			n_studies = bayesian_result$n_studies,
			prob_beneficial = if (is_ratio) {
				bayesian_result$prob_negative
			} else {
				bayesian_result$prob_negative
			}
		)
	)

	class(result) <- c("iqwig_bayesian_format", class(result))

	result
}


#' Create IQWiG-Compliant Forest Plot for Bayesian Meta-Analysis
#'
#' Generates a forest plot formatted according to IQWiG guidelines with:
#' - Study weights displayed
#' - Pooled effect with credible interval
#' - Heterogeneity statistics
#' - Prediction interval (if applicable)
#' - Proper scaling and formatting
#'
#' @param bayesian_result A bayesian_meta_result object from
#'   bayesian_meta_analysis()
#' @param study_data Data frame with study data containing yi, sei, and
#'   study_labels columns. If NULL, uses metadata from bayesian_result if
#'   available.
#' @param show_weights Logical. Display study weights (default: TRUE)
#' @param show_prediction_interval Logical. Show prediction interval
#'   (default: TRUE)
#' @param digits_estimate Integer. Decimal places for estimates (default: 3)
#' @param locale Character. Locale for formatting: "en" or "de"
#' @param title Character. Plot title (default: NULL)
#' @param subtitle Character. Plot subtitle with heterogeneity info
#'   (default: auto-generated)
#' @param null_value Numeric. Reference line value (default: 1 for ratios,
#'   0 for differences)
#' @param base_size Numeric. Base font size (default: 11)
#' @param point_size Numeric. Size of study point estimates (default: 2)
#' @param ci_linewidth Numeric. Line width for CI lines (default: 0.6)
#'
#' @return A ClinicalPlot object containing the forest plot
#'
#' @examples
#' \dontrun{
#' result <- bayesian_meta_analysis(yi = yi, sei = sei, study_labels = labels)
#' study_df <- data.frame(yi = yi, sei = sei, study_labels = labels)
#' plot <- create_bayesian_forest_plot_iqwig(result, study_data = study_df)
#' print(plot)
#' }
#' @export
create_bayesian_forest_plot_iqwig <- function(
	bayesian_result,
	study_data = NULL,
	show_weights = TRUE,
	show_prediction_interval = TRUE,
	digits_estimate = 3L,
	locale = get_locale(),
	title = NULL,
	subtitle = NULL,
	null_value = NULL,
	base_size = 11,
	point_size = 2,
	ci_linewidth = 0.6
) {
	# Validate input is a bayesian_meta_result object
	if (!inherits(bayesian_result, "bayesian_meta_result")) {
		ph_abort(
			"Input must be a bayesian_meta_result object from bayesian_meta_analysis()",
			call. = FALSE
		)
	}

	# Validate locale
	if (!locale %in% c("en", "de")) {
		ph_abort("'locale' must be 'en' or 'de'", call. = FALSE)
	}

	# Extract effect measure info
	effect_measure <- bayesian_result$effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Set null value based on effect measure if not provided
	if (is.null(null_value)) {
		null_value <- if (is_ratio) 1 else 0
	}

	# Set effect measure label
	effect_label <- switch(
		effect_measure,
		"hr" = "Hazard Ratio",
		"or" = "Odds Ratio",
		"rr" = "Risk Ratio",
		"rd" = "Risk Difference",
		"md" = "Mean Difference",
		"smd" = "Standardized Mean Difference",
		"Effect"
	)

	# Get study data
	if (is.null(study_data)) {
		# Try to extract from bayesian_result if available
		if (
			!is.null(bayesian_result$fit) && requireNamespace("brms", quietly = TRUE)
		) {
			# Extract from brms fit object
			fit <- bayesian_result$fit
			data <- brms::fixef(fit, summary = FALSE)
			# This is a simplified extraction - actual implementation may vary
			study_yi <- bayesian_result$metadata$yi %||% numeric(0)
			study_sei <- bayesian_result$metadata$sei %||% numeric(0)
			study_labels <- bayesian_result$metadata$study_labels %||%
				paste("Study", seq_along(study_yi))
		} else {
			ph_abort(
				"study_data must be provided or available in bayesian_result metadata",
				call. = FALSE
			)
		}
	} else {
		# Extract from provided study_data
		study_yi <- study_data$yi
		study_sei <- study_data$sei
		study_labels <- study_data$study_labels %||%
			study_data$label %||%
			paste("Study", seq_along(study_yi))
	}

	k <- length(study_yi)

	if (k == 0) {
		ph_abort("No studies found in study_data", call. = FALSE)
	}

	# Calculate study-specific estimates and CIs
	study_ci_lower <- study_yi - 1.96 * study_sei
	study_ci_upper <- study_yi + 1.96 * study_sei

	# Transform to original scale for ratios
	if (is_ratio) {
		study_yi_plot <- exp(study_yi)
		study_ci_lower_plot <- exp(study_ci_lower)
		study_ci_upper_plot <- exp(study_ci_upper)

		pooled_estimate_plot <- bayesian_result$posterior_median
		pooled_ci_lower_plot <- exp(bayesian_result$ci_95[1])
		pooled_ci_upper_plot <- exp(bayesian_result$ci_95[2])
	} else {
		study_yi_plot <- study_yi
		study_ci_lower_plot <- study_ci_lower
		study_ci_upper_plot <- study_ci_upper

		pooled_estimate_plot <- bayesian_result$posterior_mean
		pooled_ci_lower_plot <- bayesian_result$ci_95[1]
		pooled_ci_upper_plot <- bayesian_result$ci_95[2]
	}

	# Calculate study weights (inverse variance)
	study_variance <- study_sei^2
	study_precision <- 1 / study_variance
	total_precision <- sum(study_precision)
	study_weights <- study_precision / total_precision * 100

	# Build plot data frame for studies
	plot_data <- data.frame(
		study = study_labels,
		estimate = study_yi_plot,
		ci_lower = study_ci_lower_plot,
		ci_upper = study_ci_upper_plot,
		weight = study_weights,
		is_pooled = FALSE,
		stringsAsFactors = FALSE
	)

	# Add pooled estimate row
	pooled_row <- data.frame(
		study = "Overall",
		estimate = pooled_estimate_plot,
		ci_lower = pooled_ci_lower_plot,
		ci_upper = pooled_ci_upper_plot,
		weight = 100,
		is_pooled = TRUE,
		stringsAsFactors = FALSE
	)

	plot_data <- rbind(plot_data, pooled_row)

	# Set y-axis order (studies first, pooled at bottom)
	n_studies <- nrow(plot_data) - 1
	plot_data$row_order <- seq_len(nrow(plot_data))
	plot_data$is_pooled <- c(rep(FALSE, n_studies), TRUE)
	plot_data$y_pos <- rev(seq_len(nrow(plot_data)))

	# Calculate prediction interval if requested and applicable
	pred_interval <- NULL
	if (show_prediction_interval && is_ratio) {
		# Prediction interval on log scale, then transform
		tau_estimate <- bayesian_result$tau_mean
		if (!is.na(tau_estimate) && tau_estimate > 0) {
			# Simple prediction interval approximation
			pred_log_lower <- log(pooled_estimate_plot) -
				2 * sqrt(tau_estimate^2 + mean(study_sei^2))
			pred_log_upper <- log(pooled_estimate_plot) +
				2 * sqrt(tau_estimate^2 + mean(study_sei^2))
			pred_interval <- c(exp(pred_log_lower), exp(pred_log_upper))
		}
	}

	# Calculate x-axis limits
	all_ci_bounds <- c(plot_data$ci_lower, plot_data$ci_upper)
	all_ci_bounds <- all_ci_bounds[is.finite(all_ci_bounds)]
	xlim <- range(all_ci_bounds)

	# Expand limits slightly
	if (diff(xlim) > 0) {
		xlim <- xlim + c(-1, 1) * diff(xlim) * 0.1
	} else {
		xlim <- xlim + c(-0.1, 0.1)
	}

	# Determine decimal separator for locale
	dec_sep <- if (locale == "de") "," else "."

	# Build the forest plot using ggplot2
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$estimate, y = .data$y_pos)
	)

	# Add prediction interval if available
	if (!is.null(pred_interval) && show_prediction_interval) {
		pooled_y <- plot_data$y_pos[plot_data$is_pooled]
		pred_df <- data.frame(
			x = pred_interval[1],
			xend = pred_interval[2],
			y = pooled_y - 0.35,
			yend = pooled_y - 0.35
		)
		p <- p +
			ggplot2::geom_segment(
				data = pred_df,
				ggplot2::aes(
					x = .data$x,
					xend = .data$xend,
					y = .data$y,
					yend = .data$yend
				),
				color = "gray60",
				linewidth = 0.5,
				linetype = "dotted"
			) +
			ggplot2::annotate(
				"text",
				x = mean(pred_interval),
				y = pooled_y - 0.55,
				label = "PI",
				size = base_size / 3,
				color = "gray60"
			)
	}

	# Reference line at null value
	p <- p +
		ggplot2::geom_vline(
			xintercept = null_value,
			linetype = "dashed",
			color = "gray50"
		)

	# Error bars for individual studies
	study_data_plot <- plot_data[!plot_data$is_pooled, ]
	p <- p +
		ggplot2::geom_errorbar(
			data = study_data_plot,
			ggplot2::aes(xmin = .data$ci_lower, xmax = .data$ci_upper),
			height = 0.2,
			linewidth = ci_linewidth,
			orientation = "y"
		)

	# Individual study points (squares)
	p <- p +
		ggplot2::geom_point(
			data = study_data_plot,
			ggplot2::aes(size = .data$weight),
			shape = 15,
			color = "black"
		)

	# Pooled estimate (diamond)
	pooled_data <- plot_data[plot_data$is_pooled, ]
	p <- p +
		ggplot2::geom_point(
			data = pooled_data,
			shape = 23,
			size = 5,
			fill = "black",
			color = "black"
		)

	# Y-axis with study labels
	p <- p +
		ggplot2::scale_y_continuous(
			breaks = plot_data$y_pos,
			labels = plot_data$study,
			expand = ggplot2::expansion(mult = 0.05)
		)

	# X-axis with appropriate transformation
	if (is_ratio) {
		p <- p +
			ggplot2::scale_x_log10(
				limits = xlim,
				labels = function(x) format_number(x, digits_estimate, locale)
			)
	} else {
		p <- p +
			ggplot2::scale_x_continuous(
				limits = xlim,
				labels = function(x) format_number(x, digits_estimate, locale)
			)
	}

	# Weight size scale
	p <- p +
		ggplot2::scale_size_continuous(
			range = c(1, 4),
			guide = if (show_weights) "legend" else "none"
		)

	# Labels
	p <- p +
		ggplot2::labs(
			x = sprintf("%s (95%% CI)", effect_label),
			y = NULL,
			title = title %||% sprintf("Bayesian Meta-Analysis: %s", effect_label),
			size = if (show_weights) "Weight (%)" else NULL
		)

	# Apply theme
	p <- p +
		.pharmhand_theme(base_size = base_size) +
		ggplot2::theme(
			axis.text.y = ggplot2::element_text(hjust = 0),
			panel.grid.major.y = ggplot2::element_blank(),
			legend.position = if (show_weights) "right" else "none"
		)

	# Generate subtitle with heterogeneity info if not provided
	if (is.null(subtitle)) {
		tau_formatted <- format_number(
			bayesian_result$tau_mean,
			digits_estimate,
			locale
		)
		tau_ci_fmt <- format_ci(
			bayesian_result$tau_ci_95[1],
			bayesian_result$tau_ci_95[2],
			digits = digits_estimate,
			locale = locale
		)
		subtitle <- sprintf(
			"tau = %s %s; n = %d studies",
			tau_formatted,
			tau_ci_fmt,
			k
		)
	}

	if (!is.null(subtitle)) {
		p <- p + ggplot2::labs(subtitle = subtitle)
	}

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = plot_data,
		type = "forest_bayesian",
		title = title %||% sprintf("Bayesian Meta-Analysis: %s", effect_label),
		width = 10,
		height = max(4, 2 + k * 0.4),
		dpi = 300,
		metadata = list(
			effect_measure = effect_measure,
			n_studies = k,
			tau = bayesian_result$tau_mean,
			tau_ci = bayesian_result$tau_ci_95,
			pooled_estimate = pooled_estimate_plot,
			pooled_ci = c(pooled_ci_lower_plot, pooled_ci_upper_plot),
			prediction_interval = pred_interval
		)
	)
}
