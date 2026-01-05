#' @title Bayesian Meta-Analysis Functions
#' @name meta_bayesian
#' @description Interface for Bayesian meta-analysis using brms/rstan.
NULL

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
#' @param chains Integer. Number of MCMC chains. Default: 4
#' @param iter Integer. Total iterations per chain. Default: 4000
#' @param warmup Integer. Warmup iterations. Default: 2000
#' @param seed Integer. Random seed
#' @param ... Additional arguments passed to brms::brm
#'
#' @return A list containing:
#'   \item{estimate}{Posterior mean of overall effect}
#'   \item{ci}{Credible interval (2.5%, 97.5%)}
#'   \item{tau}{Posterior mean of heterogeneity SD}
#'   \item{tau_ci}{Credible interval for tau}
#'   \item{k}{Number of studies}
#'   \item{effect_measure}{Effect measure used}
#'   \item{model}{Model type ("bayesian")}
#'   \item{fit}{Full brms fit object (when brms available)}
#'
#'   If brms is not installed, returns a list with installation guidance.
#' @export
bayesian_meta_analysis <- function(
	yi,
	sei,
	study_labels = NULL,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	prior_mu = list(mean = 0, sd = 10),
	prior_tau = list(type = "half_cauchy", scale = 0.5),
	chains = 4,
	iter = 4000,
	warmup = 2000,
	seed = NULL,
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
	priors <- c(
		brms::prior_string(
			sprintf("normal(%f, %f)", prior_mu$mean, prior_mu$sd),
			class = "Intercept"
		),
		brms::prior_string(
			sprintf(
				"%s(0, %f)",
				if (prior_tau$type == "half_cauchy") "cauchy" else "normal",
				prior_tau$scale
			),
			class = "sd",
			lb = if (prior_tau$type == "half_cauchy") 0 else NULL
		)
	)

	# Fit model
	fit <- brms::brm(
		formula = formula,
		data = data,
		prior = priors,
		chains = chains,
		iter = iter,
		warmup = warmup,
		seed = seed,
		refresh = 0, # Suppress iteration output
		...
	)

	# Extract posteriors
	posterior_samples <- brms::as_draws_df(fit)

	# Summarize
	mu_samples <- posterior_samples$b_Intercept
	tau_samples <- posterior_samples$sd_study__Intercept

	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	if (is_ratio) {
		mu_display <- exp(mu_samples)
	} else {
		mu_display <- mu_samples
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

	class(result) <- c("bayesian_meta_result", class(result))
	result
}
