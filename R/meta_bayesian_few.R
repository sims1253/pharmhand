#' @title Bayesian Meta-Analysis for Few Studies
#' @name meta_bayesian_few
#' @description Specialized Bayesian meta-analysis functions for situations
#'   with few studies, using appropriate priors and sensitivity analysis.
#' @references IQWiG General Methods 10.3.7 p.224-227
NULL

# =============================================================================
# BayesianMetaFewResult S7 Class
# =============================================================================

#' BayesianMetaFewResult Class
#'
#' An S7 class for storing Bayesian meta-analysis results for few studies.
#'
#' @export
#'
#' @param posterior_summary Data frame with posterior summary statistics
#' @param credible_intervals Data frame with credible intervals
#' @param prior_summary Data frame with prior specification summary
#' @param few_studies_adjustment Character. Description of adjustments
#'   made for few studies
#' @param prob_positive Numeric. Posterior probability that effect is positive
#' @param prob_negative Numeric. Posterior probability that effect is negative
#' @param heterogeneity List with heterogeneity statistics (tau2, I2, etc.)
#' @param tau_summary Data frame with between-study variance summary
#' @param prior_sensitivity List with prior sensitivity analysis results
#' @param model The brms model object
#' @param n_studies Integer number of studies
#' @param effect_measure Character string for effect measure type
#' @param metadata List of additional metadata
#'
#' @return A BayesianMetaFewResult object
BayesianMetaFewResult <- S7::new_class(
	"BayesianMetaFewResult",
	package = "pharmhand",
	properties = list(
		posterior_summary = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("posterior_summary must be a data frame")
				}
				NULL
			}
		),
		credible_intervals = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("credible_intervals must be a data frame")
				}
				NULL
			}
		),
		prior_summary = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("prior_summary must be a data frame")
				}
				NULL
			}
		),
		few_studies_adjustment = S7::new_property(
			S7::class_character,
			default = ""
		),
		prob_positive = S7::new_property(
			S7::class_numeric,
			default = 0.5,
			validator = function(value) {
				if (length(value) != 1 || value < 0 || value > 1) {
					return("prob_positive must be a single value between 0 and 1")
				}
				NULL
			}
		),
		prob_negative = S7::new_property(
			S7::class_numeric,
			default = 0.5,
			validator = function(value) {
				if (length(value) != 1 || value < 0 || value > 1) {
					return("prob_negative must be a single value between 0 and 1")
				}
				NULL
			}
		),
		heterogeneity = S7::new_property(S7::class_list, default = list()),
		tau_summary = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("tau_summary must be a data frame")
				}
				NULL
			}
		),
		prior_sensitivity = S7::new_property(S7::class_list, default = list()),
		model = S7::new_property(S7::class_any),
		n_studies = S7::new_property(
			S7::class_integer,
			validator = function(value) {
				if (length(value) != 1 || value < 1) {
					return("n_studies must be a positive integer")
				}
				NULL
			}
		),
		effect_measure = S7::new_property(
			S7::class_character,
			default = "hr",
			validator = function(value) {
				valid_measures <- c("hr", "or", "rr", "rd", "md", "smd")
				if (!value %in% valid_measures) {
					return(sprintf(
						"effect_measure must be one of: %s",
						paste(valid_measures, collapse = ", ")
					))
				}
				NULL
			}
		),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

# =============================================================================
# Bayesian Meta-Analysis for Few Studies Functions
# =============================================================================

#' Bayesian Meta-Analysis for Few Studies
#'
#' Specialized Bayesian meta-analysis for situations with few studies (typically
#' < 5-10 studies). Uses conservative priors and includes sensitivity analysis.
#'
#' @param yi Numeric vector of effect estimates
#' @param sei Numeric vector of standard errors
#' @param study_labels Character vector of study names
#' @param effect_measure Character. Effect type: "hr" (hazard ratio),
#'   "or" (odds ratio), "rr" (risk ratio), "rd" (risk difference),
#'   "md" (mean difference), "smd" (standardized mean difference)
#' @param prior_mu Prior for overall effect mean (default: 0, very wide)
#' @param prior_tau Prior for heterogeneity. For few studies, uses more
#'   informative priors that pull tau towards smaller values
#' @param prior_sensitivity Logical. Whether to perform prior
#'   sensitivity analysis
#' @param chains Integer. Number of MCMC chains (default: 4)
#' @param iter Integer. Total iterations per chain (default: 6000)
#' @param warmup Integer. Warmup iterations (default: 3000)
#' @param seed Integer. Random seed for reproducibility
#' @param adapt_delta Numeric. MCMC sampler tuning parameter (0-1)
#' @param max_treedepth Integer. Maximum tree depth for NUTS sampler
#' @param backend Character. Stan backend: "auto", "cmdstanr", "rstan"
#' @param ... Additional arguments passed to brms::brm
#'
#' @return A BayesianMetaFewResult object
#'
#' @details
#' This function is specifically designed for meta-analyses with few studies
#' and implements several adjustments:
#'
#' 1. **Conservative priors**: Uses more informative priors that are appropriate
#'    when data is sparse
#' 2. **Regularization**: Stronger regularization of between-study variance
#'    (tau^2)
#' 3. **Sensitivity analysis**: Includes prior sensitivity analysis by default
#' 4. **Few studies warnings**: Provides explicit warnings about limitations
#'
#' Recommended for meta-analyses with 2-10 studies. For larger meta-analyses,
#' use the standard bayesian_meta_analysis() function.
#'
#' @references
#' Spiegelhalter, D.J. et al. (2004). Bayesian approaches to clinical trials
#' and health-care evaluation. Wiley.
#'
#' @importFrom brms brm bf prior_string as_draws_df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Meta-analysis with only 3 studies
#' yi <- log(c(0.75, 0.82, 0.68))  # log(HR) from 3 studies
#' sei <- c(0.12, 0.15, 0.18)       # standard errors
#'
#' result <- bayesian_meta_analysis_few(
#'   yi = yi,
#'   sei = sei,
#'   effect_measure = "hr",
#'   chains = 2,
#'   iter = 4000
#' )
#'
#' # With prior sensitivity analysis
#' result <- bayesian_meta_analysis_few(
#'   yi = yi,
#'   sei = sei,
#'   effect_measure = "hr",
#'   prior_sensitivity = TRUE,
#'   chains = 2,
#'   iter = 4000
#' )
#' }
bayesian_meta_analysis_few <- function(
	yi,
	sei,
	study_labels = NULL,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	prior_mu = list(mean = 0, sd = 2),
	prior_tau = list(type = "half_cauchy", scale = 0.25),
	prior_sensitivity = TRUE,
	chains = 4,
	iter = 6000,
	warmup = 3000,
	seed = NULL,
	adapt_delta = 0.99,
	max_treedepth = 15,
	backend = c("auto", "cmdstanr", "rstan"),
	...
) {
	effect_measure <- match.arg(effect_measure)
	backend <- match.arg(backend)
	k <- as.integer(length(yi))

	# Check for brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_abort(
			"Bayesian meta-analysis for few studies requires the 'brms' package. ",
			"Install with: install.packages('brms')"
		)
	}

	# Validate inputs
	if (k < 2) {
		ph_abort("At least 2 studies are required for meta-analysis")
	}

	if (k <= 3) {
		ph_warn(
			"Very few studies (",
			k,
			") for meta-analysis. Bayesian ",
			"meta-analysis for few studies uses specialized priors but ",
			"results should still be interpreted cautiously."
		)
	}

	# Generate study labels if not provided
	if (is.null(study_labels)) {
		study_labels <- paste("Study", seq_len(k))
	}

	# Validate priors for few studies
	if (prior_mu$sd > 5) {
		ph_warn(
			"Using very wide prior for overall effect (sd = ",
			prior_mu$sd,
			"). Consider using more informative prior for few studies."
		)
	}

	# For few studies, use more informative prior for tau
	# Default is already more conservative (scale = 0.25 vs 0.5)
	if (prior_tau$scale > 0.5) {
		ph_warn(
			"Using wide prior for heterogeneity (scale = ",
			prior_tau$scale,
			"). For few studies, consider scale <= 0.25."
		)
	}

	# Prepare data
	data <- data.frame(
		yi = yi,
		sei = sei,
		study = study_labels
	)

	# Determine Stan backend
	cmdstanr_ok <- FALSE
	if (
		backend %in%
			c("auto", "cmdstanr") &&
			requireNamespace("cmdstanr", quietly = TRUE)
	) {
		cmdstanr_ok <- tryCatch(
			!is.na(cmdstanr::cmdstan_version()),
			error = function(e) FALSE
		)
	}

	brms_backend <- if (backend == "rstan") {
		"rstan"
	} else if (cmdstanr_ok) {
		"cmdstanr"
	} else {
		"rstan"
	}

	# Build brms formula for few studies
	# Use more conservative priors
	formula <- brms::bf(yi | se(sei) ~ 1 + (1 | study))

	# Build priors - more conservative for few studies
	tau_prior_str <- switch(
		prior_tau$type,
		"half_cauchy" = sprintf("cauchy(0, %f)", prior_tau$scale),
		"half_normal" = sprintf("normal(0, %f)", prior_tau$scale),
		"exponential" = sprintf("exponential(%f)", 1 / prior_tau$scale),
		stop(sprintf(
			paste0(
				"Unknown tau prior type for tau_prior_str: '%s'. ",
				"Expected one of: half_cauchy, half_normal, exponential"
			),
			prior_tau$type
		))
	)

	priors <- c(
		brms::prior_string(
			sprintf("normal(%f, %f)", prior_mu$mean, prior_mu$sd),
			class = "Intercept"
		),
		brms::prior_string(tau_prior_str, class = "sd", lb = 0)
	)

	# Build brms args - only include seed if provided
	brms_args <- list(
		formula = formula,
		data = data,
		prior = priors,
		chains = chains,
		iter = iter,
		warmup = warmup,
		refresh = 0,
		control = list(
			adapt_delta = adapt_delta,
			max_treedepth = max_treedepth
		),
		backend = brms_backend
	)

	# Only add seed if not NULL
	if (!is.null(seed)) {
		brms_args$seed <- seed
	}

	# Add any additional arguments
	brms_args <- c(brms_args, list(...))

	# Run model with conservative settings for few studies
	fit <- tryCatch(
		{
			do.call(brms::brm, brms_args)
		},
		error = function(e) {
			ph_abort(sprintf(
				"Bayesian meta-analysis for few studies failed. Stan/brms error: %s",
				conditionMessage(e)
			))
		}
	)

	# Extract posteriors
	posterior_samples <- brms::as_draws_df(fit)

	# Calculate posterior summary
	intercept_samples <- posterior_samples$b_Intercept
	sd_samples <- posterior_samples$sd_study__Intercept

	# Posterior summary
	post_summary <- data.frame(
		parameter = c("Overall Effect", "Between-study SD (tau)"),
		mean = c(mean(intercept_samples), mean(sd_samples)),
		median = c(median(intercept_samples), median(sd_samples)),
		sd = c(sd(intercept_samples), sd(sd_samples)),
		stringsAsFactors = FALSE
	)

	# Credible intervals
	ci_level <- 0.95
	alpha <- 1 - ci_level
	ci_lower <- c(
		quantile(intercept_samples, alpha / 2),
		quantile(sd_samples, alpha / 2)
	)
	ci_upper <- c(
		quantile(intercept_samples, 1 - alpha / 2),
		quantile(sd_samples, 1 - alpha / 2)
	)

	cred_intervals <- data.frame(
		parameter = post_summary$parameter,
		ci_lower = ci_lower,
		ci_upper = ci_upper,
		stringsAsFactors = FALSE
	)

	# Prior summary
	prior_summary <- data.frame(
		parameter = c("Overall Effect", "Between-study SD (tau)"),
		prior_distribution = c(
			sprintf("Normal(%.1f, %.1f)", prior_mu$mean, prior_mu$sd),
			sprintf("%s(%.2f)", prior_tau$type, prior_tau$scale)
		),
		stringsAsFactors = FALSE
	)

	# Probabilities
	prob_positive <- mean(intercept_samples > 0)
	prob_negative <- mean(intercept_samples < 0)

	# Heterogeneity assessment
	# Compute typical within-study variance using Higgins & Thompson formula
	k_sei <- as.integer(length(sei))
	w <- 1 / sei^2
	vtilde <- ((k_sei - 1) * sum(w)) / ((sum(w)^2) - sum(w^2))
	i2_samples <- sd_samples^2 / (sd_samples^2 + vtilde)
	heterogeneity <- list(
		tau2_mean = mean(sd_samples^2),
		tau2_median = median(sd_samples^2),
		tau2_ci = quantile(sd_samples^2, c(alpha / 2, 1 - alpha / 2)),
		i2_mean = mean(i2_samples),
		i2_median = median(i2_samples),
		i2_ci = quantile(i2_samples, c(alpha / 2, 1 - alpha / 2))
	)

	# Tau summary
	tau_summary <- data.frame(
		parameter = "Between-study SD (tau)",
		mean = mean(sd_samples),
		median = median(sd_samples),
		sd = sd(sd_samples),
		ci_lower = quantile(sd_samples, alpha / 2),
		ci_upper = quantile(sd_samples, 1 - alpha / 2),
		stringsAsFactors = FALSE
	)

	# Prior sensitivity analysis
	prior_sensitivity_results <- list()
	if (prior_sensitivity) {
		ph_inform("Performing prior sensitivity analysis for few studies...")

		# Test different priors for tau
		test_priors <- list(
			list(type = "half_cauchy", scale = 0.1),
			list(type = "half_cauchy", scale = 0.5),
			list(type = "half_normal", scale = 0.2),
			list(type = "exponential", scale = 1)
		)

		prior_sens_results <- list()
		for (i in seq_along(test_priors)) {
			prior_test <- test_priors[[i]]

			# Fit model with alternative prior
			tau_prior_test <- switch(
				prior_test$type,
				"half_cauchy" = sprintf("cauchy(0, %f)", prior_test$scale),
				"half_normal" = sprintf("normal(0, %f)", prior_test$scale),
				"exponential" = sprintf("exponential(%f)", 1 / prior_test$scale),
				stop(sprintf(
					paste0(
						"Unknown tau prior type for tau_prior_test: '%s'. ",
						"Expected one of: half_cauchy, half_normal, exponential"
					),
					prior_test$type
				))
			)

			priors_test <- c(
				brms::prior_string(
					sprintf("normal(%.1f, %.1f)", prior_mu$mean, prior_mu$sd),
					class = "Intercept"
				),
				brms::prior_string(tau_prior_test, class = "sd", lb = 0)
			)

			result <- tryCatch(
				{
					fit_test <- brms::brm(
						formula = formula,
						data = data,
						prior = priors_test,
						chains = min(2, chains),
						iter = max(2000, iter / 2),
						warmup = max(1000, warmup / 2),
						seed = seed,
						refresh = 0,
						control = list(
							adapt_delta = adapt_delta,
							max_treedepth = max_treedepth
						),
						backend = brms_backend
					)

					samples_test <- brms::as_draws_df(fit_test)
					intercept_test <- samples_test$b_Intercept

					list(
						prior_type = prior_test$type,
						prior_scale = prior_test$scale,
						effect_mean = mean(intercept_test),
						effect_median = median(intercept_test),
						effect_sd = sd(intercept_test),
						effect_ci = quantile(intercept_test, c(alpha / 2, 1 - alpha / 2))
					)
				},
				error = function(e) {
					list(
						prior_type = prior_test$type,
						prior_scale = prior_test$scale,
						error = conditionMessage(e)
					)
				}
			)

			prior_sens_results[[i]] <- result
		}

		prior_sensitivity_results <- prior_sens_results
	}

	# Few studies adjustment description
	few_studies_desc <- sprintf(
		paste(
			"Analysis adjusted for few studies (n=%d):",
			"Used conservative priors (mu ~ N(%.1f, %.1f), tau ~ %s(%.2f)),",
			"increased iterations (%d), and included prior sensitivity analysis."
		),
		k,
		prior_mu$mean,
		prior_mu$sd,
		prior_tau$type,
		prior_tau$scale,
		iter
	)

	BayesianMetaFewResult(
		posterior_summary = post_summary,
		credible_intervals = cred_intervals,
		prior_summary = prior_summary,
		few_studies_adjustment = few_studies_desc,
		prob_positive = prob_positive,
		prob_negative = prob_negative,
		heterogeneity = heterogeneity,
		tau_summary = tau_summary,
		prior_sensitivity = prior_sensitivity_results,
		model = fit,
		n_studies = k,
		effect_measure = effect_measure,
		metadata = list(
			chains = chains,
			iter = iter,
			warmup = warmup,
			seed = seed,
			adapt_delta = adapt_delta,
			max_treedepth = max_treedepth,
			backend = brms_backend,
			prior_sensitivity = prior_sensitivity
		)
	)
}

#' Summarize Bayesian Meta-Analysis for Few Studies
#'
#' Creates a summary table from Bayesian meta-analysis results for few studies.
#'
#' @param result A BayesianMetaFewResult object
#' @param digits Integer. Number of decimal places to display
#'
#' @return A list containing a "posterior" data frame with summary statistics
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- summary_bayesian_few(result)
#' print(summary)
#' }
summary_bayesian_few <- function(result, digits = 3) {
	if (!S7::S7_inherits(result, BayesianMetaFewResult)) {
		ph_abort("'result' must be a BayesianMetaFewResult object")
	}

	# Combine posterior and credible interval information
	summary_df <- result@posterior_summary
	summary_df$ci_lower <- result@credible_intervals$ci_lower
	summary_df$ci_upper <- result@credible_intervals$ci_upper

	# Round numeric columns
	numeric_cols <- c("mean", "median", "sd", "ci_lower", "ci_upper")
	summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], round, digits)

	# Format CI
	summary_df$`95% CrI` <- sprintf(
		"%s (%s, %s)",
		summary_df$median,
		summary_df$ci_lower,
		summary_df$ci_upper
	)

	# Select and rename columns
	final_summary <- summary_df[, c(
		"parameter",
		"mean",
		"median",
		"sd",
		"95% CrI"
	)]
	colnames(final_summary) <- c(
		"Parameter",
		"Posterior Mean",
		"Posterior Median",
		"Posterior SD",
		"95% Credible Interval"
	)

	list(posterior = final_summary)
}

#' Create Bayesian Meta-Analysis Table for Few Studies
#'
#' Creates a clinical table from Bayesian meta-analysis results for few studies.
#'
#' @param result A BayesianMetaFewResult object
#' @param title Table title
#' @param subtitle Optional subtitle
#' @param footnotes Optional footnotes
#' @param autofit Logical. Whether to autofit column widths
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' table <- create_bayesian_few_table(
#'   result, title = "Bayesian Meta-Analysis (Few Studies)"
#' )
#' }
create_bayesian_few_table <- function(
	result,
	title = "Bayesian Meta-Analysis for Few Studies",
	subtitle = NULL,
	footnotes = NULL,
	autofit = TRUE
) {
	if (!S7::S7_inherits(result, BayesianMetaFewResult)) {
		ph_abort("'result' must be a BayesianMetaFewResult object")
	}

	# Get summary statistics
	summary_list <- summary_bayesian_few(result)
	summary_df <- summary_list$posterior

	# Add probabilities
	prob_row <- data.frame(
		Parameter = "P(Effect > 0)",
		`Posterior Mean` = sprintf("%.3f", result@prob_positive),
		`Posterior Median` = "",
		`Posterior SD` = "",
		`95% Credible Interval` = "",
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	# Rename for consistency
	colnames(prob_row)[5] <- "95% Credible Interval"

	summary_df <- rbind(summary_df, prob_row)

	# Add heterogeneity if available
	if (length(result@heterogeneity) > 0) {
		het_row <- data.frame(
			Parameter = "Between-study variance (tau^2)",
			`Posterior Mean` = sprintf("%.3f", result@heterogeneity$tau2_mean),
			`Posterior Median` = sprintf("%.3f", result@heterogeneity$tau2_median),
			`Posterior SD` = "",
			`95% Credible Interval` = sprintf(
				"(%.3f, %.3f)",
				result@heterogeneity$tau2_ci[1],
				result@heterogeneity$tau2_ci[2]
			),
			stringsAsFactors = FALSE,
			check.names = FALSE
		)

		# Rename for consistency
		colnames(het_row)[5] <- "95% Credible Interval"
		summary_df <- rbind(summary_df, het_row)
	}

	# Add metadata
	meta_footnotes <- c(
		result@few_studies_adjustment,
		paste("Studies:", result@n_studies),
		paste("Effect measure:", result@effect_measure),
		paste("P(Effect < 0):", sprintf("%.3f", result@prob_negative)),
		if (!is.null(footnotes)) footnotes
	)

	create_clinical_table(
		data = summary_df,
		type = "bayesian_few",
		title = title,
		footnotes = meta_footnotes,
		autofit = autofit
	)
}

#' Plot Bayesian Meta-Analysis Results for Few Studies
#'
#' Creates forest plot for Bayesian meta-analysis results for few studies.
#'
#' @param result A BayesianMetaFewResult object
#' @param title Plot title
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_bayesian_few(
#'   result, title = "Bayesian Meta-Analysis (Few Studies)"
#' )
#' }
plot_bayesian_few <- function(
	result,
	title = "Bayesian Meta-Analysis Results (Few Studies)"
) {
	if (!S7::S7_inherits(result, BayesianMetaFewResult)) {
		ph_abort("'result' must be a BayesianMetaFewResult object")
	}

	# Extract posterior summary for plotting
	post_summary <- result@posterior_summary

	if (nrow(post_summary) == 0) {
		ph_warn("No posterior summary data available for plotting")
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No data available")

		return(ClinicalPlot(
			plot = p,
			type = "bayesian_few_plot",
			title = title
		))
	}

	# Focus on the overall effect
	effect_row <- post_summary[post_summary$parameter == "Overall Effect", ]

	if (nrow(effect_row) == 0) {
		ph_warn("No overall effect data available for plotting")
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No overall effect data")

		return(ClinicalPlot(
			plot = p,
			type = "bayesian_few_plot",
			title = title
		))
	}

	# Create simple forest plot
	plot_data <- data.frame(
		estimate = effect_row$median,
		ci_lower = result@credible_intervals$ci_lower[1],
		ci_upper = result@credible_intervals$ci_upper[1],
		study = "Overall Effect",
		stringsAsFactors = FALSE
	)

	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$estimate, y = .data$study)
	) +
		ggplot2::geom_point(size = 3) +
		ggplot2::geom_errorbarh(
			ggplot2::aes(xmin = .data$ci_lower, xmax = .data$ci_upper),
			height = 0.2
		) +
		ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
		ggplot2::labs(
			title = title,
			x = sprintf("Effect Estimate (%s)", result@effect_measure),
			y = ""
		) +
		ggplot2::theme_minimal()

	ClinicalPlot(
		plot = p,
		type = "bayesian_few_plot",
		title = title,
		data = plot_data,
		metadata = list(
			n_studies = result@n_studies,
			effect_measure = result@effect_measure,
			prob_positive = result@prob_positive,
			prob_negative = result@prob_negative
		)
	)
}
