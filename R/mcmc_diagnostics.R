#' @title MCMC Diagnostics
#' @name mcmc_diagnostics
#' @description Functions for assessing MCMC convergence including trace plots,
#'   density plots, Gelman-Rubin diagnostics, and effective sample size.
NULL

# =============================================================================
# MCMC Diagnostic Plotting Functions
# =============================================================================

#' Plot MCMC Trace
#'
#' Creates trace plots for MCMC chain convergence assessment.
#'
#' @param fit A brmsfit object from brms package
#' @param parameters Character vector of parameter names to plot.
#'   If NULL, selects parameters starting with "b_", or the first 5 parameters
#'   if no "b_" parameters exist.
#' @param chains Integer vector of chain numbers to plot.
#'   If NULL, plots all chains.
#' @param title Plot title
#' @param alpha Numeric. Transparency for chain lines
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot trace for all parameters
#' plot <- plot_mcmc_trace(fit)
#'
#' # Plot trace for specific parameter
#' plot <- plot_mcmc_trace(fit, parameters = "b_Intercept")
#'
#' # Plot specific chains
#' plot <- plot_mcmc_trace(fit, chains = 1:2)
#' }
plot_mcmc_trace <- function(
	fit,
	parameters = NULL,
	chains = NULL,
	title = "MCMC Trace Plots",
	alpha = 0.8
) {
	# Validate input
	if (!inherits(fit, "brmsfit")) {
		ph_abort("'fit' must be a brmsfit object")
	}

	# Check brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_abort("Package 'brms' is required for MCMC diagnostics")
	}

	# Get parameter names if not specified
	if (is.null(parameters)) {
		parameters <- brms::variables(fit)
		# Filter out non-scalar parameters for cleaner plots
		parameters <- parameters[grep("^b_", parameters)]
		if (length(parameters) == 0) {
			parameters <- brms::variables(fit)[seq_len(min(
				5,
				length(brms::variables(fit))
			))]
		}
	}

	# Get posterior draws
	posterior_draws <- brms::as_draws_df(fit)

	# Filter to requested parameters and chains
	param_cols <- paste0(parameters, "__", "Intercept")
	param_cols <- param_cols[param_cols %in% names(posterior_draws)]

	if (length(param_cols) == 0) {
		# Try without __Intercept suffix
		param_cols <- parameters[parameters %in% names(posterior_draws)]
	}

	if (length(param_cols) == 0) {
		ph_warn("No matching parameters found for plotting")
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No parameters to plot")

		return(ClinicalPlot(
			plot = p,
			type = "mcmc_trace",
			title = title
		))
	}

	# Update parameters to match filtered param_cols
	parameters <- if (
		length(param_cols) > 0 && all(grepl("__Intercept$", param_cols))
	) {
		sub("__Intercept$", "", param_cols)
	} else {
		param_cols
	}

	# Get chain information
	if (is.null(chains)) {
		chains <- unique(posterior_draws$.chain)
	}

	# Prepare data for plotting
	# Use .iteration from draws for proper per-chain iteration scale
	plot_data <- suppressWarnings(data.frame(
		iteration = rep(posterior_draws$.iteration, length(param_cols)),
		value = unlist(posterior_draws[, param_cols]),
		parameter = rep(parameters, each = nrow(posterior_draws)),
		chain = rep(posterior_draws$.chain, length(param_cols))
	))

	# Filter to requested chains
	plot_data <- plot_data[plot_data$chain %in% chains, ]

	# Create trace plot
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(
			x = .data$iteration,
			y = .data$value,
			color = factor(.data$chain)
		)
	) +
		ggplot2::geom_line(alpha = alpha) +
		ggplot2::facet_wrap(~parameter, scales = "free_y") +
		ggplot2::labs(
			title = title,
			x = "Iteration",
			y = "Parameter Value",
			color = "Chain"
		) +
		ggplot2::scale_color_brewer(palette = "Set1") +
		ggplot2::theme_minimal() +
		ggplot2::theme(
			legend.position = "bottom",
			strip.text = ggplot2::element_text(size = 10)
		)

	ClinicalPlot(
		plot = p,
		type = "mcmc_trace",
		title = title,
		data = plot_data,
		metadata = list(
			n_parameters = length(parameters),
			n_chains = length(chains),
			n_iterations = max(posterior_draws$.iteration),
			n_samples = nrow(posterior_draws)
		)
	)
}

#' Plot MCMC Density
#'
#' Creates density plots for posterior distributions.
#'
#' @param fit A brmsfit object from brms package
#' @param parameters Character vector of parameter names to plot.
#'   If NULL, selects parameters starting with "b_", or the first 5 parameters
#'   if no "b_" parameters exist.
#' @param title Plot title
#' @param alpha Numeric. Transparency for density areas
#' @param fill_colors Character vector of colors for different chains
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot density for all parameters
#' plot <- plot_mcmc_density(fit)
#'
#' # Plot density for specific parameter
#' plot <- plot_mcmc_density(fit, parameters = "b_Intercept")
#' }
plot_mcmc_density <- function(
	fit,
	parameters = NULL,
	title = "MCMC Posterior Densities",
	alpha = 0.7,
	fill_colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
) {
	# Validate input
	if (!inherits(fit, "brmsfit")) {
		ph_abort("'fit' must be a brmsfit object")
	}

	# Check brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_abort("Package 'brms' is required for MCMC diagnostics")
	}

	# Get parameter names if not specified
	if (is.null(parameters)) {
		parameters <- brms::variables(fit)
		# Filter out non-scalar parameters for cleaner plots
		parameters <- parameters[grep("^b_", parameters)]
		if (length(parameters) == 0) {
			parameters <- brms::variables(fit)[seq_len(min(
				5,
				length(brms::variables(fit))
			))]
		}
	}

	# Get posterior draws
	posterior_draws <- brms::as_draws_df(fit)

	# Filter to requested parameters
	param_cols <- paste0(parameters, "__", "Intercept")
	param_cols <- param_cols[param_cols %in% names(posterior_draws)]

	if (length(param_cols) == 0) {
		# Try without __Intercept suffix
		param_cols <- parameters[parameters %in% names(posterior_draws)]
	}

	if (length(param_cols) == 0) {
		ph_warn("No matching parameters found for plotting")
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No parameters to plot")

		return(ClinicalPlot(
			plot = p,
			type = "mcmc_density",
			title = title
		))
	}

	# Update parameters to match filtered param_cols
	parameters <- if (
		length(param_cols) > 0 && all(grepl("__Intercept$", param_cols))
	) {
		sub("__Intercept$", "", param_cols)
	} else {
		param_cols
	}

	# Prepare data for plotting
	plot_data <- suppressWarnings(data.frame(
		value = unlist(posterior_draws[, param_cols]),
		parameter = rep(parameters, each = nrow(posterior_draws)),
		chain = rep(posterior_draws$.chain, length(param_cols))
	))

	# Create density plot
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$value, fill = factor(.data$chain))
	) +
		ggplot2::geom_density(alpha = alpha) +
		ggplot2::facet_wrap(~parameter, scales = "free") +
		ggplot2::labs(
			title = title,
			x = "Parameter Value",
			y = "Density",
			fill = "Chain"
		) +
		ggplot2::scale_fill_manual(
			values = rep(fill_colors, length.out = length(unique(plot_data$chain)))
		) +
		ggplot2::theme_minimal() +
		ggplot2::theme(
			legend.position = "bottom",
			strip.text = ggplot2::element_text(size = 10)
		)

	ClinicalPlot(
		plot = p,
		type = "mcmc_density",
		title = title,
		data = plot_data,
		metadata = list(
			n_parameters = length(parameters),
			n_chains = length(unique(plot_data$chain))
		)
	)
}

# =============================================================================
# MCMC Diagnostic Calculation Functions
# =============================================================================

#' Calculate Gelman-Rubin Diagnostic
#'
#' Calculates the Gelman-Rubin (R-hat) convergence diagnostic for MCMC chains.
#'
#' @param fit A brmsfit object from brms package
#' @param parameters Character vector or NULL (default: NULL). Subset of parameter names to compute R-hat for. If NULL, all parameters are included.
#'
#' @return Named numeric vector of R-hat values for each parameter
#' @export
#'
#' @details
#' R-hat values close to 1 indicate good convergence. Values > 1.1 suggest
#' potential convergence problems.
#'
#' @examples
#' \dontrun{
#' rhat_values <- calculate_gelman_rubin(fit)
#' print(rhat_values)
#' }
calculate_gelman_rubin <- function(fit, parameters = NULL) {
	# Validate input
	if (!inherits(fit, "brmsfit")) {
		ph_abort("'fit' must be a brmsfit object")
	}

	# Check brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_abort("Package 'brms' is required for MCMC diagnostics")
	}

	# Calculate R-hat using brms
	# Pass parameters argument if provided
	if (!is.null(parameters)) {
		rhat_values <- brms::rhat(fit, pars = parameters)
	} else {
		rhat_values <- brms::rhat(fit)
	}

	# Remove any NA values and return as named vector
	rhat_values <- rhat_values[!is.na(rhat_values)]

	# Check if any values were calculated
	if (length(rhat_values) == 0) {
		ph_warn("No R-hat values could be calculated")
		return(numeric(0))
	}

	rhat_values
}

#' Calculate Effective Sample Size
#'
#' Calculates the effective sample size (ESS) for MCMC parameters.
#'
#' @param fit A brmsfit object from brms package
#'
#' @return Named numeric vector of ESS values for each parameter
#' @export
#'
#' @details
#' ESS represents the number of independent samples that would give the same
#' precision as the correlated MCMC samples. Higher values indicate better
#' mixing of the chains.
#'
#' @examples
#' \dontrun{
#' ess_values <- calculate_effective_sample_size(fit)
#' print(ess_values)
#' }
calculate_effective_sample_size <- function(fit) {
	# Validate input
	if (!inherits(fit, "brmsfit")) {
		ph_abort("'fit' must be a brmsfit object")
	}

	# Check brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_abort("Package 'brms' is required for MCMC diagnostics")
	}

	# Calculate ESS using brms
	# Get both bulk and tail ESS
	samples <- brms::as_draws_array(fit)

	if (requireNamespace("posterior", quietly = TRUE)) {
		# Use posterior package if available for more accurate ESS
		# summarise_draws returns a tibble with ESS per variable
		draws_summary <- posterior::summarise_draws(
			samples,
			ess_bulk = posterior::ess_bulk,
			ess_tail = posterior::ess_tail
		)

		# Extract ESS values with parameter names
		eff_bulk <- draws_summary$ess_bulk
		eff_tail <- draws_summary$ess_tail
		names(eff_bulk) <- draws_summary$variable
		names(eff_tail) <- draws_summary$variable

		# Return the minimum of bulk and tail ESS for each parameter
		eff_min <- pmin(eff_bulk, eff_tail)
		names(eff_min) <- draws_summary$variable
	} else {
		# Fallback to brms implementation
		eff_bulk <- brms::neff_ratio(fit) * brms::niterations(fit)
		eff_min <- eff_bulk
	}

	# Remove any NA values
	eff_min <- eff_min[!is.na(eff_min)]

	if (length(eff_min) == 0) {
		ph_warn("No effective sample size values could be calculated")
		return(numeric(0))
	}

	eff_min
}

#' Assess MCMC Convergence
#'
#' Provides a comprehensive assessment of MCMC convergence including
#' R-hat values, effective sample sizes, and recommendations.
#'
#' @param fit A brmsfit object from brms package
#' @param rhat_threshold Numeric. R-hat threshold for convergence
#'   (default: 1.01)
#' @param ess_threshold Numeric. Minimum ESS threshold (default: 100)
#'
#' @return A list containing:
#' \describe{
#'   \item{convergence_summary}{Data frame with convergence assessment}
#'   \item{rhat_values}{Named vector of R-hat values}
#'   \item{ess_values}{Named vector of ESS values}
#'   \item{recommendations}{Character vector of recommendations}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' assessment <- assess_mcmc_convergence(fit)
#' print(assessment$convergence_summary)
#' print(assessment$recommendations)
#' }
assess_mcmc_convergence <- function(
	fit,
	rhat_threshold = 1.01,
	ess_threshold = 100
) {
	# Validate input
	if (!inherits(fit, "brmsfit")) {
		ph_abort("'fit' must be a brmsfit object")
	}

	# Validate thresholds
	if (
		!is.numeric(rhat_threshold) ||
			length(rhat_threshold) != 1 ||
			rhat_threshold <= 1
	) {
		ph_abort("'rhat_threshold' must be a single value > 1")
	}
	if (
		!is.numeric(ess_threshold) ||
			length(ess_threshold) != 1 ||
			ess_threshold <= 0
	) {
		ph_abort("'ess_threshold' must be a positive numeric value")
	}

	# Calculate diagnostics
	rhat_values <- calculate_gelman_rubin(fit)
	eff_min <- calculate_effective_sample_size(fit)

	# Create convergence summary
	if (length(rhat_values) > 0 && length(eff_min) > 0) {
		# Find common parameters
		common_params <- intersect(names(rhat_values), names(eff_min))

		if (length(common_params) > 0) {
			convergence_summary <- data.frame(
				parameter = common_params,
				rhat = rhat_values[common_params],
				eff_sample_size = round(eff_min[common_params]),
				converged = rhat_values[common_params] < rhat_threshold,
				adequate_ess = eff_min[common_params] >= ess_threshold,
				stringsAsFactors = FALSE
			)

			convergence_summary$overall_status <- ifelse(
				convergence_summary$converged & convergence_summary$adequate_ess,
				"Good",
				ifelse(
					convergence_summary$converged,
					"Marginal",
					"Poor"
				)
			)
		} else {
			convergence_summary <- data.frame()
		}
	} else {
		convergence_summary <- data.frame()
	}

	# Generate recommendations
	recommendations <- c()

	if (nrow(convergence_summary) > 0) {
		n_poor <- sum(convergence_summary$overall_status == "Poor")
		n_marginal <- sum(convergence_summary$overall_status == "Marginal")

		if (n_poor > 0) {
			recommendations <- c(
				recommendations,
				sprintf(
					paste(
						"%d parameters show poor convergence.",
						"Consider increasing warmup iterations or number of chains."
					),
					n_poor
				)
			)
		}

		if (n_marginal > 0) {
			recommendations <- c(
				recommendations,
				sprintf(
					paste(
						"%d parameters show marginal convergence.",
						"Monitor these parameters carefully."
					),
					n_marginal
				)
			)
		}

		n_low_ess <- sum(!convergence_summary$adequate_ess)
		if (n_low_ess > 0) {
			recommendations <- c(
				recommendations,
				sprintf(
					"%d parameters have ESS < %d. Consider running more iterations.",
					n_low_ess,
					ess_threshold
				)
			)
		}

		if (n_poor == 0 && n_marginal == 0) {
			recommendations <- c(
				recommendations,
				"All parameters show good convergence and adequate ESS."
			)
		}
	} else {
		recommendations <- c(
			recommendations,
			"Could not calculate convergence diagnostics."
		)
	}

	# Combine recommendations into single string
	recommendations_text <- paste(recommendations, collapse = "\n")

	list(
		convergence_summary = convergence_summary,
		rhat_values = rhat_values,
		ess_values = eff_min,
		recommendations = recommendations_text
	)
}

# =============================================================================
# Comprehensive MCMC Diagnostics Report
# =============================================================================

#' Create MCMC Diagnostics Report
#'
#' Generates a comprehensive MCMC diagnostics report including trace plots,
#' density plots, convergence assessment, and summary statistics.
#'
#' @param fit A brmsfit object from brms package
#' @param parameters Character vector of specific parameters to analyze.
#'   If NULL, selects parameters starting with "b_", or the first 5 parameters
#'   if no "b_" parameters exist.
#' @param rhat_threshold Numeric. R-hat threshold for convergence
#' @param ess_threshold Numeric. Minimum ESS threshold
#' @param title_prefix Character string for report section titles
#'
#' @return A list containing:
#' \describe{
#'   \item{trace_plots}{List of trace plot ClinicalPlot objects}
#'   \item{density_plots}{List of density plot ClinicalPlot objects}
#'   \item{convergence_assessment}{Convergence assessment results}
#'   \item{diagnostic_summary}{Data frame with key diagnostics}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' report <- create_mcmc_diagnostics_report(fit)
#' report$trace_plots[[1]]  # Access first trace plot
#' report$convergence_assessment$recommendations
#' }
create_mcmc_diagnostics_report <- function(
	fit,
	parameters = NULL,
	rhat_threshold = 1.01,
	ess_threshold = 100,
	title_prefix = "MCMC Diagnostics"
) {
	# Validate input
	if (!inherits(fit, "brmsfit")) {
		ph_abort("'fit' must be a brmsfit object")
	}

	# Check brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_abort(
			"Package 'brms' is required for MCMC diagnostics. ",
			"Install with: install.packages('brms')"
		)
	}

	# Get parameter names if not specified
	if (is.null(parameters)) {
		parameters <- brms::variables(fit)
		# Focus on main parameters for cleaner output
		parameters <- parameters[grep("^b_", parameters)]
		if (length(parameters) == 0) {
			parameters <- brms::variables(fit)[seq_len(min(
				5,
				length(brms::variables(fit))
			))]
		}
	}

	# Create trace plots
	trace_plot <- plot_mcmc_trace(
		fit,
		parameters = parameters,
		title = sprintf("%s: Trace Plots", title_prefix)
	)

	# Create density plots
	density_plot <- plot_mcmc_density(
		fit,
		parameters = parameters,
		title = sprintf("%s: Posterior Densities", title_prefix)
	)

	# Perform convergence assessment
	convergence <- assess_mcmc_convergence(
		fit,
		rhat_threshold = rhat_threshold,
		ess_threshold = ess_threshold
	)

	# Create diagnostic summary
	rhat_vals <- convergence$rhat_values
	eff_vals <- convergence$ess_values

	if (length(rhat_vals) > 0 && length(eff_vals) > 0) {
		# Compute aligned parameter names
		common_names <- intersect(names(rhat_vals), names(eff_vals))

		# Further filter to requested parameters if specified
		if (!is.null(parameters)) {
			common_names <- intersect(common_names, parameters)
		}

		if (length(common_names) > 0) {
			# Subset both vectors by common names for alignment
			rhat_vals <- rhat_vals[common_names]
			eff_vals <- eff_vals[common_names]

			diagnostic_summary <- data.frame(
				parameter = common_names,
				rhat = round(rhat_vals, 3),
				eff_sample_size = round(eff_vals),
				convergence_status = ifelse(
					rhat_vals < rhat_threshold,
					"Good",
					"Poor"
				),
				ess_status = ifelse(
					eff_vals >= ess_threshold,
					"Adequate",
					"Low"
				),
				stringsAsFactors = FALSE
			)
		} else {
			diagnostic_summary <- data.frame(
				parameter = character(),
				rhat = numeric(),
				eff_sample_size = numeric(),
				convergence_status = character(),
				ess_status = character(),
				stringsAsFactors = FALSE
			)
		}
	} else {
		diagnostic_summary <- data.frame(
			parameter = character(),
			rhat = numeric(),
			eff_sample_size = numeric(),
			convergence_status = character(),
			ess_status = character(),
			stringsAsFactors = FALSE
		)
	}

	list(
		trace_plots = list(trace_plot),
		density_plots = list(density_plot),
		convergence_assessment = convergence,
		diagnostic_summary = diagnostic_summary
	)
}
