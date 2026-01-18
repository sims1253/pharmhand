#' @title Imputation Diagnostic Plots
#' @name imputation_diagnostics
#' @description Diagnostic visualization functions for multiple imputation.
NULL

#' Plot Imputation Convergence
#'
#' Creates trace plots showing MCMC convergence of the imputation algorithm.
#'
#' @param imputation_result An ImputationResult object
#' @param vars Character vector of variables to plot. If NULL, plots all
#'   imputed variables.
#' @param title Plot title
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' imp <- perform_multiple_imputation(data, m = 5)
#' plot_imputation_convergence(imp)
#' }
plot_imputation_convergence <- function(
	imputation_result,
	vars = NULL,
	title = "Imputation Convergence"
) {
	if (!S7::S7_inherits(imputation_result, ImputationResult)) {
		ph_abort("'imputation_result' must be an ImputationResult object")
	}

	if (!requireNamespace("mice", quietly = TRUE)) {
		ph_abort("Package 'mice' is required")
	}

	mice_obj <- imputation_result@mice_object

	# Select variables
	if (is.null(vars)) {
		vars <- imputation_result@imputed_vars
	}

	# Extract convergence data from mice object
	# mice stores mean and sd per iteration per variable per imputation
	plot_data <- mice::complete(mice_obj, "long", include = TRUE)

	# Create convergence plot using ggplot2
	# Plot mean of imputed values across iterations
	iter_data <- data.frame(
		iteration = seq_len(mice_obj$iteration),
		stringsAsFactors = FALSE
	)

	# Get chainMean from mice object for convergence diagnostics
	chain_mean <- mice_obj$chainMean
	chain_var <- mice_obj$chainVar

	# Reshape for plotting
	plot_list <- list()
	for (v in vars) {
		if (v %in% dimnames(chain_mean)[[1]]) {
			mean_data <- as.data.frame(t(chain_mean[v, , ]))
			mean_data$iteration <- seq_len(nrow(mean_data))
			mean_long <- tidyr::pivot_longer(
				mean_data,
				cols = -"iteration",
				names_to = "chain",
				values_to = "mean"
			)
			mean_long$variable <- v
			plot_list[[v]] <- mean_long
		}
	}

	if (length(plot_list) == 0) {
		ph_warn("No convergence data available for selected variables")
		# Return empty plot
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No convergence data")
	} else {
		plot_df <- do.call(rbind, plot_list)

		p <- ggplot2::ggplot(
			plot_df,
			ggplot2::aes(
				x = .data$iteration,
				y = .data$mean,
				color = .data$chain
			)
		) +
			ggplot2::geom_line() +
			ggplot2::facet_wrap(~variable, scales = "free_y") +
			ggplot2::labs(
				title = title,
				x = "Iteration",
				y = "Mean of Imputed Values",
				color = "Chain"
			) +
			ggplot2::theme_minimal()
	}

	ClinicalPlot(
		plot = p,
		type = "imputation_convergence",
		title = title,
		metadata = list(
			vars = vars,
			m = imputation_result@m
		)
	)
}

#' Plot Imputation Distributions
#'
#' Compares distributions of observed vs imputed values.
#'
#' @param imputation_result An ImputationResult object
#' @param vars Character vector of variables to plot. If NULL, plots all
#'   imputed numeric variables.
#' @param title Plot title
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' imp <- perform_multiple_imputation(data, m = 5)
#' plot_imputation_distributions(imp)
#' }
plot_imputation_distributions <- function(
	imputation_result,
	vars = NULL,
	title = "Observed vs Imputed Distributions"
) {
	if (!S7::S7_inherits(imputation_result, ImputationResult)) {
		ph_abort("'imputation_result' must be an ImputationResult object")
	}

	if (!requireNamespace("mice", quietly = TRUE)) {
		ph_abort("Package 'mice' is required")
	}

	mice_obj <- imputation_result@mice_object
	original_data <- imputation_result@original_data

	# Select variables (only numeric)
	if (is.null(vars)) {
		vars <- imputation_result@imputed_vars
		vars <- vars[vapply(original_data[vars], is.numeric, logical(1))]
	}

	if (length(vars) == 0) {
		ph_warn("No numeric imputed variables to plot")
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No numeric variables")

		return(ClinicalPlot(
			plot = p,
			type = "imputation_distributions",
			title = title
		))
	}

	# Get completed data (first imputation)
	completed <- mice::complete(mice_obj, action = 1)

	# Build comparison data
	plot_list <- list()
	for (v in vars) {
		observed_idx <- !is.na(original_data[[v]])
		imputed_idx <- is.na(original_data[[v]])

		observed_vals <- original_data[[v]][observed_idx]
		imputed_vals <- completed[[v]][imputed_idx]

		if (length(imputed_vals) > 0) {
			df <- data.frame(
				value = c(observed_vals, imputed_vals),
				type = c(
					rep("Observed", length(observed_vals)),
					rep("Imputed", length(imputed_vals))
				),
				variable = v,
				stringsAsFactors = FALSE
			)
			plot_list[[v]] <- df
		}
	}

	if (length(plot_list) == 0) {
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No imputed values to compare")
	} else {
		plot_df <- do.call(rbind, plot_list)

		p <- ggplot2::ggplot(
			plot_df,
			ggplot2::aes(x = .data$value, fill = .data$type)
		) +
			ggplot2::geom_density(alpha = 0.5) +
			ggplot2::facet_wrap(~variable, scales = "free") +
			ggplot2::labs(
				title = title,
				x = "Value",
				y = "Density",
				fill = "Data Type"
			) +
			ggplot2::scale_fill_manual(
				values = c("Observed" = "#1f77b4", "Imputed" = "#ff7f0e")
			) +
			ggplot2::theme_minimal()
	}

	ClinicalPlot(
		plot = p,
		type = "imputation_distributions",
		title = title,
		metadata = list(vars = vars)
	)
}

#' Plot Missing Data Pattern
#'
#' Visualizes the pattern of missing data in a dataset.
#'
#' @param x A data frame or ImputationResult object
#' @param title Plot title
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' data <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 3))
#' plot_missing_pattern(data)
plot_missing_pattern <- function(
	x,
	title = "Missing Data Pattern"
) {
	# Extract data frame
	if (S7::S7_inherits(x, ImputationResult)) {
		data <- x@original_data
	} else if (is.data.frame(x)) {
		data <- x
	} else {
		ph_abort("'x' must be a data frame or ImputationResult object")
	}

	# Create missing indicator matrix
	missing_mat <- is.na(data)

	# Calculate missingness per variable
	miss_pct <- colMeans(missing_mat) * 100
	miss_df <- data.frame(
		variable = names(miss_pct),
		pct_missing = as.numeric(miss_pct),
		stringsAsFactors = FALSE
	)
	miss_df <- miss_df[order(-miss_df$pct_missing), ]
	miss_df$variable <- factor(miss_df$variable, levels = miss_df$variable)

	p <- ggplot2::ggplot(
		miss_df,
		ggplot2::aes(x = .data$variable, y = .data$pct_missing)
	) +
		ggplot2::geom_bar(stat = "identity", fill = "#d62728") +
		ggplot2::coord_flip() +
		ggplot2::labs(
			title = title,
			x = "Variable",
			y = "% Missing"
		) +
		ggplot2::theme_minimal() +
		ggplot2::ylim(0, 100)

	ClinicalPlot(
		plot = p,
		type = "missing_pattern",
		title = title,
		data = miss_df,
		metadata = list(n_obs = nrow(data), n_vars = ncol(data))
	)
}

#' Create Imputation Diagnostic Report
#'
#' Generates a comprehensive diagnostic report for multiple imputation.
#'
#' @param imputation_result An ImputationResult object
#'
#' @return A list containing:
#' \describe{
#'   \item{summary}{Missing data summary data frame}
#'   \item{convergence_plot}{ClinicalPlot of convergence diagnostics}
#'   \item{distribution_plot}{ClinicalPlot comparing distributions}
#'   \item{missing_pattern_plot}{ClinicalPlot of missing patterns}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' imp <- perform_multiple_imputation(data, m = 5)
#' report <- create_imputation_report(imp)
#' report$convergence_plot
#' }
create_imputation_report <- function(imputation_result) {
	if (!S7::S7_inherits(imputation_result, ImputationResult)) {
		ph_abort("'imputation_result' must be an ImputationResult object")
	}

	list(
		summary = summarize_missing(imputation_result@original_data),
		convergence_plot = plot_imputation_convergence(imputation_result),
		distribution_plot = plot_imputation_distributions(imputation_result),
		missing_pattern_plot = plot_missing_pattern(imputation_result)
	)
}
