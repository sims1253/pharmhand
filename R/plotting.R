#' Standard Clinical Plots
#'
#' Reusable functions for creating standard clinical trial plots.
#'
#' @name plotting
#' @keywords internal
NULL

#' Create Kaplan-Meier Plot
#'
#' Generates a standard Kaplan-Meier plot using ggplot2 and survival.
#'
#' @param data Data frame containing survival data
#' @param time_var Time variable name
#' @param event_var Event variable name (1=event, 0=censor)
#' @param trt_var Treatment variable name
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param risk_table Logical, include risk table below. Requires patchwork.
#'
#' @return A ClinicalPlot object
#' @export
create_km_plot <- function(
	data,
	time_var,
	event_var,
	trt_var,
	title = "Kaplan-Meier Plot",
	xlab = "Time",
	ylab = "Survival Probability",
	risk_table = FALSE
) {
	if (!requireNamespace("survival", quietly = TRUE)) {
		cli::cli_abort("Package {.pkg survival} is required for KM plots")
	}
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		cli::cli_abort("Package {.pkg ggplot2} is required for KM plots")
	}

	# Create survival object
	surv_obj <- survival::Surv(data[[time_var]], data[[event_var]])

	# Fit model
	formula_str <- paste("surv_obj ~", trt_var)
	fit <- survival::survfit(as.formula(formula_str), data = data)

	# Extract data for plotting
	if (!is.null(fit[["strata"]])) {
		strata_names <- names(fit[["strata"]])
		strata_names_clean <- gsub(paste0(trt_var, "="), "", strata_names)
		plot_strata <- rep(strata_names_clean, fit[["strata"]])
	} else {
		strata_names_clean <- "All"
		plot_strata <- rep("All", length(fit[["time"]]))
	}

	plot_data <- data.frame(
		time = fit[["time"]],
		surv = fit[["surv"]],
		n_censor = fit[["n.censor"]],
		strata = plot_strata
	)

	# Add starting point (time 0, surv 1) for each stratum
	start_data <- data.frame(
		time = 0,
		surv = 1,
		n_censor = 0,
		strata = strata_names_clean
	)

	plot_data <- rbind(start_data, plot_data)

	# Plot
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$time, y = .data$surv, color = .data$strata)
	) +
		ggplot2::geom_step() +
		# Add censoring ticks
		ggplot2::geom_point(
			data = plot_data[plot_data[["n_censor"]] > 0, ],
			ggplot2::aes(x = .data$time, y = .data$surv),
			shape = 3, # Cross shape for censored events
			size = 1.5,
			show.legend = FALSE
		) +
		ggplot2::scale_y_continuous(
			labels = scales::percent,
			limits = c(0, 1)
		) +
		ggplot2::labs(
			title = title,
			x = xlab,
			y = ylab,
			color = "Treatment"
		) +
		ggplot2::theme_minimal() +
		ggplot2::theme(
			legend.position = "bottom",
			plot.title = ggplot2::element_text(hjust = 0.5),
			panel.grid.minor = ggplot2::element_blank()
		)

	if (risk_table) {
		if (!requireNamespace("patchwork", quietly = TRUE)) {
			cli::cli_warn(
				"Package {.pkg patchwork} required for risk tables. Returning plot only."
			)
			return(ClinicalPlot(plot = p, title = title))
		}

		# Determine time points for risk table (at 0, 25%, 50%, 75%, 100% of max)
		max_time <- max(fit[["time"]])
		break_points <- seq(0, max_time, length.out = 5)
		# Round to integers if appropriate or meaningful steps
		if (max_time > 10) {
			break_points <- round(break_points)
		}

		# Get summary at these time points
		risk_summary <- summary(fit, times = break_points)

		# Create data frame for risk table
		# We need n.risk and n.censor (cumulative)
		# Handle cases where n.censor or strata might be NULL
		n_breaks <- length(risk_summary[["time"]])
		risk_data <- data.frame(
			time = risk_summary[["time"]],
			strata = if ("strata" %in% names(risk_summary)) {
				gsub(
					paste0(trt_var, "="),
					"",
					as.character(risk_summary[["strata"]])
				)
			} else {
				rep("All", n_breaks)
			},
			n_risk = risk_summary[["n.risk"]],
			n_censor = if ("n.censor" %in% names(risk_summary)) {
				risk_summary[["n.censor"]]
			} else {
				rep(0, n_breaks)
			}
		)

		# Get cumulative censored counts for each strata
		full_summary <- summary(fit)
		n_full <- length(full_summary[["time"]])
		full_risk_df <- data.frame(
			time = full_summary[["time"]],
			strata = if ("strata" %in% names(full_summary)) {
				as.character(full_summary[["strata"]])
			} else {
				rep("All", n_full)
			},
			n_censor = if ("n.censor" %in% names(full_summary)) {
				full_summary[["n.censor"]]
			} else {
				rep(0, n_full)
			}
		) |>
			dplyr::group_by(.data$strata) |>
			dplyr::mutate(cum_censor = cumsum(.data$n_censor)) |>
			dplyr::ungroup()

		# Join cumulative censor back to risk_data
		# We need the cum_censor at or just before each break point
		risk_data[["cum_censor"]] <- sapply(
			seq_len(nrow(risk_data)),
			function(i) {
				cur_time <- risk_data[["time"]][i]
				cur_strata <- risk_data[["strata"]][i]
				relevant_strata_val <- if ("strata" %in% names(full_summary)) {
					paste0(trt_var, "=", cur_strata)
				} else {
					cur_strata
				}
				relevant_censor <- full_risk_df[["cum_censor"]][
					full_risk_df[["strata"]] == relevant_strata_val &
						full_risk_df[["time"]] <= cur_time
				]

				if (length(relevant_censor) == 0) {
					0
				} else {
					max(relevant_censor, na.rm = TRUE)
				}
			}
		)

		risk_data[["label"]] <- paste0(
			risk_data[["n_risk"]],
			" (",
			risk_data[["cum_censor"]],
			")"
		)

		rt <- ggplot2::ggplot(
			risk_data,
			ggplot2::aes(
				x = .data$time,
				y = .data$strata,
				label = .data$label,
				color = .data$strata
			)
		) +
			ggplot2::geom_text(size = 3) +
			ggplot2::scale_x_continuous(
				limits = c(0, max_time),
				breaks = break_points
			) +
			ggplot2::labs(x = NULL, y = NULL) +
			ggplot2::theme_minimal() +
			ggplot2::theme(
				panel.grid = ggplot2::element_blank(),
				axis.text.x = ggplot2::element_blank(),
				legend.position = "none",
				plot.margin = ggplot2::margin(0, 0, 0, 0)
			)

		# Combine using patchwork
		combined_plot <- p +
			rt +
			patchwork::plot_layout(ncol = 1, heights = c(3, 1))

		return(ClinicalPlot(
			plot = combined_plot,
			title = title,
			width = 6,
			height = 5,
			dpi = 300
		))
	}

	ClinicalPlot(
		plot = p,
		title = title,
		width = 6,
		height = 4,
		dpi = 300
	)
}
