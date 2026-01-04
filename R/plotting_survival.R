#' @title Survival Analysis Plots
#' @name plotting_survival
#' @description Functions for creating Kaplan-Meier and related survival plots.
NULL

#' Create Kaplan-Meier Plot
#'
#' Kaplan-Meier plot using ggplot2 and survival.
#'
#' @param data ADaMData object or data frame containing survival data
#' @param time_var Time variable name (default: "AVAL")
#' @param event_var Event variable name. If "CNSR" (ADaM censoring flag),
#'   it will be automatically inverted (0=event becomes 1=event).
#'   Otherwise expects 1=event, 0=censor. Default: "CNSR"
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param risk_table Logical, include risk table below. Requires patchwork.
#' @param show_median Logical, add horizontal/vertical lines at median
#'   survival time (default: FALSE)
#' @param show_ci Logical, show confidence bands around survival curves
#'   (default: FALSE)
#' @param show_censor Logical, show censoring marks as crosses (default: TRUE)
#' @param landmarks Numeric vector of timepoints to highlight with vertical
#'   lines (e.g., c(12, 24) for 12 and 24 months). NULL for none.
#' @param xlim Optional x-axis limits as c(min, max)
#' @param palette Optional color palette for treatment groups. Can be a
#'   character vector of colors, or NULL to use
#'   `getOption("pharmhand.palette")`. Defaults to the CVD-friendly
#'   "Okabe-Ito (reordered)" palette (orange, sky blue for first two arms).
#'   Other built-in options: "Okabe-Ito",
#'   "R4", "Tableau 10", "Alphabet", etc. (see `grDevices::palette.pals()`).
#' @param conf_level Confidence level for CI bands (default: 0.95)
#' @param base_size Base font size for plot text elements (default: 11).
#'   Also used for risk table text.
#' @param type Character. Plot type: "km" for Kaplan-Meier or "loglog" for
#'   log(-log(S(t))) vs log(t). For "loglog", median lines, CI bands,
#'   risk tables, and landmarks are intentionally omitted to keep the
#'   diagnostic scale uncluttered; censor marks are supported via
#'   `show_censor`.
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic KM plot
#' km <- create_km_plot(
#'   data = adtte,
#'   time_var = "AVAL",
#'   event_var = "CNSR",
#'   title = "Overall Survival"
#' )
#'
#' # With median lines and CI bands
#' km <- create_km_plot(
#'   data = adtte,
#'   show_median = TRUE,
#'   show_ci = TRUE,
#'   risk_table = TRUE
#' )
#' }
create_km_plot <- function(
	data,
	time_var = "AVAL",
	event_var = "CNSR",
	trt_var = "TRT01P",
	title = "Kaplan-Meier Plot",
	xlab = "Time",
	ylab = "Survival Probability",
	risk_table = FALSE,
	show_median = FALSE,
	show_ci = FALSE,
	show_censor = TRUE,
	landmarks = NULL,
	xlim = NULL,
	palette = NULL,
	conf_level = 0.95,
	base_size = 11,
	type = c("km", "loglog")
) {
	if (!requireNamespace("survival", quietly = TRUE)) {
		ph_abort("Package 'survival' is required for KM plots")
	}
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		ph_abort("Package 'ggplot2' is required for KM plots")
	}

	type <- match.arg(type)

	if (type == "loglog") {
		loglog_title <- if (missing(title)) {
			"Log-Log Survival Plot"
		} else {
			title
		}
		loglog_xlab <- if (missing(xlab)) {
			"Log(Time)"
		} else {
			xlab
		}
		loglog_ylab <- if (missing(ylab)) {
			"Log(-Log(Survival))"
		} else {
			ylab
		}

		return(create_loglog_plot(
			data = data,
			time_var = time_var,
			event_var = event_var,
			trt_var = trt_var,
			title = loglog_title,
			xlab = loglog_xlab,
			ylab = loglog_ylab,
			show_censor = show_censor,
			colors = palette,
			base_size = base_size
		))
	}

	# Get filtered data if ADaMData
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

	# Handle CNSR inversion (ADaM: 0=event, 1=censor -> survival: 1=event)
	if (event_var == "CNSR") {
		df$event <- 1 - df[[event_var]]
		event_var_use <- "event"
	} else {
		event_var_use <- event_var
	}

	# Create survival object
	surv_obj <- survival::Surv(df[[time_var]], df[[event_var_use]])

	# Fit model
	formula_str <- paste("surv_obj ~", trt_var_actual)
	fit <- survival::survfit(
		as.formula(formula_str),
		data = df,
		conf.int = conf_level
	)

	# Extract data for plotting
	if (!is.null(fit[["strata"]])) {
		strata_names <- names(fit[["strata"]])
		strata_names_clean <- gsub(paste0(trt_var_actual, "="), "", strata_names)
		plot_strata <- rep(strata_names_clean, fit[["strata"]])
	} else {
		strata_names_clean <- "All"
		plot_strata <- rep("All", length(fit[["time"]]))
	}

	plot_data <- data.frame(
		time = fit[["time"]],
		surv = fit[["surv"]],
		lower = fit[["lower"]],
		upper = fit[["upper"]],
		n_censor = fit[["n.censor"]],
		strata = plot_strata
	)

	# Add starting point (time 0, surv 1) for each stratum
	start_data <- data.frame(
		time = 0,
		surv = 1,
		lower = 1,
		upper = 1,
		n_censor = 0,
		strata = strata_names_clean
	)

	plot_data <- rbind(start_data, plot_data)

	# Plot
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$time, y = .data$surv, color = .data$strata)
	)

	# Add CI bands if requested (before step lines so they appear behind)
	if (show_ci) {
		p <- p +
			ggplot2::geom_ribbon(
				ggplot2::aes(
					ymin = .data$lower,
					ymax = .data$upper,
					fill = .data$strata
				),
				alpha = 0.2,
				color = NA
			)
	}

	# Add step lines
	p <- p + ggplot2::geom_step(linewidth = 0.8)

	# Add censoring ticks if requested
	if (show_censor) {
		censor_data <- plot_data[plot_data[["n_censor"]] > 0, ]
		if (nrow(censor_data) > 0) {
			p <- p +
				ggplot2::geom_point(
					data = censor_data,
					ggplot2::aes(x = .data$time, y = .data$surv),
					shape = 3,
					size = 1.5,
					show.legend = FALSE
				)
		}
	}

	# Add median lines if requested
	if (show_median) {
		median_table <- summary(fit)$table
		if (is.null(dim(median_table))) {
			median_table <- matrix(median_table, nrow = 1)
			rownames(median_table) <- strata_names_clean[1]
		}
		medians <- median_table[, "median"]
		names(medians) <- gsub(
			paste0(trt_var_actual, "="),
			"",
			rownames(median_table)
		)

		for (i in seq_along(medians)) {
			if (!is.na(medians[i])) {
				strata_name <- names(medians)[i]
				median_val <- medians[i]
				# Horizontal line at 0.5
				p <- p +
					ggplot2::geom_segment(
						x = 0,
						xend = median_val,
						y = 0.5,
						yend = 0.5,
						linetype = "dashed",
						color = "gray50",
						linewidth = 0.5
					)
				# Vertical line at median
				p <- p +
					ggplot2::geom_segment(
						x = median_val,
						xend = median_val,
						y = 0,
						yend = 0.5,
						linetype = "dashed",
						color = "gray50",
						linewidth = 0.5
					)
			}
		}
	}

	# Add landmark lines if requested
	if (!is.null(landmarks) && length(landmarks) > 0) {
		for (lm in landmarks) {
			p <- p +
				ggplot2::geom_vline(
					xintercept = lm,
					linetype = "dotted",
					color = "gray40",
					linewidth = 0.5
				)
		}
	}

	# Resolve color palette
	resolved_palette <- .resolve_palette(palette)

	p <- p + ggplot2::scale_color_manual(values = resolved_palette)
	if (show_ci) {
		p <- p + ggplot2::scale_fill_manual(values = resolved_palette)
	}

	# Apply x-axis limits if provided
	x_scale <- if (!is.null(xlim)) {
		ggplot2::scale_x_continuous(limits = xlim)
	} else {
		ggplot2::scale_x_continuous()
	}

	p <- p +
		x_scale +
		ggplot2::scale_y_continuous(
			labels = function(x) paste0(round(x * 100, 1), "%"),
			limits = c(0, 1)
		) +
		ggplot2::labs(
			title = title,
			x = xlab,
			y = ylab,
			color = "Treatment",
			fill = "Treatment"
		) +
		.pharmhand_theme(base_size = base_size) +
		ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

	if (risk_table) {
		if (!requireNamespace("patchwork", quietly = TRUE)) {
			ph_warn(
				"Package 'patchwork' required for risk tables. Returning plot only."
			)
			return(ClinicalPlot(plot = p, title = title))
		}

		# Determine time points for risk table (at 0, 25%, 50%, 75%, 100% of max)
		max_time <- max(fit[["time"]])
		if (!is.null(xlim)) {
			max_time <- xlim[2]
		}
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
					paste0(trt_var_actual, "="),
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
					paste0(trt_var_actual, "=", cur_strata)
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

		# Convert points to mm for text sizing
		pt_to_mm <- 72.27 / 25.4

		rt <- ggplot2::ggplot(
			risk_data,
			ggplot2::aes(
				x = .data$time,
				y = .data$strata,
				label = .data$label,
				color = .data$strata
			)
		) +
			ggplot2::geom_text(size = base_size / pt_to_mm) +
			ggplot2::scale_x_continuous(
				limits = c(0, max_time),
				breaks = break_points
			) +
			ggplot2::labs(x = NULL, y = NULL) +
			.pharmhand_theme(base_size = base_size) +
			ggplot2::theme(
				panel.grid = ggplot2::element_blank(),
				axis.text.x = ggplot2::element_blank(),
				legend.position = "none",
				plot.margin = ggplot2::margin(0, 0, 0, 0, "lines")
			)

		rt <- rt + ggplot2::scale_color_manual(values = resolved_palette)

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

#' Create AE Cumulative Incidence Plot
#'
#' Creates cumulative incidence plot for adverse events using 1 - KM estimator.
#' For simple analyses without competing risks.
#'
#' @param data Data frame with time-to-event data
#' @param time_var Character. Time variable
#' @param event_var Character. Event indicator (1=AE occurred, 0=censored)
#' @param trt_var Character. Treatment variable
#' @param title Character. Plot title
#' @param xlab Character. X-axis label
#' @param ylab Character. Y-axis label
#' @param show_ci Logical. Show confidence bands (default: TRUE)
#' @param conf_level Numeric. Confidence level for intervals (default: 0.95)
#' @param colors Named character vector of colors
#' @param base_size Base font size for plot text elements (default: 11)
#'
#' @return ClinicalPlot with cumulative incidence curves
#'
#' @details
#' Uses 1 - Kaplan-Meier to estimate cumulative incidence. This is appropriate
#' when there are no competing risks or competing risks are minimal.
#'
#' If `event_var` contains values other than 0/1, they are treated as competing
#' events and coded as censored for this estimator.
#'
#' For analyses with significant competing risks (e.g., mortality), consider
#' using dedicated competing risk packages like cmprsk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Cumulative incidence plot for AEs
#' plot <- create_ae_cumulative_incidence_plot(
#'   data = adaette,
#'   time_var = "AVAL",
#'   event_var = "CNSR",
#'   trt_var = "TRT01P",
#'   title = "Cumulative Incidence of Serious AEs"
#' )
#' print(plot)
#' }
create_ae_cumulative_incidence_plot <- function(
	data,
	time_var,
	event_var,
	trt_var,
	title = "Cumulative Incidence of Adverse Events",
	xlab = "Time",
	ylab = "Cumulative Incidence",
	show_ci = TRUE,
	conf_level = 0.95,
	colors = NULL,
	base_size = 11
) {
	if (!requireNamespace("survival", quietly = TRUE)) {
		ph_abort("Package 'survival' is required for cumulative incidence plots")
	}
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		ph_abort("Package 'ggplot2' is required for cumulative incidence plots")
	}

	if (
		!is.numeric(conf_level) ||
			length(conf_level) != 1 ||
			conf_level <= 0 ||
			conf_level >= 1
	) {
		ph_abort("'conf_level' must be a single number between 0 and 1")
	}

	# Get filtered data if ADaMData
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

	# Handle CNSR inversion (ADaM: 0=event, 1=censor -> survival: 1=event)
	if (event_var == "CNSR") {
		df$event <- 1 - df[[event_var]]
		event_var_use <- "event"
	} else {
		event_var_use <- event_var
	}

	# Multi-step type conversion with warnings helps users understand data issues
	event_values <- df[[event_var_use]]
	if (is.logical(event_values)) {
		event_values_num <- as.integer(event_values)
	} else if (is.factor(event_values)) {
		event_values_num <- suppressWarnings(as.numeric(as.character(event_values)))
	} else if (is.character(event_values)) {
		event_values_num <- suppressWarnings(as.numeric(event_values))
	} else {
		event_values_num <- event_values
	}

	if (any(is.na(event_values_num) & !is.na(event_values))) {
		ph_warn(
			paste0(
				"Non-numeric values detected in '",
				event_var_use,
				"'. Treating them as censored for 1-KM cumulative incidence."
			)
		)
	}

	unique_events <- unique(stats::na.omit(event_values_num))
	non_binary_events <- setdiff(unique_events, c(0, 1))
	if (length(non_binary_events) > 0) {
		ph_warn(
			paste0(
				"Detected event codes other than 0/1 in '",
				event_var_use,
				"'. Treating non-1 values as censored for 1-KM cumulative incidence."
			)
		)
	}

	df$event <- ifelse(event_values_num == 1, 1, 0)
	event_var_use <- "event"

	# Create survival object
	surv_obj <- survival::Surv(df[[time_var]], df[[event_var_use]])

	# Fit model
	formula_str <- paste("surv_obj ~", trt_var_actual)
	fit <- survival::survfit(
		as.formula(formula_str),
		data = df,
		conf.int = conf_level
	)

	# Extract data for plotting
	if (!is.null(fit[["strata"]])) {
		strata_names <- names(fit[["strata"]])
		strata_names_clean <- gsub(paste0(trt_var_actual, "="), "", strata_names)
		plot_strata <- rep(strata_names_clean, fit[["strata"]])
	} else {
		strata_names_clean <- "All"
		plot_strata <- rep("All", length(fit[["time"]]))
	}

	plot_data <- data.frame(
		time = fit[["time"]],
		surv = fit[["surv"]],
		lower_surv = fit[["lower"]],
		upper_surv = fit[["upper"]],
		strata = plot_strata
	)

	plot_data$cum_inc <- 1 - plot_data$surv
	plot_data$lower <- 1 - plot_data$upper_surv
	plot_data$upper <- 1 - plot_data$lower_surv

	# Add starting point (time 0, cumulative incidence 0) for each stratum
	start_data <- data.frame(
		time = 0,
		surv = 1,
		lower_surv = 1,
		upper_surv = 1,
		strata = strata_names_clean,
		cum_inc = 0,
		lower = 0,
		upper = 0
	)

	plot_data <- rbind(start_data, plot_data)

	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$time, y = .data$cum_inc, color = .data$strata)
	)

	if (show_ci) {
		p <- p +
			ggplot2::geom_ribbon(
				ggplot2::aes(
					ymin = .data$lower,
					ymax = .data$upper,
					fill = .data$strata
				),
				alpha = 0.2,
				color = NA
			)
	}

	p <- p + ggplot2::geom_step(linewidth = 0.8)

	# Resolve color palette
	resolved_colors <- .resolve_palette(colors)

	p <- p + ggplot2::scale_color_manual(values = resolved_colors)
	if (show_ci) {
		p <- p + ggplot2::scale_fill_manual(values = resolved_colors)
	}

	p <- p +
		ggplot2::scale_y_continuous(
			labels = function(x) paste0(round(x * 100, 1), "%"),
			limits = c(0, 1)
		) +
		ggplot2::labs(
			title = title,
			x = xlab,
			y = ylab,
			color = "Treatment",
			fill = "Treatment"
		) +
		.pharmhand_theme(base_size = base_size) +
		ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

	ClinicalPlot(
		plot = p,
		title = title,
		width = 6,
		height = 4,
		dpi = 300
	)
}

#' Create Log-Log Survival Plot
#'
#' Creates a log(-log(survival)) vs log(time) plot for visual assessment
#' of the proportional hazards assumption. Parallel curves indicate the
#' PH assumption is likely satisfied.
#'
#' @param data ADaMData object or data frame with time-to-event data
#' @param time_var Character. Name of the time variable
#' @param event_var Character. Name of the event variable (1=event, 0=censor).
#'   If "CNSR" (ADaM censoring flag), it will be inverted (0=event becomes 1).
#' @param trt_var Character. Name of the treatment variable
#' @param title Character. Plot title (default: "Log-Log Survival Plot")
#' @param xlab Character. X-axis label (default: "Log(Time)")
#' @param ylab Character. Y-axis label (default: "Log(-Log(Survival))")
#' @param show_censor Logical, show censoring marks as crosses (default: TRUE).
#' @param colors Named character vector of colors for each treatment group.
#'   If NULL, uses `getOption("pharmhand.palette")` or the default palette.
#' @param base_size Base font size for plot text elements (default: 11).
#' @param conf_level Confidence level for survival fit (default: 0.95)
#'
#' @return A ClinicalPlot object containing a ggplot2 log-log survival plot
#'
#' @details
#' Under the Cox proportional hazards assumption, the log-log transformed
#' survival curves should be approximately parallel. Crossing or diverging
#' curves suggest the PH assumption may be violated.
#'
#' This is a complementary visual diagnostic to the statistical test
#' provided by test_ph_assumption(). Censor marks are supported; median
#' lines, CI bands, risk tables, and landmarks are intentionally omitted
#' to keep the diagnostic scale uncluttered.
#'
#' @references
#' IQWiG Methods v8.0, Section 10.3.12, p. 235-237.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Log-log plot to assess PH assumption
#' plot <- create_loglog_plot(
#'   data = adtte,
#'   time_var = "AVAL",
#'   event_var = "CNSR",
#'   trt_var = "TRT01P",
#'   title = "Log-Log Plot: PH Assumption Check"
#' )
#' print(plot)
#' }
create_loglog_plot <- function(
	data,
	time_var,
	event_var,
	trt_var,
	title = "Log-Log Survival Plot",
	xlab = "Log(Time)",
	ylab = "Log(-Log(Survival))",
	show_censor = TRUE,
	colors = NULL,
	base_size = 11,
	conf_level = 0.95
) {
	if (!requireNamespace("survival", quietly = TRUE)) {
		ph_abort("Package 'survival' is required for log-log plots")
	}
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		ph_abort("Package 'ggplot2' is required for log-log plots")
	}

	# Get filtered data if ADaMData
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

	# Handle CNSR inversion (ADaM: 0=event, 1=censor -> survival: 1=event)
	if (event_var == "CNSR") {
		df$event <- 1 - df[[event_var]]
		event_var_use <- "event"
	} else {
		event_var_use <- event_var
	}

	# Create survival object
	surv_obj <- survival::Surv(df[[time_var]], df[[event_var_use]])

	# Fit model
	formula_str <- paste("surv_obj ~", trt_var_actual)
	fit <- survival::survfit(
		as.formula(formula_str),
		data = df,
		conf.int = conf_level
	)

	# Extract data for plotting
	if (!is.null(fit[["strata"]])) {
		strata_names <- names(fit[["strata"]])
		strata_names_clean <- gsub(paste0(trt_var_actual, "="), "", strata_names)
		plot_strata <- rep(strata_names_clean, fit[["strata"]])
	} else {
		strata_names_clean <- "All"
		plot_strata <- rep("All", length(fit[["time"]]))
	}

	plot_data <- data.frame(
		time = fit[["time"]],
		surv = fit[["surv"]],
		n_censor = if (!is.null(fit[["n.censor"]])) {
			fit[["n.censor"]]
		} else {
			rep(0, length(fit[["time"]]))
		},
		strata = plot_strata
	)

	plot_data$log_time <- log(plot_data$time)
	plot_data$loglog_surv <- log(-log(plot_data$surv))
	keep <- is.finite(plot_data$log_time) & is.finite(plot_data$loglog_surv)
	n_dropped <- sum(!keep)
	if (n_dropped > 0) {
		ph_warn(sprintf(
			"Dropped %d data point(s) with non-finite log-log values",
			n_dropped
		))
	}
	plot_data <- plot_data[keep, , drop = FALSE]

	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(
			x = .data$log_time,
			y = .data$loglog_surv,
			color = .data$strata
		)
	)

	p <- p + ggplot2::geom_step(linewidth = 0.8)

	if (show_censor) {
		censor_data <- plot_data[plot_data[["n_censor"]] > 0, ]
		if (nrow(censor_data) > 0) {
			p <- p +
				ggplot2::geom_point(
					data = censor_data,
					ggplot2::aes(x = .data$log_time, y = .data$loglog_surv),
					shape = 3,
					size = 1.5,
					show.legend = FALSE
				)
		}
	}

	# Resolve color palette
	resolved_colors <- .resolve_palette(colors)

	p <- p + ggplot2::scale_color_manual(values = resolved_colors)

	p <- p +
		ggplot2::labs(
			title = title,
			x = xlab,
			y = ylab,
			color = "Treatment"
		) +
		.pharmhand_theme(base_size = base_size) +
		ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

	ClinicalPlot(
		plot = p,
		data = plot_data,
		type = "loglog",
		title = title,
		width = 6,
		height = 4,
		dpi = 300
	)
}
