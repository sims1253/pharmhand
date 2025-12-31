#' Clinical Plots
#'
#' Functions for standard clinical trial plots.
#'
#' @name plotting
#' @keywords internal
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
#'   `getOption("pharmhand.palette")`. Defaults to the CVD-friendly "Okabe-Ito"
#'   palette. Other built-in options: "R4", "Tableau 10", "Alphabet", etc.
#'   (see `grDevices::palette.pals()`).
#' @param conf_level Confidence level for CI bands (default: 0.95)
#' @param base_size Base font size for plot text elements (default: 11).
#'   Also used for risk table text.
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
	base_size = 11
) {
	if (!requireNamespace("survival", quietly = TRUE)) {
		stop("Package 'survival' is required for KM plots", call. = FALSE)
	}
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("Package 'ggplot2' is required for KM plots", call. = FALSE)
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
	# Use grDevices::palette.colors() for named palettes (requires R >= 4.0)
	resolved_palette <- if (!is.null(palette)) {
		palette
	} else {
		opt_palette <- getOption("pharmhand.palette", default = "Okabe-Ito")
		if (is.character(opt_palette) && length(opt_palette) == 1) {
			# Named palette - use palette.colors() from grDevices
			# Available palettes: palette.pals()
			tryCatch(
				grDevices::palette.colors(n = NULL, palette = opt_palette),
				error = function(e) {
					warning(
						paste0("Palette '", opt_palette, "' not found, using 'Okabe-Ito'"),
						call. = FALSE
					)
					grDevices::palette.colors(n = NULL, palette = "Okabe-Ito")
				}
			)
		} else if (is.character(opt_palette) && length(opt_palette) > 1) {
			# User provided a vector of colors via options
			opt_palette
		} else {
			# Fallback to Okabe-Ito
			grDevices::palette.colors(n = NULL, palette = "Okabe-Ito")
		}
	}

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
			labels = function(x) paste0(x * 100, "%"),
			limits = c(0, 1)
		) +
		ggplot2::labs(
			title = title,
			x = xlab,
			y = ylab,
			color = "Treatment",
			fill = "Treatment"
		) +
		ggplot2::theme_minimal(base_size = base_size) +
		ggplot2::theme(
			legend.position = "bottom",
			plot.title = ggplot2::element_text(hjust = 0.5),
			panel.grid.minor = ggplot2::element_blank()
		)

	if (risk_table) {
		if (!requireNamespace("patchwork", quietly = TRUE)) {
			warning(
				"Package 'patchwork' required for risk tables. Returning plot only.",
				call. = FALSE
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

		rt <- ggplot2::ggplot(
			risk_data,
			ggplot2::aes(
				x = .data$time,
				y = .data$strata,
				label = .data$label,
				color = .data$strata
			)
		) +
			ggplot2::geom_text(size = base_size / ggplot2::.pt) +
			ggplot2::scale_x_continuous(
				limits = c(0, max_time),
				breaks = break_points
			) +
			ggplot2::labs(x = NULL, y = NULL) +
			ggplot2::theme_minimal(base_size = base_size) +
			ggplot2::theme(
				panel.grid = ggplot2::element_blank(),
				axis.text.x = ggplot2::element_blank(),
				legend.position = "none",
				plot.margin = ggplot2::margin(0, 0, 0, 0)
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

#' Create Subgroup Forest Plot
#'
#' Generates a forest plot showing treatment effects (hazard ratio or odds
#' ratio) across pre-specified subgroups with optional interaction p-values.
#'
#' @param data ADaMData object or data frame
#' @param subgroups Named list mapping variable names to display labels,
#'   e.g., `list(AGEGR1 = "Age Group", SEX = "Sex", RACE = "Race")`
#' @param endpoint_type "tte" for time-to-event (HR) or "binary" for
#'   binary outcomes (OR)
#' @param time_var Time variable for TTE endpoints (default: "AVAL")
#' @param event_var Event variable for TTE endpoints. If "CNSR", will be
#'   inverted automatically. Default: "CNSR"
#' @param response_var Response variable for binary endpoints (default: "AVALC")
#' @param response_values Values indicating response for binary endpoints
#'   (default: c("CR", "PR"))
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param ref_group Reference treatment group. If NULL, uses first level.
#' @param conf_level Confidence level (default: 0.95)
#' @param show_interaction Logical, calculate and show interaction p-values
#'   (default: TRUE)
#' @param null_line Reference line value (default: 1 for HR/OR)
#' @param title Plot title
#' @param xlab X-axis label. If NULL, auto-generated based on endpoint_type.
#' @param log_scale Logical, use log scale for x-axis (default: TRUE)
#' @param colors Optional named vector of colors for estimate types
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' # TTE subgroup analysis
#' forest <- create_forest_plot(
#'   data = adtte,
#'   subgroups = list(
#'     AGEGR1 = "Age Group",
#'     SEX = "Sex",
#'     RACE = "Race"
#'   ),
#'   endpoint_type = "tte",
#'   title = "Subgroup Analysis - Overall Survival"
#' )
#'
#' # Binary subgroup analysis
#' forest <- create_forest_plot(
#'   data = adrs,
#'   subgroups = list(SEX = "Sex", AGEGR1 = "Age"),
#'   endpoint_type = "binary",
#'   response_values = c("CR", "PR"),
#'   title = "Response Rate by Subgroup"
#' )
#' }
create_forest_plot <- function(
	data,
	subgroups,
	endpoint_type = c("tte", "binary"),
	time_var = "AVAL",
	event_var = "CNSR",
	response_var = "AVALC",
	response_values = c("CR", "PR"),
	trt_var = "TRT01P",
	ref_group = NULL,
	conf_level = 0.95,
	show_interaction = TRUE,
	null_line = 1,
	title = "Subgroup Analysis",
	xlab = NULL,
	log_scale = TRUE,
	colors = NULL
) {
	endpoint_type <- match.arg(endpoint_type)

	# Get filtered data
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

	# Handle CNSR inversion for TTE
	if (endpoint_type == "tte" && event_var == "CNSR") {
		df$event <- 1 - df[[event_var]]
		event_var_use <- "event"
	} else if (endpoint_type == "tte") {
		event_var_use <- event_var
	}

	# Handle binary response
	if (endpoint_type == "binary") {
		df$responder <- as.integer(df[[response_var]] %in% response_values)
	}

	# Ensure treatment is factor
	df[[trt_var_actual]] <- as.factor(df[[trt_var_actual]])
	trt_levels <- levels(df[[trt_var_actual]])

	if (is.null(ref_group)) {
		ref_group <- trt_levels[1]
	}

	# Set default x-axis label
	if (is.null(xlab)) {
		xlab <- if (endpoint_type == "tte") {
			"Hazard Ratio (95% CI)"
		} else {
			"Odds Ratio (95% CI)"
		}
	}

	# Calculate overall estimate
	overall_result <- calculate_subgroup_effect(
		df = df,
		subgroup_var = NULL,
		subgroup_level = "Overall",
		endpoint_type = endpoint_type,
		time_var = time_var,
		event_var_use = if (endpoint_type == "tte") event_var_use else NULL,
		trt_var = trt_var_actual,
		ref_group = ref_group,
		conf_level = conf_level
	)

	# Calculate estimates for each subgroup
	results_list <- list(overall_result)

	for (var_name in names(subgroups)) {
		label <- subgroups[[var_name]]

		if (!var_name %in% names(df)) {
			ph_warn(sprintf("Subgroup variable '%s' not found, skipping", var_name))
			next
		}

		levels_var <- unique(df[[var_name]])
		levels_var <- levels_var[!is.na(levels_var)]

		# Calculate interaction p-value for this variable
		interaction_p <- NA_real_
		if (show_interaction) {
			interaction_p <- calculate_interaction_pvalue(
				df = df,
				subgroup_var = var_name,
				endpoint_type = endpoint_type,
				time_var = time_var,
				event_var_use = if (endpoint_type == "tte") event_var_use else NULL,
				trt_var = trt_var_actual
			)
		}

		for (lvl in levels_var) {
			result <- calculate_subgroup_effect(
				df = df,
				subgroup_var = var_name,
				subgroup_level = as.character(lvl),
				endpoint_type = endpoint_type,
				time_var = time_var,
				event_var_use = if (endpoint_type == "tte") event_var_use else NULL,
				trt_var = trt_var_actual,
				ref_group = ref_group,
				conf_level = conf_level
			)
			result$subgroup_label <- label
			result$interaction_p <- interaction_p
			results_list <- c(results_list, list(result))
		}
	}

	# Build plot data frame
	plot_df <- do.call(rbind, lapply(results_list, as.data.frame))

	# Create display labels
	plot_df$display_label <- ifelse(
		plot_df$subgroup_var == "Overall",
		"Overall",
		paste0("    ", plot_df$subgroup_level)
	)

	# Add subgroup headers
	header_rows <- data.frame(
		subgroup_var = names(subgroups),
		subgroup_level = NA_character_,
		subgroup_label = unlist(subgroups),
		n_trt = NA_integer_,
		n_ref = NA_integer_,
		estimate = NA_real_,
		lcl = NA_real_,
		ucl = NA_real_,
		pvalue = NA_real_,
		interaction_p = NA_real_,
		display_label = unlist(subgroups),
		is_header = TRUE,
		stringsAsFactors = FALSE
	)

	plot_df$is_header <- FALSE
	plot_df <- rbind(plot_df, header_rows)

	# Create row order (Overall first, then subgroups in order)
	plot_df$row_order <- NA_integer_
	row_num <- 1
	plot_df$row_order[plot_df$subgroup_var == "Overall"] <- row_num
	row_num <- row_num + 1

	for (var_name in names(subgroups)) {
		# Header
		plot_df$row_order[
			plot_df$subgroup_var == var_name & plot_df$is_header
		] <- row_num
		row_num <- row_num + 1
		# Levels
		level_rows <- which(
			plot_df$subgroup_var == var_name & !plot_df$is_header
		)
		for (i in level_rows) {
			plot_df$row_order[i] <- row_num
			row_num <- row_num + 1
		}
	}

	plot_df <- plot_df[order(plot_df$row_order), ]

	# Create the forest plot
	# Filter out header rows for point/line geoms
	point_df <- plot_df[!plot_df$is_header & !is.na(plot_df$estimate), ]

	p <- ggplot2::ggplot(
		point_df,
		ggplot2::aes(
			x = .data$estimate,
			y = stats::reorder(.data$display_label, -.data$row_order)
		)
	) +
		# Reference line at null
		ggplot2::geom_vline(
			xintercept = null_line,
			linetype = "dashed",
			color = "gray50"
		) +
		# Error bars (horizontal)
		ggplot2::geom_errorbar(
			ggplot2::aes(xmin = .data$lcl, xmax = .data$ucl),
			height = 0.2,
			orientation = "y"
		) +
		# Points
		ggplot2::geom_point(size = 3) +
		# Styling
		ggplot2::theme_minimal() +
		ggplot2::theme(
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor = ggplot2::element_blank(),
			axis.text.y = ggplot2::element_text(hjust = 0),
			plot.title = ggplot2::element_text(hjust = 0.5)
		) +
		ggplot2::labs(
			title = title,
			x = xlab,
			y = NULL
		)

	if (log_scale) {
		p <- p + ggplot2::scale_x_log10()
	}

	# Apply colors if provided
	if (!is.null(colors)) {
		p <- p + ggplot2::scale_color_manual(values = colors)
	}

	# Calculate dimensions based on number of rows
	n_rows <- nrow(plot_df)
	plot_height <- max(4, 1 + n_rows * 0.4)

	ClinicalPlot(
		plot = p,
		data = plot_df,
		type = "forest",
		title = title,
		width = 10,
		height = plot_height,
		dpi = 300,
		metadata = list(
			subgroups = subgroups,
			endpoint_type = endpoint_type,
			ref_group = ref_group
		)
	)
}

#' Calculate Subgroup Effect (HR or OR)
#'
#' @param df Data frame
#' @param subgroup_var Subgroup variable name (NULL for overall)
#' @param subgroup_level Subgroup level value
#' @param endpoint_type "tte" or "binary"
#' @param time_var Time variable for TTE
#' @param event_var_use Event variable for TTE
#' @param trt_var Treatment variable
#' @param ref_group Reference group
#' @param conf_level Confidence level
#'
#' @return List with estimate, CI, counts, etc.
#' @keywords internal
calculate_subgroup_effect <- function(
	df,
	subgroup_var,
	subgroup_level,
	endpoint_type,
	time_var,
	event_var_use,
	trt_var,
	ref_group,
	conf_level
) {
	# Filter to subgroup if specified
	if (!is.null(subgroup_var)) {
		df <- df[df[[subgroup_var]] == subgroup_level, ]
	}

	# Get counts
	n_trt <- sum(df[[trt_var]] != ref_group)
	n_ref <- sum(df[[trt_var]] == ref_group)

	# Need at least some subjects in each arm
	if (n_trt < 2 || n_ref < 2) {
		return(list(
			subgroup_var = subgroup_var %||% "Overall",
			subgroup_level = subgroup_level,
			subgroup_label = NA_character_,
			n_trt = n_trt,
			n_ref = n_ref,
			estimate = NA_real_,
			lcl = NA_real_,
			ucl = NA_real_,
			pvalue = NA_real_,
			interaction_p = NA_real_
		))
	}

	if (endpoint_type == "tte") {
		# Cox model for HR
		df[[trt_var]] <- stats::relevel(factor(df[[trt_var]]), ref = ref_group)
		surv_obj <- survival::Surv(df[[time_var]], df[[event_var_use]])

		# Initialize with NA defaults
		estimate <- NA_real_
		lcl <- NA_real_
		ucl <- NA_real_
		pvalue <- NA_real_

		tryCatch(
			{
				cox_fit <- survival::coxph(
					stats::as.formula(paste("surv_obj ~", trt_var)),
					data = df
				)
				cox_summary <- summary(cox_fit)

				estimate <- cox_summary$conf.int[1, "exp(coef)"]
				lcl <- cox_summary$conf.int[1, "lower .95"]
				ucl <- cox_summary$conf.int[1, "upper .95"]
				pvalue <- cox_summary$coefficients[1, "Pr(>|z|)"]
			},
			error = function(e) NULL
		)
	} else {
		# Logistic regression for OR
		df[[trt_var]] <- stats::relevel(factor(df[[trt_var]]), ref = ref_group)

		# Initialize with NA defaults
		estimate <- NA_real_
		lcl <- NA_real_
		ucl <- NA_real_
		pvalue <- NA_real_

		tryCatch(
			{
				glm_fit <- stats::glm(
					stats::as.formula(paste("responder ~", trt_var)),
					data = df,
					family = stats::binomial()
				)
				glm_summary <- summary(glm_fit)

				estimate <- exp(stats::coef(glm_fit)[2])
				ci <- exp(stats::confint.default(glm_fit, level = conf_level)[2, ])
				lcl <- ci[1]
				ucl <- ci[2]
				pvalue <- glm_summary$coefficients[2, "Pr(>|z|)"]
			},
			error = function(e) NULL
		)
	}

	list(
		subgroup_var = subgroup_var %||% "Overall",
		subgroup_level = subgroup_level,
		subgroup_label = NA_character_,
		n_trt = n_trt,
		n_ref = n_ref,
		estimate = estimate,
		lcl = lcl,
		ucl = ucl,
		pvalue = pvalue,
		interaction_p = NA_real_
	)
}

#' Calculate Interaction P-value
#'
#' Calculates the p-value for treatment-by-subgroup interaction using
#' likelihood ratio test. Used internally by forest plot functions.
#'
#' @param df Data frame containing analysis data with treatment, subgroup,
#'   and endpoint variables.
#' @param subgroup_var Character. Name of the subgroup variable column.
#' @param endpoint_type Character. Either "tte" for time-to-event (uses Cox
#'   regression) or "binary" for binary endpoints (uses logistic regression).
#' @param time_var Character. Name of time variable column (only used when
#'   endpoint_type = "tte").
#' @param event_var_use Character. Name of event indicator column (only used
#'   when endpoint_type = "tte").
#' @param trt_var Character. Name of treatment variable column.
#'
#' @return Numeric. P-value from likelihood ratio test comparing model with
#'   and without treatment-by-subgroup interaction term. Returns NA if model
#'   fitting fails.
#' @keywords internal
calculate_interaction_pvalue <- function(
	df,
	subgroup_var,
	endpoint_type,
	time_var,
	event_var_use,
	trt_var
) {
	tryCatch(
		{
			if (endpoint_type == "tte") {
				surv_obj <- survival::Surv(df[[time_var]], df[[event_var_use]])

				# Model without interaction
				formula_main <- stats::as.formula(
					paste("surv_obj ~", trt_var, "+", subgroup_var)
				)
				# Model with interaction
				formula_int <- stats::as.formula(
					paste("surv_obj ~", trt_var, "*", subgroup_var)
				)

				fit_main <- survival::coxph(formula_main, data = df)
				fit_int <- survival::coxph(formula_int, data = df)

				# Likelihood ratio test
				lr_test <- stats::anova(fit_main, fit_int)
				pvalue <- lr_test[["Pr(>|Chi|)"]][2]
			} else {
				# Model without interaction
				formula_main <- stats::as.formula(
					paste("responder ~", trt_var, "+", subgroup_var)
				)
				# Model with interaction
				formula_int <- stats::as.formula(
					paste("responder ~", trt_var, "*", subgroup_var)
				)

				fit_main <- stats::glm(
					formula_main,
					data = df,
					family = stats::binomial()
				)
				fit_int <- stats::glm(
					formula_int,
					data = df,
					family = stats::binomial()
				)

				# Likelihood ratio test
				lr_test <- stats::anova(fit_main, fit_int, test = "Chisq")
				pvalue <- lr_test[["Pr(>Chi)"]][2]
			}

			pvalue
		},
		error = function(e) {
			NA_real_
		}
	)
}
