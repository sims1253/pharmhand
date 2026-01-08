#' @title Efficacy Visualization Plots
#' @name plotting_efficacy
#' @description Functions for visualizing efficacy endpoints over time.
NULL

#' Create Mean Plot with Confidence Intervals Over Time
#'
#' Creates a line plot showing mean values with confidence intervals across
#' visits or time points, typically used for PRO or lab parameter visualization.
#'
#' @param data Data frame containing the data
#' @param x_var Character. Variable for x-axis (visit or time)
#' @param y_var Character. Variable for y-axis (value)
#' @param group_var Character. Grouping variable (e.g., treatment).
#'   Default: NULL
#' @param ci_level Numeric. Confidence level for intervals. Default: 0.95
#' @param show_points Logical. Show individual data points. Default: FALSE
#' @param show_n Logical. Show sample size at each point. Default: TRUE
#' @param title Character. Plot title. Default: NULL
#' @param x_label Character. X-axis label. Default: NULL (uses x_var)
#' @param y_label Character. Y-axis label. Default: NULL (uses y_var)
#' @param palette Character vector. Colors for groups. Default:
#'   NULL (uses default)
#' @param base_size Numeric. Base font size. Default: 11
#' @param line_size Numeric. Line width. Default: 1
#' @param point_size Numeric. Point size for means. Default: 3
#' @param dodge_width Numeric. Horizontal dodge for overlapping
#'   points. Default: 0.2
#' @param error_bar_width Numeric. Width of error bar caps. Default: 0.1
#'
#' @return A ClinicalPlot object containing the ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   USUBJID = rep(1:50, each = 5),
#'   AVISITN = rep(0:4, 50),
#'   AVISIT = rep(c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12"), 50),
#'   AVAL = rnorm(250, mean = rep(c(50, 48, 45, 44, 43), 50), sd = 10),
#'   TRT01P = rep(c("Treatment", "Placebo"), each = 125)
#' )
#'
#' plot <- create_mean_plot(
#'   data = data,
#'   x_var = "AVISIT",
#'   y_var = "AVAL",
#'   group_var = "TRT01P",
#'   title = "Mean Score Over Time"
#' )
#' }
create_mean_plot <- function(
	data,
	x_var,
	y_var,
	group_var = NULL,
	ci_level = 0.95,
	show_points = FALSE,
	show_n = TRUE,
	title = NULL,
	x_label = NULL,
	y_label = NULL,
	palette = NULL,
	base_size = 11,
	line_size = 1,
	point_size = 3,
	dodge_width = 0.2,
	error_bar_width = 0.1
) {
	# Validate inputs
	admiraldev::assert_data_frame(data)
	admiraldev::assert_character_scalar(x_var)
	admiraldev::assert_character_scalar(y_var)

	required_vars <- c(x_var, y_var)
	if (!is.null(group_var)) {
		required_vars <- c(required_vars, group_var)
	}

	missing_vars <- setdiff(required_vars, names(data))
	if (length(missing_vars) > 0) {
		ph_abort(sprintf(
			"Variables not found in data: %s",
			paste(missing_vars, collapse = ", ")
		))
	}

	# Calculate summary statistics
	alpha <- 1 - ci_level

	if (!is.null(group_var)) {
		summary_data <- data |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(c(x_var, group_var)))
			) |>
			dplyr::summarise(
				mean_val = mean(.data[[y_var]], na.rm = TRUE),
				sd_val = stats::sd(.data[[y_var]], na.rm = TRUE),
				n = sum(!is.na(.data[[y_var]])),
				.groups = "drop"
			) |>
			dplyr::mutate(
				se = dplyr::if_else(
					.data$n > 1,
					.data$sd_val / sqrt(.data$n),
					NA_real_
				),
				ci_lower = dplyr::if_else(
					.data$n > 1,
					.data$mean_val - stats::qt(1 - alpha / 2, .data$n - 1) * .data$se,
					NA_real_
				),
				ci_upper = dplyr::if_else(
					.data$n > 1,
					.data$mean_val + stats::qt(1 - alpha / 2, .data$n - 1) * .data$se,
					NA_real_
				)
			)
	} else {
		summary_data <- data |>
			dplyr::group_by(dplyr::across(dplyr::all_of(x_var))) |>
			dplyr::summarise(
				mean_val = mean(.data[[y_var]], na.rm = TRUE),
				sd_val = stats::sd(.data[[y_var]], na.rm = TRUE),
				n = sum(!is.na(.data[[y_var]])),
				.groups = "drop"
			) |>
			dplyr::mutate(
				se = dplyr::if_else(
					.data$n > 1,
					.data$sd_val / sqrt(.data$n),
					NA_real_
				),
				ci_lower = dplyr::if_else(
					.data$n > 1,
					.data$mean_val - stats::qt(1 - alpha / 2, .data$n - 1) * .data$se,
					NA_real_
				),
				ci_upper = dplyr::if_else(
					.data$n > 1,
					.data$mean_val + stats::qt(1 - alpha / 2, .data$n - 1) * .data$se,
					NA_real_
				)
			)
	}

	# Build plot
	if (!is.null(group_var)) {
		p <- ggplot2::ggplot(
			summary_data,
			ggplot2::aes(
				x = .data[[x_var]],
				y = .data$mean_val,
				color = .data[[group_var]],
				group = .data[[group_var]]
			)
		)
		position <- ggplot2::position_dodge(width = dodge_width)
	} else {
		p <- ggplot2::ggplot(
			summary_data,
			ggplot2::aes(
				x = .data[[x_var]],
				y = .data$mean_val,
				group = 1
			)
		)
		position <- "identity"
	}

	# Add individual points if requested
	if (show_points) {
		if (!is.null(group_var)) {
			p <- p +
				ggplot2::geom_point(
					data = data,
					ggplot2::aes(
						x = .data[[x_var]],
						y = .data[[y_var]],
						color = .data[[group_var]]
					),
					alpha = 0.3,
					size = 1,
					position = position
				)
		} else {
			p <- p +
				ggplot2::geom_point(
					data = data,
					ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]),
					alpha = 0.3,
					size = 1
				)
		}
	}

	# Add error bars
	p <- p +
		ggplot2::geom_errorbar(
			ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
			width = error_bar_width,
			linewidth = line_size * 0.7,
			position = position
		)

	# Add lines and points
	p <- p +
		ggplot2::geom_line(linewidth = line_size, position = position) +
		ggplot2::geom_point(size = point_size, position = position)

	# Add sample sizes if requested
	if (show_n) {
		p <- p +
			ggplot2::geom_text(
				ggplot2::aes(
					y = .data$ci_lower,
					label = paste0("n=", .data$n)
				),
				vjust = 1.5,
				size = base_size / 3,
				position = position
			)
	}

	# Apply theme and labels
	p <- p + .pharmhand_theme(base_size = base_size)

	if (!is.null(title)) {
		p <- p + ggplot2::ggtitle(title)
	}

	p <- p +
		ggplot2::labs(
			x = if (!is.null(x_label)) x_label else x_var,
			y = if (!is.null(y_label)) y_label else y_var
		)

	# Apply color palette
	if (!is.null(group_var)) {
		colors <- .resolve_palette(palette)
		p <- p + ggplot2::scale_color_manual(values = colors)
	}

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = summary_data,
		type = "mean_plot",
		title = title,
		metadata = list(
			x_var = x_var,
			y_var = y_var,
			group_var = group_var,
			ci_level = ci_level
		)
	)
}

#' Create Spider Plot for Individual Trajectories
#'
#' Creates a spider plot showing individual patient trajectories over time,
#' commonly used for PRO data, tumor response, or other longitudinal measures.
#'
#' @param data Data frame containing longitudinal data
#' @param x_var Character. Variable for x-axis (visit/time)
#' @param y_var Character. Variable for y-axis (value or percent change)
#' @param subject_var Character. Subject ID variable. Default: "USUBJID"
#' @param group_var Character. Grouping variable for coloring. Default: NULL
#' @param reference_line Numeric. Y-value for reference line. Default: NULL
#' @param threshold_lines Numeric vector. Additional threshold
#'   lines. Default: NULL
#' @param highlight_subjects Character vector. Subject IDs to
#'   highlight. Default: NULL
#' @param title Character. Plot title. Default: NULL
#' @param x_label Character. X-axis label. Default: NULL
#' @param y_label Character. Y-axis label. Default: NULL
#' @param palette Character vector. Colors for groups. Default: NULL
#' @param alpha Numeric. Line transparency (0-1). Default: 0.6
#' @param base_size Numeric. Base font size. Default: 11
#' @param line_size Numeric. Line width. Default: 0.5
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Tumor response spider plot
#' data <- data.frame(
#'   USUBJID = rep(paste0("SUBJ", 1:20), each = 6),
#'   AVISITN = rep(0:5, 20),
#'   PCHG = c(replicate(20, cumsum(c(0, rnorm(5, mean = -5, sd = 15))))),
#'   TRT01P = rep(c("Treatment", "Placebo"), each = 60)
#' )
#'
#' plot <- create_spider_plot(
#'   data = data,
#'   x_var = "AVISITN",
#'   y_var = "PCHG",
#'   group_var = "TRT01P",
#'   reference_line = 0,
#'   threshold_lines = c(-30, 20),
#'   title = "Individual Tumor Response Over Time",
#'   y_label = "% Change from Baseline"
#' )
#' }
create_spider_plot <- function(
	data,
	x_var,
	y_var,
	subject_var = "USUBJID",
	group_var = NULL,
	reference_line = NULL,
	threshold_lines = NULL,
	highlight_subjects = NULL,
	title = NULL,
	x_label = NULL,
	y_label = NULL,
	palette = NULL,
	alpha = 0.6,
	base_size = 11,
	line_size = 0.5
) {
	# Validate inputs
	admiraldev::assert_data_frame(data)
	admiraldev::assert_character_scalar(x_var)
	admiraldev::assert_character_scalar(y_var)
	admiraldev::assert_character_scalar(subject_var)

	required_vars <- c(x_var, y_var, subject_var)
	if (!is.null(group_var)) {
		required_vars <- c(required_vars, group_var)
	}

	missing_vars <- setdiff(required_vars, names(data))
	if (length(missing_vars) > 0) {
		ph_abort(sprintf(
			"Variables not found in data: %s",
			paste(missing_vars, collapse = ", ")
		))
	}

	# Build base plot
	if (!is.null(group_var)) {
		p <- ggplot2::ggplot(
			data,
			ggplot2::aes(
				x = .data[[x_var]],
				y = .data[[y_var]],
				group = .data[[subject_var]],
				color = .data[[group_var]]
			)
		)
	} else {
		p <- ggplot2::ggplot(
			data,
			ggplot2::aes(
				x = .data[[x_var]],
				y = .data[[y_var]],
				group = .data[[subject_var]]
			)
		)
	}

	# Add reference line if specified
	if (!is.null(reference_line)) {
		p <- p +
			ggplot2::geom_hline(
				yintercept = reference_line,
				linetype = "solid",
				color = "gray40",
				linewidth = 0.8
			)
	}

	# Add threshold lines if specified
	if (!is.null(threshold_lines)) {
		for (thresh in threshold_lines) {
			p <- p +
				ggplot2::geom_hline(
					yintercept = thresh,
					linetype = "dashed",
					color = "gray60",
					linewidth = 0.5
				)
		}
	}

	# Add individual lines
	if (!is.null(highlight_subjects)) {
		# Non-highlighted subjects in gray
		data_other <- dplyr::filter(
			data,
			!(.data[[subject_var]] %in% highlight_subjects)
		)
		data_highlight <- dplyr::filter(
			data,
			.data[[subject_var]] %in% highlight_subjects
		)

		p <- p +
			ggplot2::geom_line(
				data = data_other,
				alpha = alpha * 0.5,
				linewidth = line_size,
				color = "gray70"
			)

		p <- p +
			ggplot2::geom_line(
				data = data_highlight,
				alpha = 1,
				linewidth = line_size * 1.5
			)
	} else {
		p <- p +
			ggplot2::geom_line(
				alpha = alpha,
				linewidth = line_size
			)
	}

	# Apply theme
	p <- p + .pharmhand_theme(base_size = base_size)

	# Labels
	if (!is.null(title)) {
		p <- p + ggplot2::ggtitle(title)
	}

	p <- p +
		ggplot2::labs(
			x = if (!is.null(x_label)) x_label else x_var,
			y = if (!is.null(y_label)) y_label else y_var
		)

	# Apply color palette
	if (!is.null(group_var)) {
		colors <- .resolve_palette(palette)
		p <- p + ggplot2::scale_color_manual(values = colors)
	}

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = data,
		type = "spider_plot",
		title = title,
		metadata = list(
			x_var = x_var,
			y_var = y_var,
			subject_var = subject_var,
			group_var = group_var,
			n_subjects = length(unique(data[[subject_var]]))
		)
	)
}
