#' @title Meta-Analysis Visualization Functions
#' @name meta_plots
#' @description Functions for creating forest plots, funnel plots, and
#'   network plots for meta-analysis.
NULL

#' Create Forest Plot for Meta-Analysis
#'
#' Creates a forest plot displaying individual study effects and pooled estimate
#' from a meta-analysis.
#'
#' @param meta_result A MetaResult object from meta_analysis()
#' @param title Character. Plot title. Default: NULL
#' @param xlab Character. X-axis label. Default: based on effect_measure
#' @param show_weights Logical. Show study weights. Default: TRUE
#' @param show_heterogeneity Logical. Show heterogeneity stats. Default: TRUE
#' @param show_prediction Logical. Show prediction interval. Default: TRUE
#' @param null_value Numeric. Reference line value. Default: 1 for ratios,
#'   0 for differences
#' @param xlim Numeric vector. X-axis limits. Default: NULL (auto)
#' @param palette Character vector. Colors. Default: NULL
#' @param base_size Numeric. Base font size. Default: 11
#'
#' @return A ClinicalPlot object containing the forest plot
#' @export
#'
#' @examples
#' # Meta-analysis forest plot
#' yi <- log(c(0.75, 0.82, 0.68, 0.91))
#' sei <- c(0.12, 0.15, 0.18, 0.14)
#' result <- meta_analysis(
#'   yi = yi, sei = sei,
#'   study_labels = c("Study A", "Study B", "Study C", "Study D"),
#'   effect_measure = "hr"
#' )
#' plot <- create_meta_forest_plot(result, title = "Treatment Effect")
#' plot@type
create_meta_forest_plot <- function(
	meta_result,
	title = NULL,
	xlab = NULL,
	show_weights = TRUE,
	show_heterogeneity = TRUE,
	show_prediction = TRUE,
	null_value = NULL,
	xlim = NULL,
	palette = NULL,
	base_size = 11
) {
	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	effect_measure <- meta_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Set defaults based on effect measure
	if (is.null(null_value)) {
		null_value <- if (is_ratio) 1 else 0
	}

	if (is.null(xlab)) {
		xlab <- switch(
			effect_measure,
			"hr" = "Hazard Ratio",
			"or" = "Odds Ratio",
			"rr" = "Risk Ratio",
			"rd" = "Risk Difference",
			"md" = "Mean Difference",
			"smd" = "Standardized Mean Difference",
			"Effect Size"
		)
	}

	# Extract study data
	study_results <- meta_result@study_results
	study_labels <- names(study_results)
	k <- length(study_results)
	weights <- meta_result@weights

	# Build data frame for plotting
	plot_data <- data.frame(
		study = study_labels,
		estimate = sapply(study_results, function(x) x@estimate),
		ci_lower = sapply(study_results, function(x) x@ci[1]),
		ci_upper = sapply(study_results, function(x) x@ci[2]),
		weight = as.numeric(weights) * 100,
		stringsAsFactors = FALSE
	)

	# Add pooled estimate row
	pooled_row <- data.frame(
		study = "Overall",
		estimate = meta_result@estimate,
		ci_lower = meta_result@ci[1],
		ci_upper = meta_result@ci[2],
		weight = 100,
		stringsAsFactors = FALSE
	)

	plot_data <- rbind(plot_data, pooled_row)
	plot_data$is_pooled <- c(rep(FALSE, k), TRUE)
	plot_data$row <- seq_len(nrow(plot_data))

	# Set y-axis order (pooled at bottom)
	plot_data$y_pos <- rev(seq_len(nrow(plot_data)))

	# Transform to log scale for ratios
	plot_data$x_plot <- plot_data$estimate
	plot_data$x_lower <- plot_data$ci_lower
	plot_data$x_upper <- plot_data$ci_upper
	x_trans <- if (is_ratio) "log10" else "identity"

	# Calculate plot limits
	if (is.null(xlim)) {
		all_vals <- c(plot_data$x_lower, plot_data$x_upper)
		all_vals <- all_vals[is.finite(all_vals)]
		xlim <- range(all_vals)
		# Expand slightly
		xlim <- xlim + c(-1, 1) * diff(xlim) * 0.1
	}

	# Build plot
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$x_plot, y = .data$y_pos)
	)

	# Reference line
	p <- p +
		ggplot2::geom_vline(
			xintercept = null_value,
			linetype = "dashed",
			color = "gray50"
		)

	# Prediction interval (if available and requested)
	if (show_prediction && !is.null(meta_result@prediction_interval)) {
		pred <- meta_result@prediction_interval
		pooled_y <- plot_data$y_pos[plot_data$is_pooled]
		p <- p +
			ggplot2::geom_segment(
				data = data.frame(
					x = pred[1],
					xend = pred[2],
					y = pooled_y - 0.3,
					yend = pooled_y - 0.3
				),
				ggplot2::aes(
					x = .data$x,
					xend = .data$xend,
					y = .data$y,
					yend = .data$yend
				),
				color = "gray60",
				linewidth = 0.5
			)
	}

	# Error bars
	p <- p +
		ggplot2::geom_errorbar(
			ggplot2::aes(xmin = .data$x_lower, xmax = .data$x_upper),
			height = 0.2,
			linewidth = 0.6,
			orientation = "y"
		)

	# Points (diamonds for pooled, squares for studies)
	study_data <- plot_data[!plot_data$is_pooled, ]
	pooled_data <- plot_data[plot_data$is_pooled, ]

	# Study points (size proportional to weight)
	p <- p +
		ggplot2::geom_point(
			data = study_data,
			ggplot2::aes(size = .data$weight),
			shape = 15,
			color = "black"
		)

	# Pooled estimate (diamond)
	p <- p +
		ggplot2::geom_point(
			data = pooled_data,
			shape = 23,
			size = 5,
			fill = "black",
			color = "black"
		)

	# Y-axis labels
	p <- p +
		ggplot2::scale_y_continuous(
			breaks = plot_data$y_pos,
			labels = plot_data$study,
			expand = ggplot2::expansion(mult = 0.05)
		)

	# X-axis transformation
	if (is_ratio) {
		p <- p + ggplot2::scale_x_log10(limits = xlim)
	} else {
		p <- p + ggplot2::scale_x_continuous(limits = xlim)
	}

	# Size scale
	p <- p +
		ggplot2::scale_size_continuous(
			range = c(1, 5),
			guide = if (show_weights) "legend" else "none"
		)

	# Labels
	p <- p +
		ggplot2::labs(
			x = xlab,
			y = NULL,
			title = title,
			size = "Weight (%)"
		)

	# Theme
	p <- p + .pharmhand_theme(base_size = base_size)
	p <- p +
		ggplot2::theme(
			axis.text.y = ggplot2::element_text(hjust = 0),
			panel.grid.major.y = ggplot2::element_blank()
		)

	# Add heterogeneity annotation
	if (show_heterogeneity) {
		het <- meta_result@heterogeneity
		het_text <- sprintf(
			"I2 = %.1f%%, tau2 = %.3f, Q = %.1f (p %s)",
			het$I2,
			het$tau2,
			het$Q,
			format_pvalue(het$Q_pvalue)
		)
		p <- p + ggplot2::labs(caption = het_text)
	}

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = plot_data,
		type = "forest_meta",
		title = title,
		metadata = list(
			effect_measure = effect_measure,
			model = meta_result@model,
			k = k
		)
	)
}

#' Create Funnel Plot for Publication Bias Assessment
#'
#' Creates a funnel plot to visually assess publication bias in meta-analysis.
#' Studies are plotted by effect size vs. precision (1/SE).
#'
#' @param meta_result A MetaResult object from meta_analysis()
#' @param show_ci Logical. Show pseudo 95% CI region. Default: TRUE
#' @param show_egger Logical. Show Egger's regression line. Default: TRUE
#' @param title Character. Plot title. Default: "Funnel Plot"
#' @param xlab Character. X-axis label. Default: based on effect_measure
#' @param ylab Character. Y-axis label. Default: "Standard Error"
#' @param contour_levels Numeric vector. Contour p-value levels.
#'   Default: c(0.1, 0.05, 0.01)
#' @param base_size Numeric. Base font size. Default: 11
#'
#' @return A ClinicalPlot object containing the funnel plot
#' @export
#'
#' @examples
#' # Funnel plot for publication bias assessment
#' yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
#' sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)
#' meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
#' plot <- create_funnel_plot(meta_res, title = "Funnel Plot")
#' plot@type
create_funnel_plot <- function(
	meta_result,
	show_ci = TRUE,
	show_egger = TRUE,
	title = "Funnel Plot",
	xlab = NULL,
	ylab = "Standard Error",
	contour_levels = c(0.1, 0.05, 0.01),
	base_size = 11
) {
	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	effect_measure <- meta_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Get data from MetaResult metadata
	yi <- meta_result@metadata$yi
	sei <- meta_result@metadata$sei
	study_labels <- meta_result@metadata$study_labels

	if (is.null(yi) || is.null(sei)) {
		ph_abort("MetaResult must contain yi and sei in metadata")
	}

	# Pooled estimate (on log scale for ratios)
	if (is_ratio) {
		pooled <- log(meta_result@estimate)
	} else {
		pooled <- meta_result@estimate
	}

	# Set x-axis label
	if (is.null(xlab)) {
		xlab <- switch(
			effect_measure,
			"hr" = "Log Hazard Ratio",
			"or" = "Log Odds Ratio",
			"rr" = "Log Risk Ratio",
			"rd" = "Risk Difference",
			"md" = "Mean Difference",
			"smd" = "Standardized Mean Difference",
			"Effect Size"
		)
	}

	# Create plot data
	plot_data <- data.frame(
		study = study_labels,
		effect = yi,
		se = sei,
		precision = 1 / sei,
		stringsAsFactors = FALSE
	)

	# Perform Egger's test
	egger <- eggers_test(yi = yi, sei = sei)

	# Build funnel plot
	p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$effect, y = .data$se))

	# Add contour regions if requested
	if (show_ci) {
		# Create data for pseudo-confidence regions
		se_range <- c(0, max(sei) * 1.1)
		se_seq <- seq(0.001, se_range[2], length.out = 100)

		ci_data <- data.frame(
			se = se_seq,
			lower_95 = pooled - 1.96 * se_seq,
			upper_95 = pooled + 1.96 * se_seq
		)

		p <- p +
			ggplot2::geom_polygon(
				data = data.frame(
					x = c(pooled, ci_data$lower_95, rev(ci_data$upper_95)),
					y = c(0, ci_data$se, rev(ci_data$se))
				),
				ggplot2::aes(x = .data$x, y = .data$y),
				fill = "white",
				color = "gray70",
				linetype = "dashed",
				alpha = 0.5
			)
	}

	# Vertical line at pooled estimate
	p <- p +
		ggplot2::geom_vline(
			xintercept = pooled,
			linetype = "solid",
			color = "gray40"
		)

	# Egger's regression line
	if (show_egger && !is.na(egger$intercept)) {
		# Line: effect = intercept * se + slope (where slope approximates pooled)
		se_range <- range(plot_data$se)
		egger_line <- data.frame(
			se = se_range,
			effect = egger$slope + egger$intercept * se_range
		)
		p <- p +
			ggplot2::geom_line(
				data = egger_line,
				ggplot2::aes(x = .data$effect, y = .data$se),
				color = "red",
				linetype = "dotted",
				linewidth = 1
			)
	}

	# Study points
	p <- p + ggplot2::geom_point(size = 3, shape = 19)

	# Reverse y-axis (smaller SE at top)
	p <- p + ggplot2::scale_y_reverse()

	# Labels
	p <- p +
		ggplot2::labs(
			x = xlab,
			y = ylab,
			title = title
		)

	# Theme
	p <- p + .pharmhand_theme(base_size = base_size)

	# Add Egger's test result as caption
	if (show_egger) {
		egger_text <- sprintf(
			"Egger's test: intercept = %.2f, t = %.2f, p %s",
			egger$intercept,
			egger$t_value,
			format_pvalue(egger$p_value)
		)
		p <- p + ggplot2::labs(caption = egger_text)
	}

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = plot_data,
		type = "funnel",
		title = title,
		metadata = list(
			effect_measure = effect_measure,
			egger_test = egger,
			pooled_estimate = pooled
		)
	)
}

#' Visualize Network Geometry
#'
#' Creates a network graph visualization showing the structure of treatment
#' comparisons in a network meta-analysis.
#'
#' @param nma_result Result from network_meta() or data with network structure
#' @param node_size Character. Size nodes by "equal", "n_studies", or
#'   "n_patients". Default: "equal"
#' @param edge_width Character. Width edges by "equal" or "n_studies".
#'   Default: "n_studies"
#' @param show_labels Logical. Show edge labels with study counts. Default: TRUE
#' @param highlight_ref Logical. Highlight reference treatment. Default: TRUE
#' @param title Character. Plot title. Default: "Network Geometry"
#' @param layout Character. Layout algorithm: "circle", "star",
#'   "auto". Default: "circle"
#' @param palette Character vector. Node colors. Default: NULL
#' @param base_size Numeric. Base font size. Default: 11
#'
#' @return A ClinicalPlot object containing the network graph
#' @export
#'
#' @examples
#' # Network geometry visualization
#' nma_data <- data.frame(
#'   study = c("S1", "S2", "S3"),
#'   treat1 = c("A", "B", "A"),
#'   treat2 = c("B", "C", "C"),
#'   effect = log(c(0.75, 0.90, 0.80)),
#'   se = c(0.12, 0.15, 0.18)
#' )
#' nma_result <- network_meta(nma_data, effect_measure = "hr")
#' plot <- create_network_plot(nma_result, title = "Treatment Network")
#' plot@type
create_network_plot <- function(
	nma_result,
	node_size = c("equal", "n_studies", "n_patients"),
	edge_width = c("n_studies", "equal"),
	show_labels = TRUE,
	highlight_ref = TRUE,
	title = "Network Geometry",
	layout = c("circle", "star", "auto"),
	palette = NULL,
	base_size = 11
) {
	node_size <- match.arg(node_size)
	edge_width <- match.arg(edge_width)
	layout <- match.arg(layout)

	# Extract network structure
	if (S7::S7_inherits(nma_result, NMAResult)) {
		network <- nma_result@network
		treatments <- network$treatments
		edges <- network$edges
		reference <- network$reference
	} else {
		ph_abort("nma_result must be an NMAResult object from network_meta()")
	}

	n_nodes <- length(treatments)

	# Calculate node positions based on layout
	if (layout == "circle") {
		angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[1:n_nodes]
		node_x <- cos(angles)
		node_y <- sin(angles)
	} else if (layout == "star") {
		# Reference in center, others around
		ref_idx <- which(treatments == reference)
		other_idx <- setdiff(seq_along(treatments), ref_idx)
		n_other <- length(other_idx)

		node_x <- numeric(n_nodes)
		node_y <- numeric(n_nodes)
		node_x[ref_idx] <- 0
		node_y[ref_idx] <- 0

		angles <- seq(0, 2 * pi, length.out = n_other + 1)[1:n_other]
		node_x[other_idx] <- cos(angles)
		node_y[other_idx] <- sin(angles)
	} else {
		# Auto: place nodes on a circle (circular layout)
		angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[1:n_nodes]
		node_x <- cos(angles)
		node_y <- sin(angles)
	}

	# Node data
	node_data <- data.frame(
		treatment = treatments,
		x = node_x,
		y = node_y,
		is_reference = treatments == reference,
		stringsAsFactors = FALSE
	)

	# Calculate node sizes
	if (node_size == "n_studies") {
		# Count studies involving each treatment
		node_data$n_studies <- vapply(
			treatments,
			function(t) {
				sum(edges$treat1 == t | edges$treat2 == t)
			},
			integer(1)
		)
		node_data$size <- 3 +
			5 * (node_data$n_studies / max(1, max(node_data$n_studies)))
	} else {
		node_data$size <- 5
	}

	# Edge data
	edge_data <- data.frame(
		treat1 = edges$treat1,
		treat2 = edges$treat2,
		n_studies = edges$n_studies,
		stringsAsFactors = FALSE
	)

	# Add coordinates
	edge_data$x1 <- node_data$x[match(edge_data$treat1, node_data$treatment)]
	edge_data$y1 <- node_data$y[match(edge_data$treat1, node_data$treatment)]
	edge_data$x2 <- node_data$x[match(edge_data$treat2, node_data$treatment)]
	edge_data$y2 <- node_data$y[match(edge_data$treat2, node_data$treatment)]

	# Edge midpoints for labels
	edge_data$xmid <- (edge_data$x1 + edge_data$x2) / 2
	edge_data$ymid <- (edge_data$y1 + edge_data$y2) / 2

	# Edge widths
	if (edge_width == "n_studies") {
		max_n <- max(edge_data$n_studies)
		if (max_n == 0) {
			edge_data$width <- 0.5
		} else {
			edge_data$width <- 0.5 + 2 * (edge_data$n_studies / max_n)
		}
	} else {
		edge_data$width <- 1
	}

	# Build plot
	p <- ggplot2::ggplot()

	# Draw edges
	p <- p +
		ggplot2::geom_segment(
			data = edge_data,
			ggplot2::aes(
				x = .data$x1,
				y = .data$y1,
				xend = .data$x2,
				yend = .data$y2,
				linewidth = .data$width
			),
			color = "gray60"
		)

	# Edge labels (number of studies)
	if (show_labels) {
		p <- p +
			ggplot2::geom_label(
				data = edge_data,
				ggplot2::aes(
					x = .data$xmid,
					y = .data$ymid,
					label = .data$n_studies
				),
				size = base_size / 4,
				fill = "white",
				label.padding = ggplot2::unit(0.15, "lines")
			)
	}

	# Draw nodes
	if (highlight_ref) {
		# Non-reference nodes
		p <- p +
			ggplot2::geom_point(
				data = node_data[!node_data$is_reference, ],
				ggplot2::aes(x = .data$x, y = .data$y, size = .data$size),
				color = "steelblue",
				fill = "steelblue",
				shape = 21
			)

		# Reference node (highlighted)
		p <- p +
			ggplot2::geom_point(
				data = node_data[node_data$is_reference, ],
				ggplot2::aes(x = .data$x, y = .data$y, size = .data$size),
				color = "darkred",
				fill = "darkred",
				shape = 21
			)
	} else {
		p <- p +
			ggplot2::geom_point(
				data = node_data,
				ggplot2::aes(x = .data$x, y = .data$y, size = .data$size),
				color = "steelblue",
				fill = "steelblue",
				shape = 21
			)
	}

	# Node labels (treatment names)
	p <- p +
		ggplot2::geom_text(
			data = node_data,
			ggplot2::aes(
				x = .data$x,
				y = .data$y,
				label = .data$treatment
			),
			size = base_size / 3,
			fontface = "bold",
			nudge_x = 0.15,
			nudge_y = 0.15
		)

	# Remove axes and apply theme
	p <- p +
		ggplot2::coord_fixed() +
		ggplot2::scale_size_identity() +
		ggplot2::scale_linewidth_identity() +
		ggplot2::theme_void(base_size = base_size) +
		ggplot2::theme(
			plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
			legend.position = "none"
		) +
		ggplot2::ggtitle(title)

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = list(nodes = node_data, edges = edge_data),
		type = "network_geometry",
		title = title,
		metadata = list(
			n_treatments = n_nodes,
			n_edges = nrow(edge_data),
			reference = reference
		)
	)
}
