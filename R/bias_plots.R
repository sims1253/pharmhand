#' Risk of Bias Visualization Functions
#'
#' Functions for visualizing risk of bias assessments using traffic light
#' plots and summary plots.
#'
#' @name bias_plots
#' @references
#'   McGuinness LA, Higgins JPT (2020). Risk-of-bias VISualization (robvis).
#'   R package version 0.5.1. https://github.com/mcguinlu/robvis
NULL

#' Standard Risk of Bias Color Palette
#'
#' Color palette following robvis conventions for risk of bias judgments.
#' Compatible with both RoB 2 (Low/Some concerns/High) and ROBINS-I
#' (Low/Moderate/Serious/Critical/No information) tools.
#'
#' @keywords internal
.ROB_COLORS <- c(
	"Low" = "#009e73",
	"Some concerns" = "#f0e442",
	"Moderate" = "#f0e442",
	"High" = "#d55e00",
	"Serious" = "#d55e00",
	"Critical" = "#cc0000",
	"No information" = "#999999"
)

#' RoB 2 Domain Labels
#'
#' @keywords internal
.ROB2_DOMAIN_LABELS <- c(
	"D1_randomization" = "D1: Randomization",
	"D2_deviations" = "D2: Deviations",
	"D3_missing_data" = "D3: Missing data",
	"D4_measurement" = "D4: Measurement",
	"D5_selection" = "D5: Selection"
)

#' ROBINS-I Domain Labels
#'
#' @keywords internal
.ROBINSI_DOMAIN_LABELS <- c(
	"D1_confounding" = "D1: Confounding",
	"D2_selection" = "D2: Selection",
	"D3_classification" = "D3: Classification",
	"D4_deviations" = "D4: Deviations",
	"D5_missing_data" = "D5: Missing data",
	"D6_measurement" = "D6: Measurement",
	"D7_selection_report" = "D7: Selection"
)

#' Create Traffic Light Plot for Risk of Bias
#'
#' Creates a traffic light plot showing domain-level risk of bias judgments
#' for each study. Works with both RoB 2 and ROBINS-I assessment results.
#'
#' The traffic light plot displays individual study assessments with colored
#' squares for each domain, following the robvis convention:
#' \itemize{
#'   \item Green: Low risk
#'   \item Yellow: Some concerns (RoB 2) / Moderate (ROBINS-I)
#'   \item Orange: Serious (ROBINS-I)
#'   \item Red: High risk (RoB 2) / Critical (ROBINS-I)
#'   \item Gray: No information
#' }
#'
#' @param results List of RoB2Result or ROBINSIResult objects
#' @param title Plot title. Default: "Risk of Bias Assessment"
#' @param show_overall Logical. Include overall judgment column. Default: TRUE
#' @param base_size Base font size. Default: 11
#' @param colors Named character vector of colors for judgments.
#'   If NULL, uses the standard .ROB_COLORS palette. Default: NULL
#'
#' @return A ClinicalPlot object containing the traffic light plot
#' @export
#'
#' @examples
#' \dontrun{
#' # RoB 2 example
#' rob2_results <- list(
#'   assess_rob2(
#'     study_id = "STUDY001",
#'     d1_randomization = "Low",
#'     d2_deviations = "Low",
#'     d3_missing_data = "Low",
#'     d4_measurement = "Some concerns",
#'     d5_selection = "Low",
#'     outcome = "Overall Survival"
#'   ),
#'   assess_rob2(
#'     study_id = "STUDY002",
#'     d1_randomization = "Low",
#'     d2_deviations = "Low",
#'     d3_missing_data = "High",
#'     d4_measurement = "Low",
#'     d5_selection = "Low",
#'     outcome = "Overall Survival"
#'   )
#' )
#'
#' plot <- create_rob_traffic_light_plot(rob2_results)
#' plot(plot)
#'
#' # ROBINS-I example
#' robinsi_results <- list(
#'   assess_robins_i(
#'     study_id = "OBS001",
#'     d1_confounding = "Serious",
#'     d2_selection = "Low",
#'     d3_classification = "Low",
#'     d4_deviations = "Low",
#'     d5_missing_data = "Low",
#'     d6_measurement = "Moderate",
#'     d7_selection_report = "Low",
#'     outcome = "Mortality"
#'   )
#' )
#'
#' plot <- create_rob_traffic_light_plot(robinsi_results)
#' }
create_rob_traffic_light_plot <- function(
	results,
	title = "Risk of Bias Assessment",
	show_overall = TRUE,
	base_size = 11,
	colors = NULL
) {
	# Validate inputs
	if (!is.list(results) || length(results) == 0) {
		ph_abort(
			"results must be a non-empty list of RoB2Result or ROBINSIResult objects"
		)
	}

	# Check all elements are valid result objects
	is_rob2 <- all(vapply(
		results,
		function(r) S7::S7_inherits(r, RoB2Result),
		logical(1)
	))
	is_robinsi <- all(vapply(
		results,
		function(r) S7::S7_inherits(r, ROBINSIResult),
		logical(1)
	))

	if (!is_rob2 && !is_robinsi) {
		ph_abort(
			"All elements in results must be RoB2Result or ROBINSIResult objects"
		)
	}

	if (is_rob2 && is_robinsi) {
		ph_abort("results cannot contain both RoB2Result and ROBINSIResult objects")
	}

	# Determine domains and labels based on tool type
	if (is_rob2) {
		domains <- ROB2_DOMAINS
		domain_labels <- .ROB2_DOMAIN_LABELS
	} else {
		domains <- ROBINSI_DOMAINS
		domain_labels <- .ROBINSI_DOMAIN_LABELS
	}

	# Resolve colors
	if (is.null(colors)) {
		colors <- .ROB_COLORS
	}

	# Build plot data frame
	plot_data_list <- lapply(results, function(r) {
		study_id <- r@study_id
		overall <- r@overall

		# Extract domain judgments
		judgments <- vapply(
			domains,
			function(d) r@domains[[d]]$judgment,
			character(1)
		)

		# Create data frame for this study
		study_df <- data.frame(
			study_id = study_id,
			domain = domains,
			domain_label = unname(domain_labels[domains]),
			judgment = judgments,
			overall = overall,
			stringsAsFactors = FALSE
		)

		study_df
	})

	plot_df <- do.call(rbind, plot_data_list)

	# Order domains (domains as rows, studies as columns in display)
	plot_df$domain <- factor(
		plot_df$domain,
		levels = domains,
		ordered = TRUE
	)

	plot_df$domain_label <- factor(
		plot_df$domain_label,
		levels = unique(domain_labels[domains]),
		ordered = TRUE
	)

	# Order studies
	study_order <- unique(plot_df$study_id)
	plot_df$study_id <- factor(
		plot_df$study_id,
		levels = study_order,
		ordered = TRUE
	)

	# Create judgment factor with all levels
	all_judgments <- unique(c(names(colors), plot_df$judgment))
	plot_df$judgment <- factor(
		plot_df$judgment,
		levels = all_judgments,
		ordered = TRUE
	)

	# Determine plot dimensions
	n_studies <- length(study_order)
	n_domains <- length(domains)

	# Set width based on whether overall is shown
	width <- if (isTRUE(show_overall)) {
		max(6, n_studies * 0.8 + 2)
	} else {
		max(4, n_studies * 0.8)
	}
	height <- max(4, n_domains * 0.8 + 1)

	# Create the traffic light plot using geom_tile
	p <- ggplot2::ggplot(
		plot_df,
		ggplot2::aes(
			x = .data$study_id,
			y = .data$domain_label,
			fill = .data$judgment
		)
	) +
		ggplot2::geom_tile(color = "white", linewidth = 0.5) +
		ggplot2::scale_fill_manual(
			values = colors,
			drop = FALSE,
			na.value = "grey90"
		) +
		.pharmhand_theme(base_size = base_size) +
		ggplot2::theme(
			axis.text.x = ggplot2::element_text(
				angle = 45,
				hjust = 1,
				vjust = 1
			),
			axis.text.y = ggplot2::element_text(hjust = 0.5),
			plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
			legend.position = "right",
			panel.grid = ggplot2::element_blank(),
			aspect.ratio = NULL
		) +
		ggplot2::labs(
			title = title,
			x = NULL,
			y = NULL,
			fill = "Risk of Bias"
		)

	# Add overall column if requested
	if (isTRUE(show_overall)) {
		# Get overall judgments
		overall_df <- unique(plot_df[, c("study_id", "overall")])

		# Create separate data frame for overall column
		overall_plot_df <- data.frame(
			study_id = overall_df$study_id,
			domain = "overall",
			domain_label = "Overall",
			judgment = overall_df$overall,
			overall = overall_df$overall,
			stringsAsFactors = FALSE
		)

		overall_plot_df$judgment <- factor(
			overall_plot_df$judgment,
			levels = all_judgments,
			ordered = TRUE
		)
		overall_plot_df$domain_label <- factor(
			overall_plot_df$domain_label,
			levels = c(levels(plot_df$domain_label), "Overall"),
			ordered = TRUE
		)

		# Combine with main data
		combined_df <- rbind(
			plot_df,
			overall_plot_df
		)

		# Recreate plot with combined data
		p <- ggplot2::ggplot(
			combined_df,
			ggplot2::aes(
				x = .data$study_id,
				y = .data$domain_label,
				fill = .data$judgment
			)
		) +
			ggplot2::geom_tile(color = "white", linewidth = 0.5) +
			ggplot2::scale_fill_manual(
				values = colors,
				drop = FALSE,
				na.value = "grey90"
			) +
			.pharmhand_theme(base_size = base_size) +
			ggplot2::theme(
				axis.text.x = ggplot2::element_text(
					angle = 45,
					hjust = 1,
					vjust = 1
				),
				axis.text.y = ggplot2::element_text(hjust = 0.5),
				plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
				legend.position = "right",
				panel.grid = ggplot2::element_blank()
			) +
			ggplot2::labs(
				title = title,
				x = NULL,
				y = NULL,
				fill = "Risk of Bias"
			)
	}

	# Return ClinicalPlot object with correct data
	final_data <- if (isTRUE(show_overall)) combined_df else plot_df

	ClinicalPlot(
		plot = p,
		data = final_data,
		type = "rob_traffic_light",
		title = title,
		width = width,
		height = height,
		dpi = 300,
		metadata = list(
			tool = if (is_rob2) "RoB 2" else "ROBINS-I",
			domains = domains,
			n_studies = n_studies,
			show_overall = show_overall
		)
	)
}


#' Create Summary Plot for Risk of Bias
#'
#' Creates a stacked bar plot showing the proportion of studies at each
#' risk level for each domain. This provides an overview of risk of bias
#' distribution across the evidence base.
#'
#' @param results List of RoB2Result or ROBINSIResult objects
#' @param title Plot title. Default: "Risk of Bias Summary"
#' @param weighted Logical. Weight by sample size if available in metadata.
#'   Currently not implemented - included for future extension. Default: FALSE
#' @param base_size Base font size. Default: 11
#' @param colors Named character vector of colors for judgments.
#'   If NULL, uses the standard .ROB_COLORS palette. Default: NULL
#' @param horizontal Logical. Display bars horizontally (domains on y-axis).
#'   Default: TRUE
#'
#' @return A ClinicalPlot object containing the summary plot
#' @export
#'
#' @examples
#' \dontrun{
#' # RoB 2 example
#' rob2_results <- list(
#'   assess_rob2(
#'     study_id = "STUDY001",
#'     d1_randomization = "Low",
#'     d2_deviations = "Low",
#'     d3_missing_data = "Low",
#'     d4_measurement = "Some concerns",
#'     d5_selection = "Low",
#'     outcome = "OS"
#'   ),
#'   assess_rob2(
#'     study_id = "STUDY002",
#'     d1_randomization = "Low",
#'     d2_deviations = "Low",
#'     d3_missing_data = "High",
#'     d4_measurement = "Low",
#'     d5_selection = "Low",
#'     outcome = "OS"
#'   ),
#'   assess_rob2(
#'     study_id = "STUDY003",
#'     d1_randomization = "Some concerns",
#'     d2_deviations = "Some concerns",
#'     d3_missing_data = "Low",
#'     d4_measurement = "Low",
#'     d5_selection = "Low",
#'     outcome = "PFS"
#'   )
#' )
#'
#' plot <- create_rob_summary_plot(rob2_results)
#' plot(plot)
#' }
create_rob_summary_plot <- function(
	results,
	title = "Risk of Bias Summary",
	weighted = FALSE,
	base_size = 11,
	colors = NULL,
	horizontal = TRUE
) {
	# Validate inputs
	if (!is.list(results) || length(results) == 0) {
		ph_abort(
			"results must be a non-empty list of RoB2Result or ROBINSIResult objects"
		)
	}

	# Warn if weighted = TRUE (not implemented)
	if (isTRUE(weighted)) {
		ph_warn("weighted = TRUE is not implemented; results will be unweighted")
	}

	# Check all elements are valid result objects
	is_rob2 <- all(vapply(
		results,
		function(r) S7::S7_inherits(r, RoB2Result),
		logical(1)
	))
	is_robinsi <- all(vapply(
		results,
		function(r) S7::S7_inherits(r, ROBINSIResult),
		logical(1)
	))

	if (!is_rob2 && !is_robinsi) {
		ph_abort(
			"All elements in results must be RoB2Result or ROBINSIResult objects"
		)
	}

	if (is_rob2 && is_robinsi) {
		ph_abort("results cannot contain both RoB2Result and ROBINSIResult objects")
	}

	# Determine domains based on tool type
	if (is_rob2) {
		domains <- ROB2_DOMAINS
		domain_labels <- .ROB2_DOMAIN_LABELS
	} else {
		domains <- ROBINSI_DOMAINS
		domain_labels <- .ROBINSI_DOMAIN_LABELS
	}

	# Resolve colors
	if (is.null(colors)) {
		colors <- .ROB_COLORS
	}

	# Build summary data
	summary_list <- lapply(results, function(r) {
		judgments <- vapply(
			domains,
			function(d) r@domains[[d]]$judgment,
			character(1)
		)

		data.frame(
			study_id = r@study_id,
			domain = domains,
			domain_label = unname(domain_labels[domains]),
			judgment = judgments,
			stringsAsFactors = FALSE
		)
	})

	summary_df <- do.call(rbind, summary_list)

	# Calculate proportions per domain
	proportions <- summary_df |>
		dplyr::summarise(
			n = dplyr::n(),
			.by = c("domain", "domain_label", "judgment")
		) |>
		dplyr::mutate(
			proportion = .data$n / sum(.data$n),
			.by = c("domain", "domain_label")
		)

	# Get all possible judgments for color scale
	all_judgments <- names(colors)

	proportions$judgment <- factor(
		proportions$judgment,
		levels = all_judgments,
		ordered = TRUE
	)

	proportions$domain_label <- factor(
		proportions$domain_label,
		levels = unique(unname(domain_labels[domains])),
		ordered = TRUE
	)

	# Determine dimensions
	n_domains <- length(domains)
	width <- if (isTRUE(horizontal)) max(6, n_domains * 1.2) else 8
	height <- if (isTRUE(horizontal)) 5 else max(4, n_domains * 0.8)

	# Create the summary plot
	if (isTRUE(horizontal)) {
		p <- ggplot2::ggplot(
			proportions,
			ggplot2::aes(
				x = .data$domain_label,
				y = .data$proportion,
				fill = .data$judgment
			)
		) +
			ggplot2::geom_bar(
				stat = "identity",
				position = ggplot2::position_fill(reverse = TRUE),
				color = "white",
				linewidth = 0.3
			) +
			ggplot2::coord_flip()
	} else {
		p <- ggplot2::ggplot(
			proportions,
			ggplot2::aes(
				x = .data$domain_label,
				y = .data$proportion,
				fill = .data$judgment
			)
		) +
			ggplot2::geom_bar(
				stat = "identity",
				position = ggplot2::position_fill(reverse = TRUE),
				color = "white",
				linewidth = 0.3
			)
	}

	p <- p +
		ggplot2::scale_fill_manual(
			values = colors,
			drop = FALSE,
			na.value = "grey90"
		) +
		ggplot2::scale_y_continuous(
			labels = function(x) paste0(round(x * 100), "%"),
			expand = c(0, 0)
		) +
		.pharmhand_theme(base_size = base_size) +
		ggplot2::theme(
			axis.text.x = ggplot2::element_text(hjust = 1),
			axis.text.y = ggplot2::element_text(hjust = 0.5),
			plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
			legend.position = "bottom",
			panel.grid.x = ggplot2::element_blank()
		) +
		ggplot2::labs(
			title = title,
			x = NULL,
			y = "Proportion of Studies",
			fill = "Risk of Bias"
		)

	# Return ClinicalPlot object
	ClinicalPlot(
		plot = p,
		data = proportions,
		type = "rob_summary",
		title = title,
		width = width,
		height = height,
		dpi = 300,
		metadata = list(
			tool = if (is_rob2) "RoB 2" else "ROBINS-I",
			domains = domains,
			n_studies = length(results),
			weighted = weighted
		)
	)
}


#' Save Risk of Bias Plot
#'
#' Saves a risk of bias plot to file. The file format is determined by the
#' filename extension (.png, .pdf, .svg, .tiff, .jpeg, .bmp, or .wmf).
#'
#' @param plot ClinicalPlot object or ggplot
#' @param filename Output filename (extension determines format)
#' @param width Width in inches. Default: 10
#' @param height Height in inches. Default: 6
#' @param dpi Resolution for raster formats (png, tiff, jpeg, bmp).
#'   Default: 300
#' @param ... Additional arguments passed to ggplot2::ggsave
#'
#' @return Invisible filename
#' @export
#'
#' @examples
#' \dontrun{
#' # Create and save traffic light plot
#' results <- list(
#'   assess_rob2(
#'     study_id = "STUDY001",
#'     d1_randomization = "Low",
#'     d2_deviations = "Low",
#'     d3_missing_data = "Low",
#'     d4_measurement = "Some concerns",
#'     d5_selection = "Low"
#'   )
#' )
#'
#' plot <- create_rob_traffic_light_plot(results)
#' save_rob_plot(plot, "rob_traffic_light.png", width = 8, height = 5)
#'
#' # Save as PDF (vector format)
#' save_rob_plot(plot, "rob_summary.pdf", width = 10, height = 6)
#' }
save_rob_plot <- function(
	plot,
	filename,
	width = 10,
	height = 6,
	dpi = 300,
	...
) {
	# Validate inputs
	if (missing(filename) || is.null(filename) || !nzchar(filename)) {
		ph_abort("filename is required and must be a non-empty string")
	}

	if (!S7::S7_inherits(plot, ClinicalPlot)) {
		if (inherits(plot, "ggplot")) {
			# Convert ggplot to ClinicalPlot
			plot <- ClinicalPlot(
				plot = plot,
				type = "rob_plot",
				title = "Risk of Bias Plot",
				width = width,
				height = height,
				dpi = dpi
			)
		} else {
			ph_abort("plot must be a ClinicalPlot object or ggplot")
		}
	}

	# Validate dimensions
	if (!is.numeric(width) || length(width) != 1 || width <= 0) {
		ph_abort("width must be a single positive numeric value")
	}

	if (!is.numeric(height) || length(height) != 1 || height <= 0) {
		ph_abort("height must be a single positive numeric value")
	}

	if (!is.numeric(dpi) || length(dpi) != 1 || dpi <= 0) {
		ph_abort("dpi must be a single positive numeric value")
	}

	# Get the ggplot object
	ggplot_obj <- plot@plot

	# Validate ggplot object is not NULL
	if (is.null(ggplot_obj)) {
		ph_abort("plot object is NULL - cannot save empty plot")
	}

	# Save with ggsave - direct call without tryCatch to ensure errors propagate
	ggplot2::ggsave(
		filename = filename,
		plot = ggplot_obj,
		width = width,
		height = height,
		dpi = dpi,
		units = "in",
		...
	)

	ph_inform(sprintf("Plot saved to: %s", filename))
	invisible(filename)
}


#' Export Risk of Bias Data to Tidy Format
#'
#' Converts a list of RoB2Result or ROBINSIResult objects to a tidy data frame
#' suitable for further analysis or export.
#'
#' @param results List of RoB2Result or ROBINSIResult objects
#' @param wide_format Logical. If TRUE, returns wide format with one row per
#'   study and columns for each domain judgment. If FALSE, returns long format
#'   with one row per domain per study. Default: FALSE
#'
#' @return A data frame with risk of bias assessment data
#' @export
#'
#' @examples
#' \dontrun{
#' results <- list(
#'   assess_rob2(
#'     study_id = "STUDY001",
#'     d1_randomization = "Low",
#'     d2_deviations = "Low",
#'     d3_missing_data = "Low",
#'     d4_measurement = "Some concerns",
#'     d5_selection = "Low",
#'     outcome = "OS"
#'   )
#' )
#'
#' # Long format (default)
#' long_df <- rob_data_to_tidy(results)
#' print(long_df)
#'
#' # Wide format
#' wide_df <- rob_data_to_tidy(results, wide_format = TRUE)
#' print(wide_df)
#' }
rob_data_to_tidy <- function(results, wide_format = FALSE) {
	# Validate inputs
	if (!is.list(results) || length(results) == 0) {
		ph_abort(
			"results must be a non-empty list of RoB2Result or ROBINSIResult objects"
		)
	}

	# Check all elements are valid result objects
	is_rob2 <- all(vapply(
		results,
		function(r) S7::S7_inherits(r, RoB2Result),
		logical(1)
	))
	is_robinsi <- all(vapply(
		results,
		function(r) S7::S7_inherits(r, ROBINSIResult),
		logical(1)
	))

	if (!is_rob2 && !is_robinsi) {
		ph_abort(
			"All elements in results must be RoB2Result or ROBINSIResult objects"
		)
	}

	# Also reject mixed inputs (both RoB2 and ROBINS-I)
	if (is_rob2 && is_robinsi) {
		ph_abort(
			"Mixed RoB2Result and ROBINSIResult inputs not supported. ",
			"Use only one tool type."
		)
	}

	if (isTRUE(wide_format)) {
		# Wide format: one row per study
		wide_list <- lapply(results, function(r) {
			row <- data.frame(
				study_id = r@study_id,
				overall = r@overall,
				stringsAsFactors = FALSE
			)

			# Add outcome if present
			if (S7::S7_inherits(r, ROBINSIResult)) {
				row$outcome <- r@outcome
				row$intervention <- r@intervention
				row$comparator <- r@comparator
			} else {
				row$outcome <- r@outcome
			}

			# Add domain judgments
			domains <- if (is_rob2) ROB2_DOMAINS else ROBINSI_DOMAINS
			for (d in domains) {
				short_name <- gsub("^D\\d+_", "", d)
				row[[paste0(short_name, "_judgment")]] <- r@domains[[d]]$judgment
			}

			row
		})

		do.call(rbind, wide_list)
	} else {
		# Long format: one row per domain per study
		long_list <- lapply(results, function(r) {
			domains <- if (is_rob2) ROB2_DOMAINS else ROBINSI_DOMAINS
			domain_labels <- if (is_rob2) {
				.ROB2_DOMAIN_LABELS
			} else {
				.ROBINSI_DOMAIN_LABELS
			}

			study_rows <- lapply(domains, function(d) {
				data.frame(
					study_id = r@study_id,
					outcome = r@outcome,
					domain = d,
					domain_label = unname(domain_labels[d]),
					judgment = r@domains[[d]]$judgment,
					support = r@domains[[d]]$support %||% "",
					overall = r@overall,
					stringsAsFactors = FALSE
				)
			})

			do.call(rbind, study_rows)
		})

		do.call(rbind, long_list)
	}
}
