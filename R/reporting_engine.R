#' Reporting Engine
#'
#' Convert analysis results to flextable and assemble Word documents.
#'
#' @name reporting_engine
NULL

#' Convert AnalysisResults to flextable (S7 Method)
#'
#' @param x AnalysisResults object
#' @param ... Additional arguments
#'
#' @export
#' @name as_flextable_AnalysisResults
as_flextable_AnalysisResults <- S7::method(
	as_flextable,
	AnalysisResults
) <- function(x, ...) {
	df <- x@stats

	if (x@type == "baseline") {
		ft <- flextable::flextable(df) |>
			flextable::theme_booktabs() |>
			flextable::merge_v(j = "variable") |>
			flextable::set_header_labels(
				variable = "Variable",
				TRT01P = "Treatment",
				n = "n",
				mean = "Mean",
				sd = "SD",
				median = "Median",
				min = "Min",
				max = "Max"
			)
	} else if (x@type == "safety_ae") {
		# Hierarchy styling for SOC-PT
		# Defensive check for "label" column
		df_display <- if ("label" %in% names(df)) {
			dplyr::select(df, -"label")
		} else {
			df
		}
		ft <- flextable::flextable(df_display) |>
			flextable::theme_booktabs() |>
			flextable::padding(
				i = ~ level == "PT",
				j = 1,
				padding.left = 20
			) |>
			flextable::bold(i = ~ level == "SOC", j = 1)
	} else {
		ft <- flextable::flextable(df) |> flextable::theme_booktabs()
	}

	# Apply general clinical styling
	ft <- ft |>
		flextable::autofit() |>
		flextable::fontsize(size = 9, part = "all")

	return(ft)
}

#' Convert AnalysisResults to gt (S7 Method)
#'
#' @param x AnalysisResults object
#' @param ... Additional arguments
#'
#' @export
#' @name as_gt_AnalysisResults
as_gt_AnalysisResults <- S7::method(as_gt, AnalysisResults) <- function(
	x,
	...
) {
	df <- x@stats

	gt_tbl <- gt::gt(df)

	if (x@type == "baseline") {
		gt_tbl <- gt_tbl |>
			gt::tab_header(
				title = x@metadata$title %||% "Baseline Characteristics"
			) |>
			gt::tab_options(table.font.size = "small")
	} else if (x@type == "safety_ae") {
		gt_tbl <- gt_tbl |>
			gt::tab_header(title = "Adverse Events Analysis") |>
			gt::tab_style(
				style = gt::cell_text(weight = "bold"),
				locations = gt::cells_body(rows = level == "SOC")
			)
	}

	return(gt_tbl)
}

#' Write ClinicalReport to Word (S7 Method)
#'
#' @param x ClinicalReport object
#' @param path File path
#' @param add_toc Logical, whether to add table of contents (default: FALSE)
#' @param ... Additional arguments
#'
#' @export
#' @name write_docx_ClinicalReport
write_docx_ClinicalReport <- S7::method(
	write_docx,
	list(ClinicalReport, S7::class_character)
) <- function(
	x,
	path,
	add_toc = FALSE,
	...
) {
	doc <- officer::read_docx()

	# Add header
	doc <- officer::body_add_par(doc, x@study_title, style = "heading 1")
	doc <- officer::body_add_par(
		doc,
		paste("Study ID:", x@study_id),
		style = "Normal"
	)

	# Add Table of Contents if requested
	if (add_toc) {
		doc <- officer::body_add_toc(doc)
		doc <- officer::body_add_break(doc)
	} else {
		doc <- officer::body_add_break(doc)
	}

	# Batch process sections
	for (section in x@sections) {
		# Add section title
		doc <- officer::body_add_par(doc, section@title, style = "heading 2")

		# Add content
		for (item in section@content) {
			if (S7::S7_inherits(item, ClinicalTable)) {
				doc <- flextable::body_add_flextable(doc, item@flextable)
			} else if (S7::S7_inherits(item, ClinicalPlot)) {
				# Extract plot to temp file
				tmp <- tempfile(fileext = ".png")
				ggplot2::ggsave(
					tmp,
					item@plot,
					width = item@width,
					height = item@height,
					dpi = item@dpi
				)
				doc <- officer::body_add_img(
					doc,
					src = tmp,
					width = item@width,
					height = item@height
				)
				unlink(tmp) # Clean up immediately after embedding
			}
			doc <- officer::body_add_par(doc, "", style = "Normal") # Spacer
		}
	}

	print(doc, target = path)
	invisible(x)
}

#' Helper to create a ClinicalTable from AnalysisResults
#'
#' @param res AnalysisResults object
#' @param title Character string for table title
#' @export
create_clinical_table <- function(res, title = "") {
	ft <- as_flextable(res)
	ClinicalTable(
		data = res@stats,
		flextable = ft,
		type = res@type,
		title = title
	)
}

#' Apply Clinical Table Styling
#'
#' Apply standardized clinical table styling to a flextable,
#' inspired by carver's tbl_display pattern.
#'
#' @param ft A flextable object
#' @param style Style preset: "default", "clinical", "hta", or "compact"
#' @param font_name Font family (default: "Arial")
#' @param font_size_body Body font size in points (default: 9)
#' @param font_size_header Header font size in points (default: 10)
#' @param border_color Border color (default: "gray70")
#' @param header_bg Header background color (default: "gray95")
#' @param body_bg Body background color (default: "white")
#' @param font_color Font color for all text (default: "black")
#' @param zebra Logical, apply zebra striping (default: FALSE)
#' @param na_string String to display for NA values (default: "--")
#' @param autofit Logical, perform expensive layout calculations (default: TRUE)
#'
#' @return A styled flextable object
#' @export
#'
#' @examples
#' \dontrun{
#' ft <- flextable::flextable(mtcars[1:5, 1:4])
#' ft <- apply_clinical_style(ft, style = "clinical")
#' }
apply_clinical_style <- function(
	ft,
	style = c("default", "clinical", "hta", "compact"),
	font_name = "Arial",
	font_size_body = 9,
	font_size_header = 10,
	border_color = "gray70",
	header_bg = "gray95",
	body_bg = "white",
	font_color = "black",
	zebra = FALSE,
	na_string = "--",
	autofit = TRUE
) {
	style <- match.arg(style)

	# Base styling
	ft <- ft |>
		flextable::font(fontname = font_name, part = "all") |>
		flextable::fontsize(size = font_size_body, part = "body") |>
		flextable::fontsize(size = font_size_header, part = "header") |>
		flextable::align(align = "left", part = "body") |>
		flextable::align(align = "center", part = "header") |>
		flextable::color(color = font_color, part = "all")

	# Border styling
	border_style <- officer::fp_border(color = border_color, width = 1)

	ft <- ft |>
		flextable::border_remove() |>
		flextable::hline_top(border = border_style, part = "header") |>
		flextable::hline_bottom(border = border_style, part = "header") |>
		flextable::hline_bottom(border = border_style, part = "body")

	# Style-specific adjustments
	if (style == "clinical") {
		ft <- ft |>
			flextable::bg(bg = header_bg, part = "header") |>
			flextable::bold(part = "header")
	} else if (style == "hta") {
		# HTA/AMNOG style: more formal, denser
		ft <- ft |>
			flextable::bg(bg = header_bg, part = "header") |>
			flextable::bold(part = "header") |>
			flextable::padding(padding = 2, part = "all")
	} else if (style == "compact") {
		ft <- ft |>
			flextable::padding(padding = 1, part = "all") |>
			flextable::fontsize(size = 8, part = "all")
	}

	# Set explicit body background for dark mode compatibility
	ft <- ft |>
		flextable::bg(bg = body_bg, part = "body")

	# Zebra striping
	if (zebra) {
		ft <- ft |>
			flextable::bg(
				bg = "gray98",
				i = seq(2, flextable::nrow_part(ft, "body"), by = 2),
				part = "body"
			)
	}

	# Handle NA values
	ft <- ft |>
		flextable::colformat_char(na_str = na_string) |>
		flextable::colformat_double(na_str = na_string) |>
		flextable::colformat_int(na_str = na_string)

	# Autofit and ensure it fits to page width (A4 portrait with margins)
	# Check global option for override
	global_autofit <- getOption("pharmhand.autofit", default = NULL)
	use_autofit <- if (!is.null(global_autofit)) global_autofit else autofit

	if (use_autofit) {
		ft <- ft |>
			flextable::autofit() |>
			flextable::fit_to_width(max_width = 7.5)
	}

	ft
}

# =============================================================================
# IQWiG and G-BA Compliant Themes
# =============================================================================

#' IQWiG Theme for Flextable
#'
#' Apply IQWiG-compliant styling to a flextable. Based on formatting standards
#' from IQWiG Methods v8.0.
#'
#' @param ft A flextable object
#' @param font_name Font family (default: "Arial")
#' @param font_size Font size in points (default: 9)
#' @param header_bold Logical, bold header text (default: TRUE)
#' @param decimal_separator Decimal separator: "." or "," (default: ",")
#' @param autofit Logical, autofit column widths (default: TRUE)
#'
#' @return A styled flextable object
#' @export
#'
#' @references
#' IQWiG (2023). Allgemeine Methoden, Version 8.0.
#'
#' @examples
#' \dontrun{
#' ft <- flextable::flextable(mtcars[1:5, 1:4])
#' ft <- theme_iqwig(ft)
#' }
theme_iqwig <- function(
	ft,
	font_name = "Arial",
	font_size = 9,
	header_bold = TRUE,
	decimal_separator = ",",
	autofit = TRUE
) {
	# Validate input
	if (!inherits(ft, "flextable")) {
		ph_abort("'ft' must be a flextable object")
	}

	# IQWiG standard colors
	border_color <- "black"
	header_bg <- "white"
	body_bg <- "white"

	# Base styling - IQWiG uses clean, professional formatting
	ft <- ft |>
		flextable::font(fontname = font_name, part = "all") |>
		flextable::fontsize(size = font_size, part = "all") |>
		flextable::color(color = "black", part = "all") |>
		flextable::bg(bg = body_bg, part = "body") |>
		flextable::bg(bg = header_bg, part = "header") |>
		flextable::align(align = "left", part = "body") |>
		flextable::align(align = "center", part = "header") |>
		flextable::valign(valign = "center", part = "all")

	# Bold header if requested
	if (header_bold) {
		ft <- ft |> flextable::bold(part = "header")
	}

	# IQWiG border style: horizontal lines at top/bottom of header and bottom
	# of table
	border_style <- officer::fp_border(color = border_color, width = 1)

	ft <- ft |>
		flextable::border_remove() |>
		flextable::hline_top(border = border_style, part = "header") |>
		flextable::hline_bottom(border = border_style, part = "header") |>
		flextable::hline_bottom(border = border_style, part = "body")

	# Padding for readability
	ft <- ft |> flextable::padding(padding = 3, part = "all")

	# Handle NA values
	ft <- ft |>
		flextable::colformat_char(na_str = "--") |>
		flextable::colformat_double(na_str = "--") |>
		flextable::colformat_int(na_str = "--")

	# Autofit
	if (autofit) {
		ft <- ft |>
			flextable::autofit() |>
			flextable::fit_to_width(max_width = 7.5)
	}

	ft
}

#' G-BA Module 4 Theme for Flextable
#'
#' Apply G-BA Module 4 compliant styling to a flextable. Based on formatting
#' requirements from G-BA Dossier templates for AMNOG submissions.
#'
#' @param ft A flextable object
#' @param font_name Font family (default: "Arial")
#' @param font_size Font size in points (default: 10)
#' @param header_bold Logical, bold header text (default: TRUE)
#' @param header_bg Header background color (default: "#E8E8E8" light gray)
#' @param decimal_separator Decimal separator: "." or "," (default: ",")
#' @param autofit Logical, autofit column widths (default: TRUE)
#'
#' @return A styled flextable object
#' @export
#'
#' @references
#' G-BA (2023). Dossiervorlage Modul 4.
#'
#' @examples
#' \dontrun{
#' ft <- flextable::flextable(mtcars[1:5, 1:4])
#' ft <- theme_gba(ft)
#' }
theme_gba <- function(
	ft,
	font_name = "Arial",
	font_size = 10,
	header_bold = TRUE,
	header_bg = "#E8E8E8",
	decimal_separator = ",",
	autofit = TRUE
) {
	# Validate input
	if (!inherits(ft, "flextable")) {
		ph_abort("'ft' must be a flextable object")
	}

	# G-BA standard colors
	border_color <- "black"
	body_bg <- "white"

	# Base styling - G-BA uses slightly larger font and gray header
	ft <- ft |>
		flextable::font(fontname = font_name, part = "all") |>
		flextable::fontsize(size = font_size, part = "all") |>
		flextable::color(color = "black", part = "all") |>
		flextable::bg(bg = body_bg, part = "body") |>
		flextable::bg(bg = header_bg, part = "header") |>
		flextable::align(align = "left", part = "body") |>
		flextable::align(align = "center", part = "header") |>
		flextable::valign(valign = "center", part = "all")

	# Bold header if requested
	if (header_bold) {
		ft <- ft |> flextable::bold(part = "header")
	}

	# G-BA border style: full grid with black borders
	border_style <- officer::fp_border(color = border_color, width = 1)
	border_thin <- officer::fp_border(color = border_color, width = 0.5)

	ft <- ft |>
		flextable::border_remove() |>
		# Outer borders (thicker)
		flextable::hline_top(border = border_style, part = "header") |>
		flextable::hline_bottom(border = border_style, part = "body") |>
		flextable::vline(border = border_thin, part = "all") |>
		# Header bottom line
		flextable::hline_bottom(border = border_style, part = "header") |>
		# Inner horizontal lines (thinner)
		flextable::hline(border = border_thin, part = "body")

	# Padding for readability
	ft <- ft |> flextable::padding(padding = 4, part = "all")

	# Handle NA values
	ft <- ft |>
		flextable::colformat_char(na_str = "--") |>
		flextable::colformat_double(na_str = "--") |>
		flextable::colformat_int(na_str = "--")

	# Autofit
	if (autofit) {
		ft <- ft |>
			flextable::autofit() |>
			flextable::fit_to_width(max_width = 7.5)
	}

	ft
}

#' Create HTA-Style Table
#'
#' Create a flextable formatted for HTA/AMNOG submissions.
#'
#' @param data Data frame to display
#' @param title Table title
#' @param footnotes Character vector of footnotes
#' @param col_widths Named numeric vector of column widths (optional)
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A styled flextable object
#' @export
create_hta_table <- function(
	data,
	title = NULL,
	footnotes = character(),
	col_widths = NULL,
	autofit = TRUE
) {
	ft <- flextable::flextable(data)

	# Apply HTA styling
	# If col_widths are provided for all columns, we can suggest skipping autofit
	# However, apply_clinical_style handles the actual autofit call.
	# We pass autofit through.
	ft <- apply_clinical_style(ft, style = "hta", autofit = autofit)

	# Add title if provided
	if (!is.null(title) && nchar(title) > 0) {
		ft <- ft |>
			flextable::add_header_lines(title) |>
			flextable::bold(i = 1, part = "header")
	}

	# Add footnotes in a more efficient way if possible
	if (length(footnotes) > 0) {
		for (fn in footnotes) {
			ft <- ft |> flextable::add_footer_lines(fn)
		}
		ft <- ft |>
			flextable::fontsize(size = 8, part = "footer") |>
			flextable::italic(part = "footer")
	}

	# Apply custom column widths
	if (!is.null(col_widths)) {
		for (col_name in names(col_widths)) {
			if (col_name %in% names(data)) {
				ft <- ft |>
					flextable::width(
						j = col_name,
						width = col_widths[[col_name]]
					)
			}
		}
	}

	ft
}

#' Convert LayeredTable to flextable
#'
#' Build and style a LayeredTable as a flextable.
#'
#' @param x LayeredTable object
#' @param style Style preset (default: "clinical")
#' @param ... Additional arguments passed to apply_clinical_style
#'
#' @return A styled flextable object
#' @export
layered_to_flextable <- function(x, style = "clinical", ...) {
	if (!S7::S7_inherits(x, LayeredTable)) {
		ph_abort("'x' must be a LayeredTable object")
	}

	# Build the table data
	data <- build_table(x)

	# Create flextable
	ft <- flextable::flextable(data)

	# Apply styling
	ft <- apply_clinical_style(ft, style = style, ...)

	# Add title if present
	if (!is.null(x@title) && nchar(x@title) > 0) {
		ft <- ft |>
			flextable::add_header_lines(x@title) |>
			flextable::bold(i = 1, part = "header")
	}

	ft
}
