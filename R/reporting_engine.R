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
	# Handle empty results gracefully
	if (x@is_empty) {
		# Return empty flextable with appropriate message
		ft <- flextable::flextable(data.frame(Message = "No data available"))
		return(
			ft |>
				flextable::theme_booktabs() |>
				flextable::autofit() |>
				flextable::fontsize(size = 9, part = "all")
		)
	}

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
			dplyr::select(df, -dplyr::all_of("label"))
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
	# Handle empty results gracefully
	if (x@is_empty) {
		return(gt::gt(data.frame(Message = "No data available")))
	}

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

#' Clinical table from analysis results
#'
#' Converts an `AnalysisResults` object into a `ClinicalTable` for reporting.
#'
#' @param res An `AnalysisResults` object with `@stats` (table data) and `@type`
#'   (table type) slots.
#' @param title Optional table title.
#'
#' @return A `ClinicalTable` object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert analysis results to clinical table
#' results <- analyze_demographics(adsl_data)
#' table <- clinical_table_from_results(results, title = "Demographics")
#' }
clinical_table_from_results <- function(res, title = "") {
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
#' @param na_string String to display for NA values
#'   (default: getOption("pharmhand.na_string", "--"))
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
	na_string = get_na_string(),
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
# Helper Functions
# =============================================================================

#' Ensure ADaMData
#'
#' Helper function that coerces raw data frames to ADaMData if they aren't
#' already ADaMData objects.
#'
#' @param data A data frame or ADaMData object
#' @param domain Character string for the ADaM domain (used when wrapping
#'   data frames)
#' @param trt_var Treatment variable name (passed to ADaMData constructor
#'   when wrapping data frames)
#' @param subject_var Subject ID variable name (passed to ADaMData constructor
#'   when wrapping data frames)
#'
#' @return An ADaMData object
#' @keywords internal
.ensure_adam_data <- function(
	data,
	domain = "ADSL",
	trt_var = NULL,
	subject_var = NULL
) {
	if (S7::S7_inherits(data, ADaMData)) {
		# Warn if trt_var provided and differs from object's property
		if (!is.null(trt_var) && trt_var != data@trt_var) {
			ph_warn(
				"'trt_var' argument provided (",
				trt_var,
				") differs from ADaMData object's 'trt_var' property (",
				data@trt_var,
				"). Using the object's property."
			)
		}
		# Warn if subject_var provided and differs from object's property
		if (!is.null(subject_var) && subject_var != data@subject_var) {
			ph_warn(
				"'subject_var' argument provided (",
				subject_var,
				") differs from ADaMData object's 'subject_var' property (",
				data@subject_var,
				"). Using the object's property."
			)
		}
		return(data)
	}

	if (is.data.frame(data)) {
		ph_inform("Automatically wrapping data.frame in ADaMData object")

		# Build constructor arguments
		constructor_args <- list(data = data, domain = domain)
		if (!is.null(trt_var)) {
			constructor_args$trt_var <- trt_var
		}
		if (!is.null(subject_var)) {
			constructor_args$subject_var <- subject_var
		}

		return(do.call(ADaMData, constructor_args))
	}

	ph_abort(
		"'data' must be an ADaMData object or data.frame. ",
		"Got: ",
		class(data)[1]
	)
}

# =============================================================================
# IQWiG and G-BA Compliant Themes
# =============================================================================

#' IQWiG Theme for Flextable
#'
#' Apply IQWiG-compliant styling to a flextable. Based on formatting standards
#' from IQWiG Methods v8.0, with numeric formatting driven by
#' `decimal_separator` and missing values shown via `na_string`.
#'
#' @param ft A flextable object
#' @param font_name Font family (default: "Arial")
#' @param font_size Font size in points (default: 9)
#' @param header_bold Logical, bold header text (default: TRUE)
#' @param decimal_separator Decimal separator: "." or "," (default: ",");
#'   applies to numeric columns with "." as the thousands separator when ",".
#' @param na_string String to display for missing values
#'   (default: getOption("pharmhand.na_string", "--"))
#' @param autofit Logical, autofit column widths (default: TRUE)
#'
#' @return A styled flextable object
#' @export
#'
#' @references
#' IQWiG (2025). Allgemeine Methoden, Version 8.0.
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
	na_string = getOption("pharmhand.na_string", "--"),
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

	# Handle NA values and decimal separator
	if (decimal_separator == ",") {
		ft <- ft |>
			flextable::colformat_char(na_str = na_string) |>
			flextable::colformat_double(
				na_str = na_string,
				decimal.mark = ",",
				big.mark = "."
			) |>
			flextable::colformat_int(na_str = na_string)
	} else {
		ft <- ft |>
			flextable::colformat_char(na_str = na_string) |>
			flextable::colformat_double(na_str = na_string) |>
			flextable::colformat_int(na_str = na_string)
	}

	# Autofit
	if (autofit) {
		ft <- ft |>
			flextable::autofit() |>
			flextable::fit_to_width(max_width = 7.5)
	}

	attr(ft, "pharmhand_theme") <- "iqwig"

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
#' @param na_string String to display for missing values
#'   (default: getOption("pharmhand.na_string", "--"))
#' @param autofit Logical, autofit column widths (default: TRUE)
#'
#' @return A styled flextable object
#' @export
#'
#' @references
#' G-BA (2024). Dossiervorlage Modul 4. See \url{https://www.g-ba.de}
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
	na_string = getOption("pharmhand.na_string", "--"),
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

	# Handle NA values and decimal separator
	if (decimal_separator == ",") {
		ft <- ft |>
			flextable::colformat_char(na_str = na_string) |>
			flextable::colformat_double(
				na_str = na_string,
				decimal.mark = ",",
				big.mark = "."
			) |>
			flextable::colformat_int(na_str = na_string)
	} else {
		ft <- ft |>
			flextable::colformat_char(na_str = na_string) |>
			flextable::colformat_double(na_str = na_string) |>
			flextable::colformat_int(na_str = na_string)
	}

	# Autofit
	if (autofit) {
		ft <- ft |>
			flextable::autofit() |>
			flextable::fit_to_width(max_width = 7.5)
	}

	attr(ft, "pharmhand_theme") <- "gba"

	ft
}

#' Create Clinical Table (Factory Function)
#'
#' Primary factory function for creating ClinicalTable objects with proper
#' styling. Supports multiple data types and themes, with optional custom
#' summarization logic.
#'
#' @param data Data to display. Can be:
#'   - `ADaMData` object (uses `@data` or `@filtered_data`)
#'   - `LayeredTable` object (built using `build_table()`)
#'   - `AnalysisResults` object (uses `@stats`)
#'   - Raw `data.frame`
#' @param type Character string for table type (e.g., "demographics", "ae_soc")
#' @param title Table title
#' @param footnotes Character vector of footnotes
#' @param theme Theme preset: "hta", "iqwig", "gba", or "clinical"
#'   (default: "hta")
#' @param summary_fn Optional function for custom summarization. Receives the
#'   extracted data and should return a data.frame. If NULL, data is used as-is.
#' @param ... Additional arguments passed to theme functions
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with data.frame
#' df <- data.frame(Treatment = c("A", "B"), N = c(100, 95))
#' table <- create_clinical_table(
#'   data = df,
#'   type = "summary",
#'   title = "Treatment Summary",
#'   footnotes = "ITT Population"
#' )
#'
#' # With custom summary function
#' table <- create_clinical_table(
#'   data = adsl_data,
#'   type = "demographics",
#'   summary_fn = function(d) {
#'     d |> dplyr::summarise(N = dplyr::n(), .by = TRT01P)
#'   },
#'   theme = "iqwig"
#' )
#'
#' # With ADaMData object
#' table <- create_clinical_table(
#'   data = ADaMData(adsl_df, domain = "ADSL"),
#'   type = "demographics",
#'   title = "Baseline Characteristics",
#'   theme = "gba"
#' )
#'
#' # With LayeredTable object
#' demo_lt <- LayeredTable(
#'   data = adsl_df,
#'   trt_var = "TRT01P",
#'   layers = list(
#'     CountLayer(target_var = "SEX", label = "Sex"),
#'     DescriptiveLayer(target_var = "AGE", label = "Age (years)")
#'   )
#' )
#' table <- create_clinical_table(
#'   data = demo_lt,
#'   type = "demographics",
#'   title = "Demographics"
#' )
#' }
create_clinical_table <- function(
	data,
	type,
	title = NULL,
	footnotes = character(),
	theme = c("hta", "iqwig", "gba", "clinical"),
	summary_fn = NULL,
	...
) {
	theme <- match.arg(theme)

	# Capture ... args for theme functions
	dots <- list(...)

	# Table metadata - merge theme metadata with any custom metadata passed via ...
	table_metadata <- list(theme = theme)
	if ("metadata" %in% names(dots)) {
		table_metadata <- c(table_metadata, dots$metadata)
		dots$metadata <- NULL # Remove so it's not passed to theme functions
	}

	# Extract data based on input type
	if (S7::S7_inherits(data, ADaMData)) {
		table_data <- data@data
	} else if (S7::S7_inherits(data, LayeredTable)) {
		table_data <- build_table(data)
	} else if (S7::S7_inherits(data, AnalysisResults)) {
		table_data <- data@stats
	} else if (is.data.frame(data)) {
		table_data <- data
	} else {
		ph_abort(
			"'data' must be an ADaMData, LayeredTable, AnalysisResults, ",
			"or data.frame object. Got: ",
			class(data)[1]
		)
	}

	# Apply custom summary function if provided
	if (!is.null(summary_fn)) {
		if (!is.function(summary_fn)) {
			ph_abort("'summary_fn' must be a function or NULL")
		}
		table_data <- summary_fn(table_data)
		if (!is.data.frame(table_data)) {
			ph_abort("'summary_fn' must return a data.frame")
		}
	}

	# Apply theme styling
	ft <- switch(
		theme,
		"hta" = do.call(
			create_hta_table,
			c(
				list(
					data = table_data,
					title = title,
					footnotes = footnotes
				),
				dots
			)
		),
		"iqwig" = {
			ft <- flextable::flextable(table_data)
			ft <- do.call(theme_iqwig, c(list(ft), dots))
			# Add title if provided
			if (!is.null(title) && nchar(title) > 0) {
				ft <- ft |>
					flextable::add_header_lines(title) |>
					flextable::bold(i = 1, part = "header")
			}
			# Add footnotes
			if (length(footnotes) > 0) {
				for (fn in footnotes) {
					ft <- ft |> flextable::add_footer_lines(fn)
				}
				ft <- ft |>
					flextable::fontsize(size = 8, part = "footer") |>
					flextable::italic(part = "footer")
			}
			ft
		},
		"gba" = {
			ft <- flextable::flextable(table_data)
			ft <- do.call(theme_gba, c(list(ft), dots))
			# Add title if provided
			if (!is.null(title) && nchar(title) > 0) {
				ft <- ft |>
					flextable::add_header_lines(title) |>
					flextable::bold(i = 1, part = "header")
			}
			# Add footnotes
			if (length(footnotes) > 0) {
				for (fn in footnotes) {
					ft <- ft |> flextable::add_footer_lines(fn)
				}
				ft <- ft |>
					flextable::fontsize(size = 8, part = "footer") |>
					flextable::italic(part = "footer")
			}
			ft
		},
		"clinical" = {
			ft <- flextable::flextable(table_data)
			ft <- do.call(apply_clinical_style, c(list(ft, style = "clinical"), dots))
			# Add title if provided
			if (!is.null(title) && nchar(title) > 0) {
				ft <- ft |>
					flextable::add_header_lines(title) |>
					flextable::bold(i = 1, part = "header")
			}
			# Add footnotes
			if (length(footnotes) > 0) {
				for (fn in footnotes) {
					ft <- ft |> flextable::add_footer_lines(fn)
				}
				ft <- ft |>
					flextable::fontsize(size = 8, part = "footer") |>
					flextable::italic(part = "footer")
			}
			ft
		}
	)

	# Wrap in ClinicalTable
	ClinicalTable(
		data = table_data,
		flextable = ft,
		type = type,
		title = title,
		metadata = table_metadata
	)
}

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
	autofit = ph_default("autofit")
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

#' Convert Clinical Content to G-BA Template
#'
#' Applies G-BA Module 4 formatting rules to clinical content.
#' For ClinicalTable objects, the G-BA theme is applied.
#' For ClinicalReport objects, all contained tables are formatted.
#'
#' @param x A ClinicalTable, ClinicalReport, or list of these
#' @param path Optional file path. If provided with a ClinicalReport,
#'   the report is written to disk.
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable or ClinicalReport with G-BA formatting applied
#' @export
#'
#' @examples
#' \dontrun{
#' table <- create_hta_module4_table()
#' table <- to_gba_template(table)
#' }
to_gba_template <- function(x, path = NULL, autofit = TRUE) {
	if (S7::S7_inherits(x, ClinicalTable)) {
		if (is.null(x@flextable)) {
			x@flextable <- flextable::flextable(x@data)
		}
		x@flextable <- theme_gba(x@flextable, autofit = autofit)
		return(x)
	}

	if (S7::S7_inherits(x, ClinicalReport)) {
		sections <- lapply(x@sections, function(section) {
			section@content <- lapply(section@content, function(item) {
				if (S7::S7_inherits(item, ClinicalTable)) {
					if (is.null(item@flextable)) {
						item@flextable <- flextable::flextable(item@data)
					}
					item@flextable <- theme_gba(item@flextable, autofit = autofit)
				}
				item
			})
			section
		})

		x@sections <- sections

		if (!is.null(path)) {
			write_docx(x, path)
		}

		return(x)
	}

	if (is.list(x)) {
		if (!is.null(path)) {
			ph_warn(
				"'path' argument ignored for list input; use write_docx separately"
			)
		}
		return(lapply(x, to_gba_template, autofit = autofit))
	}

	ph_abort("'x' must be a ClinicalTable, ClinicalReport, or list")
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

# =============================================================================
# Workflow Helper Functions
# =============================================================================

#' Quick Demographics Report
#'
#' High-level convenience function to generate a demographics report from
#' ADSL data in a single call. Creates demographics table and writes to Word.
#'
#' @param data Data frame or ADaMData object containing ADSL data
#' @param output Character string output file path (e.g., "demographics.docx")
#' @param title Character string report title
#' @param trt_var Character string treatment variable name
#' @param include_title Logical, include title page
#' @param ... Additional arguments passed to create_demographics_table()
#'
#' @return Invisibly returns the ClinicalReport object
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick demographics report in one line
#' quick_demographics_report(adsl_df, "demo.docx")
#'
#' # With custom settings
#' quick_demographics_report(
#'   adsl_df,
#'   "demo.docx",
#'   title = "Study XYZ Demographics",
#'   trt_var = "ARM"
#' )
#' }
quick_demographics_report <- function(
	data,
	output,
	title = "Demographics and Baseline Characteristics",
	trt_var = ph_default("trt_var"),
	include_title = TRUE,
	...
) {
	# Auto-coerce if needed
	if (is.data.frame(data) && !S7::S7_inherits(data, ADaMData)) {
		data <- ADaMData(data = data, domain = "ADSL")
	}

	# Create demographics table
	demo_table <- create_demographics_table(
		data = data,
		title = title,
		trt_var = trt_var,
		...
	)

	# Create report
	report <- ClinicalReport(
		study_id = "QUICK_REPORT",
		study_title = title
	)

	# Add demographics section
	section <- ReportSection(
		title = title,
		section_type = "demographics",
		content = list(demo = demo_table)
	)
	report <- add_section(report, section)

	# Write to Word
	generate_word(report, output, include_title = include_title)

	invisible(report)
}

#' Quick Safety Report
#'
#' High-level convenience function to generate a safety report from ADAE/ADSL
#' data in a single call. Creates multiple safety tables and writes to Word.
#'
#' @param data Data frame containing ADAE data
#' @param adsl Data frame containing ADSL data (optional, for denominators)
#' @param output Character string output file path (e.g., "safety.docx")
#' @param title Character string report title
#' @param trt_var Character string treatment variable name
#' @param include_overview Logical, include AE overview table
#' @param include_soc Logical, include SOC table
#' @param include_soc_pt Logical, include SOC/PT hierarchical table
#' @param include_sae Logical, include SAE table
#' @param include_title Logical, include title page
#' @param ... Additional arguments passed to table functions
#'
#' @return Invisibly returns the ClinicalReport object
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick safety report with default tables
#' quick_safety_report(adae_df, adsl_df, "safety.docx")
#'
#' # Custom configuration
#' quick_safety_report(
#'   adae_df, adsl_df,
#'   "safety.docx",
#'   title = "Study XYZ Safety Analysis",
#'   include_soc_pt = FALSE
#' )
#' }
quick_safety_report <- function(
	data,
	adsl = NULL,
	output,
	title = "Safety Analysis",
	trt_var = ph_default("trt_var"),
	include_overview = TRUE,
	include_soc = TRUE,
	include_soc_pt = FALSE,
	include_sae = TRUE,
	include_title = TRUE,
	...
) {
	# Create report
	report <- ClinicalReport(
		study_id = "QUICK_SAFETY",
		study_title = title
	)

	content_list <- list()

	# Add overview table
	if (include_overview) {
		content_list$overview <- create_ae_summary_table(
			data = data,
			adsl = adsl,
			type = "overview",
			trt_var = trt_var,
			...
		)
	}

	# Add SOC table
	if (include_soc) {
		content_list$soc <- create_ae_summary_table(
			data = data,
			adsl = adsl,
			type = "soc",
			trt_var = trt_var,
			...
		)
	}

	# Add SOC/PT table
	if (include_soc_pt) {
		content_list$soc_pt <- create_ae_summary_table(
			data = data,
			adsl = adsl,
			type = "soc_pt",
			trt_var = trt_var,
			...
		)
	}

	# Add SAE table
	if (include_sae) {
		content_list$sae <- create_ae_summary_table(
			data = data,
			adsl = adsl,
			type = "sae",
			trt_var = trt_var,
			...
		)
	}

	# Add safety section
	if (length(content_list) > 0) {
		section <- ReportSection(
			title = "Adverse Events",
			section_type = "safety",
			content = content_list
		)
		report <- add_section(report, section)
	}

	# Write to Word
	generate_word(report, output, include_title = include_title)

	invisible(report)
}
