#' S7 Generics and Methods for Clinical Reports
#'
#' Generic functions with multiple dispatch for handling clinical content.
#' These use S7's method dispatch system to provide polymorphic behavior
#' based on the class of arguments.
#'
#' @name S7_generics
NULL

#' Analyze ADaM datasets
#'
#' Generic function for performing clinical analysis on ADaM data.
#'
#' @param x An ADaMData object or data frame
#' @param ... Additional arguments passed to methods
#'
#' @return An AnalysisResults object
#'
#' @export
analyze <- S7::new_generic("analyze", "x")

#' Convert analysis results to flextable
#'
#' Generic function for converting analysis results or clinical tables
#' to flextable objects for consistent reporting.
#'
#' @param x An AnalysisResults or ClinicalTable object
#' @param ... Additional arguments passed to methods
#'
#' @return A flextable object
#'
#' @export
as_flextable <- S7::new_generic("as_flextable", "x")

#' Convert analysis results to gt
#'
#' Generic function for converting analysis results to gt objects.
#'
#' @param x An AnalysisResults object
#' @param ... Additional arguments passed to methods
#'
#' @return A gt object
#'
#' @export
as_gt <- S7::new_generic("as_gt", "x")

#' Write clinical content to a Word document
#'
#' Generic function for writing clinical content to a Word document
#' at a specified path.
#'
#' @param x A ClinicalReport, StudyResult, or ClinicalContent object
#' @param path Character string specifying the output file path
#' @param ... Additional arguments passed to methods
#'
#' @return The modified object (invisibly)
#'
#' @export
write_docx <- S7::new_generic("write_docx", c("x", "path"))

# Register S3 class from officer package for S7 dispatch
class_rdocx <- S7::new_S3_class("rdocx")

#' Convert clinical content to Word format
#'
#' Generic function that converts clinical content (tables, plots) to
#' a format suitable for Word documents. Dispatches on the class of `x`.
#'
#' @param x A ClinicalContent object (ClinicalTable or ClinicalPlot)
#' @param ... Additional arguments passed to methods
#'
#' @return A Word-compatible object (flextable or external_img)
#'
#' @export to_word
#'
#' @examples
#' \dontrun{
#' # For a table
#' word_obj <- to_word(clinical_table)
#'
#' # For a plot
#' word_obj <- to_word(clinical_plot)
#' }
to_word <- S7::new_generic("to_word", "x")

#' @describeIn to_word Method for ClinicalTable
#' @noRd
S7::method(to_word, ClinicalTable) <- function(x, ...) {
	x@flextable
}

#' @describeIn to_word Method for ClinicalPlot
#' @note The temporary file created for the plot image persists until the
#'   returned external_img object is embedded in a Word document and saved.
#'   For reports with many plots, temp files may accumulate during processing.
#'   Files are cleaned up when R's temp directory is cleared (session end or
#'   system cleanup).
#' @noRd
S7::method(to_word, ClinicalPlot) <- function(x, ...) {
	# Create temporary file for plot.
	# Note: Cannot delete immediately as officer::external_img stores a file path
	# reference that is read when the document is actually rendered/saved.
	tmp <- tempfile(fileext = ".png")

	# Extract plot object (handle ggsurvplot)
	plot_obj <- if (x@is_survival) x@plot$plot else x@plot

	ggplot2::ggsave(
		filename = tmp,
		plot = plot_obj,
		width = x@width,
		height = x@height,
		dpi = x@dpi
	)

	officer::external_img(src = tmp)
}

#' @describeIn to_word Method for list of content
#' @noRd
S7::method(to_word, S7::class_list) <- function(x, ...) {
	lapply(x, function(item) to_word(item, ...))
}

#' Add content to a Word document
#'
#' Generic function for adding clinical content to an officer Word document.
#' Uses multiple dispatch on both `doc` and `content`.
#'
#' @param doc An rdocx object from officer
#' @param content A ClinicalContent object
#' @param ... Additional arguments passed to methods
#'
#' @return The modified rdocx object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' doc <- officer::read_docx()
#' doc <- add_to_docx(doc, clinical_table)
#' doc <- add_to_docx(doc, clinical_plot)
#' }
add_to_docx <- S7::new_generic("add_to_docx", c("doc", "content"))

#' @describeIn add_to_docx Method for rdocx + ClinicalTable
#' @noRd
#' @describeIn add_to_docx Method for rdocx + ClinicalTable
#' @noRd
S7::method(add_to_docx, list(class_rdocx, ClinicalTable)) <- function(
	doc,
	content,
	...
) {
	flextable::body_add_flextable(doc, content@flextable)
}

#' @describeIn add_to_docx Method for rdocx + ClinicalPlot
#' @noRd
S7::method(add_to_docx, list(class_rdocx, ClinicalPlot)) <- function(
	doc,
	content,
	...
) {
	# Create temporary file for plot
	tmp <- tempfile(fileext = ".png")
	plot_obj <- if (content@is_survival) content@plot$plot else content@plot
	ggplot2::ggsave(
		filename = tmp,
		plot = plot_obj,
		width = content@width,
		height = content@height,
		dpi = content@dpi
	)
	doc <- officer::body_add_img(
		doc,
		src = tmp,
		width = content@width,
		height = content@height
	)
	unlink(tmp)
	doc
}

#' @describeIn add_to_docx Method for rdocx + list of content
#' @noRd
S7::method(add_to_docx, list(class_rdocx, S7::class_list)) <- function(
	doc,
	content,
	...
) {
	batch_size <- getOption("FunctionReport.docx_batch_size", 50)

	for (i in seq(1, length(content), by = batch_size)) {
		batch <- content[i:min(i + batch_size - 1, length(content))]
		for (item in batch) {
			doc <- add_to_docx(doc, item, ...)
		}
	}
	doc
}

#' @describeIn add_to_docx Method for rdocx + ReportSection
#' @noRd
S7::method(add_to_docx, list(class_rdocx, ReportSection)) <- function(
	doc,
	content,
	...
) {
	# Add section title if available
	if (!is.null(content@title)) {
		doc <- officer::body_add_par(doc, content@title, style = "heading 2")
	}

	# Add all content in the section
	for (item in content@content) {
		doc <- add_to_docx(doc, item, ...)
	}
	doc
}

#' Save ClinicalTable as PNG
#'
#' Saves a ClinicalTable's flextable to a PNG file.
#'
#' @param x A ClinicalTable object
#' @param path Optional file path. If NULL, creates a temp file.
#'
#' @return The file path where the PNG was saved
#' @keywords internal
save_as_png <- function(x, path = NULL) {
	if (is.null(path)) {
		path <- tempfile(fileext = ".png")
	}
	flextable::save_as_image(x@flextable, path = path)
	path
}

#' Save ClinicalTable as PDF
#'
#' Saves a ClinicalTable's flextable to a PDF file. Uses webshot2 for
#' high-quality HTML-to-PDF conversion if available, otherwise falls back
#' to image-based export via flextable::save_as_image().
#'
#' @param x A ClinicalTable object
#' @param path Optional file path. If NULL, creates a temp file.
#'
#' @return The file path where the PDF was saved
#'
#' @note The image-based fallback may result in lower quality output compared
#'   to native PDF rendering. For best results, install the webshot2 package.
#'
#' @keywords internal
save_as_pdf <- function(x, path = NULL) {
	if (is.null(path)) {
		path <- tempfile(fileext = ".pdf")
	}

	# Try webshot2 for higher quality PDF export
	if (requireNamespace("webshot2", quietly = TRUE)) {
		# Create temporary HTML file
		tmp_html <- tempfile(fileext = ".html")
		on.exit(unlink(tmp_html), add = TRUE)

		flextable::save_as_html(x@flextable, path = tmp_html)
		webshot2::webshot(tmp_html, file = path, selector = "body")
	} else {
		# Fallback to image-based export
		cli::cli_inform(
			c(
				"i" = "Using image-based PDF export (install {.pkg webshot2} for better quality)"
			)
		)
		flextable::save_as_image(x@flextable, path = path)
	}

	path
}

#' Save ClinicalPlot to file
#'
#' Saves a ClinicalPlot to a file in the specified format.
#'
#' @param x A ClinicalPlot object
#' @param format Character string for output format ("png", "pdf", "svg")
#' @param path Optional file path. If NULL, creates a temp file.
#'
#' @return The file path where the plot was saved
#' @keywords internal
save_plot_as <- function(x, format = "png", path = NULL) {
	if (is.null(path)) {
		path <- tempfile(fileext = paste0(".", format))
	}

	# Extract plot object (handle ggsurvplot)
	plot_obj <- if (x@is_survival) x@plot$plot else x@plot

	ggplot2::ggsave(
		filename = path,
		plot = plot_obj,
		width = x@width,
		height = x@height,
		dpi = x@dpi,
		device = format
	)

	path
}

#' Format clinical content to different output formats
#'
#' Generic function for formatting clinical content to various output
#' formats. Uses multiple dispatch on both `x` and `format`.
#'
#' @param x A ClinicalContent object
#' @param format Character string specifying output format
#' @param ... Additional arguments passed to methods
#'
#' @return Formatted content in the specified format
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert table to different formats
#' png_data <- format_content(clinical_table, "png")
#' pdf_data <- format_content(clinical_table, "pdf")
#' }
format_content <- S7::new_generic("format_content", c("x", "format"))

#' @describeIn format_content Method for ClinicalTable + character format
#' @noRd
S7::method(
	format_content,
	list(ClinicalTable, S7::class_character)
) <- function(x, format, ...) {
	switch(
		format,
		"docx" = to_word(x),
		"png" = save_as_png(x),
		"pdf" = save_as_pdf(x),
		cli::cli_abort("Unsupported format: {format}")
	)
}

#' @describeIn format_content Method for ClinicalPlot + character format
#' @noRd
S7::method(format_content, list(ClinicalPlot, S7::class_character)) <- function(
	x,
	format,
	...
) {
	switch(
		format,
		"png" = save_plot_as(x, "png"),
		"pdf" = save_plot_as(x, "pdf"),
		"svg" = save_plot_as(x, "svg"),
		cli::cli_abort("Unsupported format: {format}")
	)
}

#' Generate a summary of clinical content
#'
#' Generic function for generating summary information about
#' clinical content objects.
#'
#' @param x A clinical content object
#' @param ... Additional arguments passed to methods
#'
#' @return A list with summary information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summary_info <- summarize_content(clinical_table)
#' }
summarize_content <- S7::new_generic("summarize_content", "x")

#' @describeIn summarize_content Method for ClinicalTable
#' @noRd
S7::method(summarize_content, ClinicalTable) <- function(x, ...) {
	list(
		type = x@type,
		title = x@title,
		n_rows = x@n_rows,
		n_cols = x@n_cols,
		columns = x@column_names,
		metadata = x@metadata
	)
}

#' @describeIn summarize_content Method for ClinicalPlot
#' @noRd
S7::method(summarize_content, ClinicalPlot) <- function(x, ...) {
	list(
		type = x@type,
		title = x@title,
		width = x@width,
		height = x@height,
		dpi = x@dpi,
		plot_class = class(x@plot),
		is_survival = x@is_survival,
		metadata = x@metadata
	)
}

#' @describeIn summarize_content Method for StudyResult
#' @noRd
S7::method(summarize_content, StudyResult) <- function(x, ...) {
	list(
		study_id = x@study_id,
		study_title = x@study_title,
		n_tables = x@n_tables,
		n_plots = x@n_plots,
		table_names = x@table_names,
		plot_names = x@plot_names,
		metadata = x@metadata
	)
}

#' @describeIn summarize_content Method for ReportSection
#' @noRd
S7::method(summarize_content, ReportSection) <- function(x, ...) {
	list(
		title = x@title,
		section_type = x@section_type,
		n_content = x@n_content,
		content_types = vapply(x@content, function(item) item@type, character(1)),
		metadata = x@metadata
	)
}

#' Add a table to a StudyResult
#'
#' @param obj A StudyResult object
#' @param table A ClinicalTable object
#' @param name Character string for table name/identifier
#'
#' @return The modified StudyResult object
#'
#' @export
#' @rdname add_content
add_table <- S7::new_generic(
	"add_table",
	"obj",
	function(obj, table, name = NULL) {
		S7::S7_dispatch()
	}
)

#' @describeIn add_table Method for StudyResult
#' @noRd
S7::method(add_table, StudyResult) <- function(obj, table, name = NULL) {
	checkmate::assert_class(table, "ClinicalTable")
	checkmate::assert_string(name, null.ok = TRUE)

	if (is.null(name)) {
		name <- paste0("table_", obj@n_tables + 1)
	}

	obj@tables[[name]] <- table
	obj
}

#' Add a plot to a StudyResult
#'
#' @param obj A StudyResult object
#' @param plot A ClinicalPlot object
#' @param name Character string for plot name/identifier
#'
#' @return The modified StudyResult object
#'
#' @export
#' @rdname add_content
add_plot <- S7::new_generic(
	"add_plot",
	"obj",
	function(obj, plot, name = NULL) {
		S7::S7_dispatch()
	}
)

#' @describeIn add_plot Method for StudyResult
#' @noRd
S7::method(add_plot, StudyResult) <- function(obj, plot, name = NULL) {
	checkmate::assert_class(plot, "ClinicalPlot")
	checkmate::assert_string(name, null.ok = TRUE)

	if (is.null(name)) {
		name <- paste0("plot_", obj@n_plots + 1)
	}

	obj@plots[[name]] <- plot
	obj
}

#' Add a section to a ClinicalReport
#'
#' @param obj A ClinicalReport object
#' @param section A ReportSection object
#' @param name Character string for section name/identifier
#'
#' @return The modified ClinicalReport object
#'
#' @export
#' @rdname add_content
add_section <- S7::new_generic(
	"add_section",
	"obj",
	function(obj, section, name = NULL) {
		S7::S7_dispatch()
	}
)

#' @describeIn add_section Method for ClinicalReport
#' @noRd
S7::method(add_section, ClinicalReport) <- function(obj, section, name = NULL) {
	checkmate::assert_class(section, "ReportSection")
	checkmate::assert_string(name, null.ok = TRUE)

	if (is.null(name)) {
		name <- paste0("section_", obj@n_sections + 1)
	}

	obj@sections[[name]] <- section
	obj
}

#' Add content to a ReportSection
#'
#' @param obj A ReportSection object
#' @param content A ClinicalContent object
#' @param name Character string for content name/identifier
#'
#' @return The modified ReportSection object
#'
#' @export
#' @rdname add_content
add_content <- S7::new_generic(
	"add_content",
	"obj",
	function(obj, content, name = NULL) {
		S7::S7_dispatch()
	}
)

#' @describeIn add_content Method for ReportSection
#' @noRd
S7::method(add_content, ReportSection) <- function(obj, content, name = NULL) {
	checkmate::assert_class(content, "ClinicalContent")
	checkmate::assert_string(name, null.ok = TRUE)

	if (is.null(name)) {
		name <- paste0("content_", obj@n_content + 1)
	}

	obj@content[[name]] <- content
	obj
}

#' Generate a Word document from a ClinicalReport
#'
#' @param obj A ClinicalReport object
#' @param path Character string for output file path
#' @param include_title Logical, whether to include study title
#' @param include_toc Logical, whether to include table of contents
#'
#' @return The ClinicalReport object (invisibly)
#'
#' @export
generate_word <- S7::new_generic(
	"generate_word",
	"obj",
	function(obj, path, include_title = TRUE, include_toc = TRUE) {
		S7::S7_dispatch()
	}
)

#' @describeIn generate_word Method for ClinicalReport
#' @noRd
S7::method(generate_word, ClinicalReport) <- function(
	obj,
	path,
	include_title = TRUE,
	include_toc = TRUE
) {
	checkmate::assert_string(path)
	checkmate::assert_flag(include_title)
	checkmate::assert_flag(include_toc)

	# Create new Word document
	doc <- officer::read_docx()

	# Add title if requested
	if (include_title && !is.null(obj@study_title)) {
		doc <- officer::body_add_par(doc, obj@study_title, style = "heading 1")
	}

	# Add table of contents if requested
	if (include_toc) {
		doc <- officer::body_add_toc(doc)
	}

	# Add each section
	for (section in obj@sections) {
		doc <- add_to_docx(doc, section)
	}

	# Save document
	print(doc, target = path)

	invisible(obj)
}
