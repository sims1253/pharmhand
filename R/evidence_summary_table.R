#' Evidence Summary Tables
#'
#' Functions for creating evidence summary tables for IQWiG/G-BA dossiers
#' and health technology assessment submissions.
#'
#' @name evidence_summary_tables
#' @importFrom dplyr bind_rows
NULL

#' Create Evidence Summary Table
#'
#' Creates a formatted evidence summary table displaying study results,
#' effect estimates, heterogeneity statistics, risk of bias assessments,
#' and evidence grades for multiple endpoints.
#'
#' @param data List of named elements containing endpoint data.
#'   Each element should be a list with components:
#'   - `result`: MetaResult or ComparisonResult object with effect estimate,
#'     confidence interval, p-value, and study count
#'   - `grade`: EvidenceGrade object with evidence assessment
#'   - `rob`: RoB2Result object or list of RoB2Result objects for risk of bias
#'     (optional, can be derived from grade)
#'   - `label`: Character string for endpoint display name (optional)
#' @param title Table title (default: "Evidence Summary")
#' @param columns Character vector of column names to include. Default columns:
#'   "Endpoint", "N Studies", "Effect (95% CI)", "p-value", "I2", "RoB",
#'   "Grade".
#' @param conf_level Numeric confidence level for CIs (default: 0.95)
#' @param language Output language: "en" for English, "de" for German.
#'   Default: "en".
#' @param footnotes Character vector of footnotes to add to the table.
#' @param ... Additional arguments passed to [create_clinical_table()],
#'   such as `col_widths`, `autofit`, `theme`, etc.
#'
#' @return A ClinicalTable object containing the formatted evidence summary
#'   table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with meta-analysis results
#' endpoints <- list(
#'   "Overall Survival" = list(
#'     result = MetaResult(
#'       estimate = 0.75,
#'       ci = c(0.60, 0.94),
#'       p_value = 0.012,
#'       n = 3L,
#'       effect_measure = "hr",
#'       heterogeneity = list(I2 = 25.0, tau2 = 0.01)
#'     ),
#'     grade = EvidenceGrade(
#'       grade = "indication",
#'       grade_de = "Hinweis",
#'       direction = "benefit",
#'       n_studies = 3L
#'     ),
#'     rob = list(
#'       RoB2Result(study_id = "Study1", ...),
#'       RoB2Result(study_id = "Study2", ...)
#'     )
#'   ),
#'   "Progression-Free Survival" = list(
#'     result = MetaResult(
#'       estimate = 0.68,
#'       ci = c(0.52, 0.89),
#'       p_value = 0.005,
#'       n = 3L,
#'       effect_measure = "hr",
#'       heterogeneity = list(I2 = 45.0, tau2 = 0.03)
#'     ),
#'     grade = EvidenceGrade(
#'       grade = "proof",
#'       grade_de = "Beleg",
#'       direction = "benefit",
#'       n_studies = 3L
#'     )
#'   )
#' )
#'
#' table <- create_evidence_summary_table(endpoints)
#' }
create_evidence_summary_table <- function(
	data,
	title = "Evidence Summary",
	columns = c(
		"Endpoint",
		"N Studies",
		"Effect (95% CI)",
		"p-value",
		"I2",
		"RoB",
		"Grade"
	),
	conf_level = 0.95,
	language = c("en", "de"),
	footnotes = character(),
	...
) {
	language <- match.arg(language)

	# Validate data input
	if (!is.list(data) || length(data) == 0) {
		ph_abort("'data' must be a non-empty list")
	}

	# Build table data row by row
	table_data <- lapply(names(data), function(name) {
		.build_evidence_summary_row(
			endpoint_data = data[[name]],
			name = name,
			conf_level = conf_level,
			language = language
		)
	})

	# Convert to data frame using dplyr::bind_rows for proper column preservation
	result_df <- dplyr::bind_rows(table_data)
	rownames(result_df) <- NULL

	# Handle column subsetting
	if (!all(columns %in% names(result_df))) {
		missing_cols <- setdiff(columns, names(result_df))
		if (length(missing_cols) > 0) {
			ph_warn(paste(
				"Requested columns not available:",
				paste(missing_cols, collapse = ", ")
			))
		}
		columns <- intersect(columns, names(result_df))
	}
	result_df <- result_df[, columns, drop = FALSE]

	# Create footnotes
	default_footnotes <- c(
		sprintf("CI = %.0f%% Confidence Interval", 100 * conf_level),
		"RoB = Risk of Bias (RoB 2 assessment)",
		"Grade = IQWiG evidence grade"
	)
	if (language == "de") {
		default_footnotes <- c(
			sprintf("KI = %.0f%% Konfidenzintervall", 100 * conf_level),
			"RoB = Risiko einer Verzerrung (RoB 2 Bewertung)",
			"Grad = IQWiG Evidenzgrad"
		)
	}
	all_footnotes <- c(footnotes, default_footnotes)

	# Use create_clinical_table factory
	create_clinical_table(
		data = result_df,
		type = "evidence_summary",
		title = title,
		footnotes = all_footnotes,
		theme = "hta",
		metadata = list(
			columns = columns,
			language = language,
			conf_level = conf_level,
			n_endpoints = length(data)
		),
		...
	)
}


#' Create Study Characteristics Table
#'
#' Creates a summary table of study characteristics for G-BA Module 4
#' requirements, displaying design, sample size, treatment, and comparator
#' information for each included study.
#'
#' @param data List of Study objects, TwoArmStudy objects, or data frame
#'   with study-level characteristics.
#' @param title Table title (default: "Study Characteristics")
#' @param columns Character vector of column names. Default columns:
#'   "Study", "Design", "N", "Treatment", "Comparator", "Population".
#' @param include_metadata Logical, include study metadata columns
#'   (default: TRUE).
#' @param footnotes Character vector of footnotes to add.
#' @param ... Additional arguments passed to [create_clinical_table()],
#'   such as `col_widths`, `autofit`, `theme`, etc.
#'
#' @return A ClinicalTable object containing the study characteristics table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' studies <- list(
#'   TwoArmStudy(
#'     study_id = "STUDY001",
#'     study_title = "Phase III Trial of Drug X vs Placebo",
#'     design = "rct",
#'     population = "ITT"
#'   ),
#'   TwoArmStudy(
#'     study_id = "STUDY002",
#'     study_title = "Phase III Trial of Drug X vs Standard of Care",
#'     design = "rct",
#'     population = "FAS"
#'   )
#' )
#'
#' table <- create_study_characteristics_table(studies)
#' }
create_study_characteristics_table <- function(
	data,
	title = "Study Characteristics",
	columns = c(
		"Study",
		"Design",
		"N",
		"Treatment",
		"Comparator",
		"Population"
	),
	include_metadata = TRUE,
	footnotes = character(),
	...
) {
	# Build table data
	study_rows <- lapply(data, function(s) {
		.build_study_characteristic_row(
			study = s,
			include_metadata = include_metadata
		)
	})

	# Convert to data frame using dplyr::bind_rows for proper column preservation
	result_df <- dplyr::bind_rows(study_rows)
	rownames(result_df) <- NULL

	# Apply column subsetting
	if (!all(columns %in% names(result_df))) {
		missing_cols <- setdiff(columns, names(result_df))
		if (length(missing_cols) > 0) {
			ph_warn(paste(
				"Requested columns not available:",
				paste(missing_cols, collapse = ", ")
			))
		}
		columns <- intersect(columns, names(result_df))
	}
	result_df <- result_df[, columns, drop = FALSE]

	# Default footnotes
	default_footnotes <- c(
		"ITT = Intent-To-Treat Population",
		"FAS = Full Analysis Set",
		"N = Number of randomized subjects"
	)

	# Use create_clinical_table factory
	create_clinical_table(
		data = result_df,
		type = "study_characteristics",
		title = title,
		footnotes = c(footnotes, default_footnotes),
		theme = "hta",
		metadata = list(
			columns = columns,
			n_studies = length(data)
		),
		...
	)
}


#' Export Evidence Table
#'
#' Exports an evidence summary table to Word, HTML, or Excel format.
#'
#' @param table ClinicalTable object from [create_evidence_summary_table()]
#'   or [create_study_characteristics_table()].
#' @param file Character string for output file path. Extension
#'   determines format:
#'   - `.docx`: Microsoft Word document
#'   - `.html`: HTML file
#'   - `.xlsx`: Microsoft Excel file
#' @param title Character string for document title (used in Word/HTML exports).
#' @param ... Additional arguments passed to export functions:
#'   - For Word: `template` (officer template path), `header_footer`
#'     (list with header/footer text)
#'   - For HTML: `css` (custom CSS string), `standalone`
#'     (wrap in HTML boilerplate)
#'   - For Excel: `sheet_name` (worksheet name), `append` (logical)
#'
#' @return Invisible NULL. Writes file to disk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create table
#' table <- create_evidence_summary_table(endpoints)
#'
#' # Export to Word
#' export_evidence_table(table, "evidence_summary.docx")
#'
#' # Export to HTML
#' export_evidence_table(table, "evidence_summary.html")
#'
#' # Export to Excel
#' export_evidence_table(table, "evidence_summary.xlsx")
#' }
export_evidence_table <- function(
	table,
	file,
	title = NULL,
	...
) {
	# Validate input
	if (!S7::S7_inherits(table, ClinicalTable)) {
		ph_abort("'table' must be a ClinicalTable object")
	}

	# Determine format from file extension
	ext <- tolower(tools::file_ext(file))

	if (!ext %in% c("docx", "html", "xlsx")) {
		ph_abort(
			sprintf(
				"Unsupported file extension '.%s'. Use .docx, .html, or .xlsx",
				ext
			)
		)
	}

	# Get table data and title
	table_data <- table@data
	table_title <- title %||% table@title %||% "Evidence Summary"

	# Dispatch to appropriate export function
	switch(
		ext,
		docx = .export_to_word(table_data, table_title, file, ...),
		html = .export_to_html(table_data, table_title, file, ...),
		xlsx = .export_to_excel(table_data, file, ...)
	)

	invisible(NULL)
}


#' Create Risk of Bias Summary Table
#'
#' Creates a summary table of risk of bias assessments across studies
#' and domains, suitable for G-BA Module 4 requirements.
#'
#' @param data List of RoB2Result objects (one per study).
#' @param title Table title (default: "Risk of Bias Assessment").
#' @param include_justification Logical, include justification column
#'   (default: FALSE).
#' @param footnotes Character vector of footnotes.
#' @param ... Additional arguments passed to [create_clinical_table()],
#'   such as `col_widths`, `autofit`, `theme`, etc.
#'
#' @return A ClinicalTable object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rob_results <- list(
#'   RoB2Result(study_id = "Study 1", ...),
#'   RoB2Result(study_id = "Study 2", ...)
#' )
#' rob_table <- create_rob_summary_table(rob_results)
#' }
create_rob_summary_table <- function(
	data,
	title = "Risk of Bias Assessment",
	include_justification = FALSE,
	footnotes = character(),
	...
) {
	if (!is.list(data) || length(data) == 0) {
		ph_abort("'data' must be a non-empty list of RoB2Result objects")
	}

	# Validate all elements are RoB2Result
	for (i in seq_along(data)) {
		if (!S7::S7_inherits(data[[i]], RoB2Result)) {
			ph_abort(sprintf(
				"Element %d of 'data' must be a RoB2Result object",
				i
			))
		}
	}

	# Build summary data frame
	summary_data <- lapply(data, function(r) {
		.build_rob_summary_row(r, include_justification)
	})

	result_df <- dplyr::bind_rows(summary_data)
	rownames(result_df) <- NULL

	# Default footnotes
	default_footnotes <- c(
		"D1-D5: RoB 2 domains",
		"Overall: Overall risk of bias judgment"
	)

	create_clinical_table(
		data = result_df,
		type = "rob_summary",
		title = title,
		footnotes = c(footnotes, default_footnotes),
		theme = "hta",
		metadata = list(
			n_studies = length(data),
			include_justification = include_justification
		),
		...
	)
}


# =============================================================================
# Internal Helper Functions
# =============================================================================

#' Build Evidence Summary Row
#'
#' Internal helper to build a single row of data for the evidence summary table.
#'
#' @param endpoint_data List containing endpoint components (result, grade, rob)
#' @param name Character string for endpoint name
#' @param conf_level Numeric confidence level
#' @param language Language setting ("en" or "de")
#'
#' @return List of row data with all column values
#' @keywords internal
.build_evidence_summary_row <- function(
	endpoint_data,
	name,
	conf_level,
	language
) {
	# Extract result object
	result <- endpoint_data$result
	if (is.null(result)) {
		ph_abort(sprintf("Endpoint '%s' is missing 'result' component", name))
	}

	# Extract grade object
	grade <- endpoint_data$grade

	# Extract RoB data
	rob <- endpoint_data$rob

	# Build row data
	row_data <- list()

	# Endpoint label
	row_data[["Endpoint"]] <- endpoint_data$label %||% name

	# Number of studies using S7 property
	n_studies <- if (S7::S7_inherits(result, StatResult)) {
		result@n
	} else if (is.list(result) && !is.null(result$n)) {
		result$n
	} else {
		NA_integer_
	}
	row_data[["N Studies"]] <- n_studies

	# Effect estimate and CI
	row_data[["Effect (95% CI)"]] <- .format_effect_ci(
		result = result,
		conf_level = conf_level,
		language = language
	)

	# P-value using S7 property
	pval <- if (S7::S7_inherits(result, StatResult)) {
		result@p_value
	} else if (is.list(result) && !is.null(result$p_value)) {
		result$p_value
	} else {
		NA_real_
	}
	row_data[["p-value"]] <- format_pvalue(
		pval,
		locale = if (language == "de") "de" else "en"
	)

	# I2 heterogeneity using S7 property
	i2 <- if (S7::S7_inherits(result, MetaResult)) {
		heterogeneity <- result@heterogeneity
		if (is.list(heterogeneity)) heterogeneity$I2 else NA_real_
	} else if (is.list(result) && !is.null(result$heterogeneity)) {
		result$heterogeneity$I2
	} else {
		NA_real_
	}
	row_data[["I2"]] <- if (is.na(i2)) {
		"--"
	} else {
		sprintf("%.0f%%", i2)
	}

	# Risk of bias summary
	row_data[["RoB"]] <- .format_rob_summary(
		rob = rob,
		grade = grade,
		language = language
	)

	# Evidence grade
	row_data[["Grade"]] <- .format_grade(
		grade = grade,
		language = language
	)

	row_data
}


#' Build Study Characteristic Row
#'
#' Internal helper to build a single row of data for the study characteristics
#' table.
#'
#' @param study Study object, list, or data frame containing study information
#' @param include_metadata Logical, include metadata columns
#'
#' @return List of row data with all column values
#' @keywords internal
.build_study_characteristic_row <- function(study, include_metadata) {
	# Handle different input types
	if (S7::S7_inherits(study, Study)) {
		# Extract from Study object using S7 properties
		study_id <- study@study_id
		design <- study@design
		population <- study@population

		# Get treatment/comparator based on study type
		if (S7::S7_inherits(study, TwoArmStudy)) {
			treatment <- study@treatment_var %||% "Treatment"
			comparator <- study@comparator
		} else if (S7::S7_inherits(study, SingleArmStudy)) {
			treatment <- study@treatment_var %||% study_id
			comparator <- "Single arm"
		} else {
			treatment <- "--"
			comparator <- "--"
		}

		# Get N from metadata using S7 property
		metadata <- study@metadata
		n <- if (!is.null(metadata$n)) {
			sprintf("%d", metadata$n)
		} else if (!is.null(metadata$sample_size)) {
			sprintf("%d", metadata$sample_size)
		} else {
			"--"
		}
	} else if (is.data.frame(study)) {
		# Already a data frame - use first row
		study_id <- study$study_id[1] %||% study$Study[1] %||% "--"
		design <- study$design[1] %||% study$Design[1] %||% "--"
		n <- as.character(study$n[1] %||% study$N[1] %||% "--")
		treatment <- study$treatment[1] %||% study$Treatment[1] %||% "--"
		comparator <- study$comparator[1] %||% study$Comparator[1] %||% "--"
		population <- study$population[1] %||% study$Population[1] %||% "--"
	} else if (is.list(study)) {
		# List with named elements
		study_id <- study$study_id %||% study$Study %||% "--"
		design <- study$design %||% study$Design %||% "--"
		n <- as.character(study$n %||% study$N %||% "--")
		treatment <- study$treatment %||% study$Treatment %||% "--"
		comparator <- study$comparator %||% study$Comparator %||% "--"
		population <- study$population %||% study$Population %||% "--"
	} else {
		ph_abort(
			"Each element of 'data' must be a Study object, list, or data.frame"
		)
	}

	# Format design for display
	design_label <- switch(
		tolower(design),
		"rct" = "Randomized Controlled Trial",
		"observational" = "Observational Study",
		"single-arm" = "Single-Arm Study",
		"crossover" = "Crossover Trial",
		toupper(design)
	)

	list(
		Study = study_id,
		Design = design_label,
		N = n,
		Treatment = treatment,
		Comparator = comparator,
		Population = population
	)
}


#' Build RoB Summary Row
#'
#' Internal helper to build a single row of data for the RoB summary table.
#'
#' @param rob_result RoB2Result object containing risk of bias assessment
#' @param include_justification Logical, include justification column
#'
#' @return List of row data with all column values
#' @keywords internal
.build_rob_summary_row <- function(rob_result, include_justification) {
	# Extract values using S7 properties
	study_id <- rob_result@study_id
	outcome <- rob_result@outcome
	overall <- rob_result@overall
	domains <- rob_result@domains

	# Build row with domain judgments using S7 properties
	row <- list(
		Study = study_id,
		Outcome = outcome,
		D1_Randomization = domains$D1_randomization$judgment,
		D2_Deviations = domains$D2_deviations$judgment,
		D3_Missing_Data = domains$D3_missing_data$judgment,
		D4_Measurement = domains$D4_measurement$judgment,
		D5_Selection = domains$D5_selection$judgment,
		Overall = overall
	)

	if (include_justification) {
		overall_justification <- rob_result@overall_justification
		row$Justification <- overall_justification
	}

	row
}


#' @keywords internal
.format_effect_ci <- function(result, conf_level, language) {
	# Extract effect measure and estimate using S7 property
	if (S7::S7_inherits(result, StatResult)) {
		effect_measure <- result@effect_measure %||% "hr"
		estimate <- result@estimate
		ci <- result@ci
	} else if (is.list(result)) {
		effect_measure <- result$effect_measure %||% result$effect %||% "hr"
		estimate <- result$estimate
		ci <- result$ci
	} else {
		return("--")
	}

	# Map effect measure to symbol
	effect_symbol <- switch(
		tolower(effect_measure),
		"hr" = "HR",
		"or" = "OR",
		"rr" = "RR",
		"rd" = "RD",
		"md" = "MD",
		"smd" = "SMD",
		"irr" = "IRR",
		toupper(effect_measure)
	)

	# Format with locale-aware decimal separator
	dec_sep <- if (language == "de") "," else "."

	if (is.na(estimate) || length(ci) != 2 || anyNA(ci)) {
		return("--")
	}

	# Format numbers
	est_str <- format_number(
		estimate,
		digits = 2,
		locale = if (language == "de") "de" else "en",
		trim = TRUE
	)
	ci_lower <- format_number(
		ci[1],
		digits = 2,
		locale = if (language == "de") "de" else "en",
		trim = TRUE
	)
	ci_upper <- format_number(
		ci[2],
		digits = 2,
		locale = if (language == "de") "de" else "en",
		trim = TRUE
	)

	sprintf("%s %s [%s; %s]", effect_symbol, est_str, ci_lower, ci_upper)
}


#' @keywords internal
.format_rob_summary <- function(rob, grade, language) {
	# If rob is provided, extract from RoB object(s) first using S7 property
	if (!is.null(rob)) {
		if (S7::S7_inherits(rob, RoB2Result)) {
			rob_level <- tolower(rob@overall)
		} else if (is.list(rob) && length(rob) > 0) {
			# Multiple studies - summarize
			levels <- vapply(
				rob,
				function(r) {
					if (S7::S7_inherits(r, RoB2Result)) {
						tolower(r@overall)
					} else {
						"unknown"
					}
				},
				character(1)
			)
			rob_level <- names(sort(table(levels), decreasing = TRUE))[1]
		} else {
			rob_level <- "unknown"
		}
		# Otherwise, if grade is provided, extract RoB from domains using S7 property
	} else if (!is.null(grade) && S7::S7_inherits(grade, EvidenceGrade)) {
		domains <- grade@domains
		if (
			length(domains) > 0 &&
				!is.null(domains$limitations)
		) {
			rob_level <- domains$limitations$level
		} else {
			rob_level <- "unknown"
		}
	} else {
		rob_level <- "unknown"
	}

	# Map to display format
	display_map <- list(
		"low" = if (language == "de") "Niedrig" else "Low",
		"some_concerns" = if (language == "de") {
			"Einige Bedenken"
		} else {
			"Some concerns"
		},
		"high" = if (language == "de") "Hoch" else "High",
		"unknown" = "--"
	)

	display_map[[rob_level]] %||% "--"
}


#' @keywords internal
.format_grade <- function(grade, language) {
	if (is.null(grade)) {
		return("--")
	}

	if (S7::S7_inherits(grade, EvidenceGrade)) {
		if (language == "de") {
			grade@grade_de %||% "--"
		} else {
			grade_value <- grade@grade
			switch(
				grade_value,
				"proof" = "Proof",
				"indication" = "Indication",
				"hint" = "Hint",
				"none" = "No proof",
				grade_value
			)
		}
	} else if (is.list(grade)) {
		if (language == "de") {
			grade$grade_de %||% grade$grade %||% "--"
		} else {
			switch(
				grade$grade,
				"proof" = "Proof",
				"indication" = "Indication",
				"hint" = "Hint",
				"none" = "No proof",
				grade$grade %||% "--"
			)
		}
	} else {
		"--"
	}
}


#' @keywords internal
.export_to_word <- function(data, title, file, ...) {
	args <- list(...)

	# Use create_clinical_table factory for consistency
	ct <- create_clinical_table(
		data = data,
		type = "evidence_summary",
		title = title,
		footnotes = args$footnotes %||% character(),
		theme = "hta"
	)

	# Extract flextable from ClinicalTable
	ft <- ct@flextable

	# Add to Word document
	if (!is.null(args$template) && file.exists(args$template)) {
		# Use template
		doc <- officer::read_docx(args$template)
	} else {
		# Create new document
		doc <- officer::read_docx()
	}

	# Add title and table
	doc <- doc |>
		officer::body_add_par(title, style = "heading 1") |>
		flextable::body_add_flextable(ft)

	# Save
	print(doc, target = file)
}


#' @keywords internal
.export_to_html <- function(data, title, file, ...) {
	args <- list(...)

	# Use create_clinical_table factory for consistency
	ct <- create_clinical_table(
		data = data,
		type = "evidence_summary",
		title = title,
		footnotes = args$footnotes %||% character(),
		theme = "hta"
	)

	# Extract flextable from ClinicalTable
	ft <- ct@flextable

	# Convert to HTML and ensure it's character
	html_content <- as.character(flextable::htmltools_value(ft))

	# Wrap in standalone HTML if requested
	if (isTRUE(args$standalone)) {
		css <- args$css %||%
			paste0(
				"<style>",
				"table { border-collapse: collapse; width: 100%; font-family: Arial; }",
				"th, td { border: 1px solid black; padding: 8px; text-align: left; }",
				"th { background-color: #E8E8E8; font-weight: bold; }",
				".title { font-size: 14pt; font-weight: bold; margin-bottom: 10px; }",
				".footer { font-size: 8pt; font-style: italic; margin-top: 10px; }",
				"</style>"
			)
		html_content <- paste0(
			"<!DOCTYPE html>\n",
			"<html>\n<head>\n",
			"<title>",
			title,
			"</title>\n",
			css,
			"\n",
			"</head>\n<body>\n",
			"<div class='title'>",
			title,
			"</div>\n",
			html_content,
			"</body>\n</html>"
		)
	}

	# Write file
	writeLines(html_content, file)
}


#' @keywords internal
.export_to_excel <- function(data, file, ...) {
	args <- list(...)

	# Prepare data for Excel (ensure character matrix)
	excel_data <- as.data.frame(
		lapply(data, function(col) {
			if (is.numeric(col)) {
				as.character(col)
			} else {
				col
			}
		}),
		stringsAsFactors = FALSE
	)

	# Use writexl for Excel export (requires writexl package)
	if (requireNamespace("writexl", quietly = TRUE)) {
		writexl::write_xlsx(excel_data, path = file)
	} else {
		# Fallback to openxlsx if available
		if (requireNamespace("openxlsx", quietly = TRUE)) {
			sheet_name <- args$sheet_name %||% "Evidence Summary"
			openxlsx::write.xlsx(
				excel_data,
				file = file,
				sheetName = sheet_name,
				...
			)
		} else {
			ph_abort(
				"Either 'writexl' or 'openxlsx' package is required for Excel export"
			)
		}
	}
}
