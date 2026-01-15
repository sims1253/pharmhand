#' Evidence Summary Tables
#'
#' Functions for creating evidence summary tables for IQWiG/G-BA dossiers
#' and health technology assessment submissions.
#'
#' @name evidence_summary_tables
NULL

#' Create Evidence Summary Table
#'
#' Creates a formatted evidence summary table displaying study results,
#' effect estimates, heterogeneity statistics, risk of bias assessments,
#' and evidence grades for multiple endpoints.
#'
#' @param endpoints List of named elements containing endpoint data. Each element
#'   should be a list with components:
#'   - `result`: MetaResult or ComparisonResult object with effect estimate,
#'     confidence interval, p-value, and study count
#'   - `grade`: EvidenceGrade object with evidence assessment
#'   - `rob`: RoB2Result object or list of RoB2Result objects for risk of bias
#'     (optional, can be derived from grade)
#'   - `label`: Character string for endpoint display name (optional)
#' @param title Table title (default: "Evidence Summary")
#' @param columns Character vector of column names to include. Default columns:
#'   "Endpoint", "N Studies", "Effect (95% CI)", "p-value", "I2", "RoB", "Grade".
#' @param conf_level Numeric confidence level for CIs (default: 0.95)
#' @param language Output language: "en" for English, "de" for German.
#'   Default: "en".
#' @param footnotes Character vector of footnotes to add to the table.
#' @param col_widths Named numeric vector of column widths (in inches).
#' @param autofit Logical, whether to autofit column widths (default: TRUE).
#'
#' @return A ClinicalTable object containing the formatted evidence summary table.
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
	endpoints,
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
	col_widths = NULL,
	autofit = TRUE
) {
	language <- match.arg(language)

	# Validate endpoints input
	if (!is.list(endpoints) || length(endpoints) == 0) {
		ph_abort("'endpoints' must be a non-empty list")
	}

	# Build table data row by row
	table_data <- lapply(names(endpoints), function(name) {
		ep <- endpoints[[name]]

		# Extract result object
		result <- ep$result
		if (is.null(result)) {
			ph_abort(sprintf("Endpoint '%s' is missing 'result' component", name))
		}

		# Extract grade object
		grade <- ep$grade

		# Extract RoB data
		rob <- ep$rob

		# Build row data
		row_data <- list()

		# Endpoint label
		row_data[["Endpoint"]] <- ep$label %||% name

		# Number of studies
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

		# P-value
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

		# I2 heterogeneity
		i2 <- if (S7::S7_inherits(result, MetaResult)) {
			result@heterogeneity$I2
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
	})

	# Convert to data frame
	result_df <- as.data.frame(
		do.call(rbind.data.frame, table_data),
		stringsAsFactors = FALSE
	)
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
		metadata = list(
			columns = columns,
			language = language,
			conf_level = conf_level,
			n_endpoints = length(endpoints)
		),
		col_widths = col_widths,
		autofit = autofit
	)
}


#' Create Study Characteristics Table
#'
#' Creates a summary table of study characteristics for G-BA Module 4
#' requirements, displaying design, sample size, treatment, and comparator
#' information for each included study.
#'
#' @param studies List of Study objects, TwoArmStudy objects, or data frame
#'   with study-level characteristics.
#' @param title Table title (default: "Study Characteristics")
#' @param columns Character vector of column names. Default columns:
#'   "Study", "Design", "N", "Treatment", "Comparator", "Population".
#' @param include_metadata Logical, include study metadata columns (default: TRUE).
#' @param footnotes Character vector of footnotes to add.
#' @param col_widths Named numeric vector of column widths (in inches).
#' @param autofit Logical, whether to autofit column widths (default: TRUE).
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
	studies,
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
	col_widths = NULL,
	autofit = TRUE
) {
	# Build table data
	study_rows <- lapply(studies, function(s) {
		# Handle different input types
		if (S7::S7_inherits(s, Study)) {
			# Extract from Study object
			study_id <- s@study_id
			design <- s@design
			population <- s@population

			# Get treatment/comparator based on study type
			if (S7::S7_inherits(s, TwoArmStudy)) {
				treatment <- s@treatment_var %||% "Treatment"
				comparator <- s@comparator
			} else if (S7::S7_inherits(s, SingleArmStudy)) {
				treatment <- s@treatment_var %||% s@study_id
				comparator <- "Single arm"
			} else {
				treatment <- "--"
				comparator <- "--"
			}

			# Get N from metadata or data
			n <- if (!is.null(s@metadata$n)) {
				sprintf("%d", s@metadata$n)
			} else if (!is.null(s@metadata$sample_size)) {
				sprintf("%d", s@metadata$sample_size)
			} else {
				"--"
			}
		} else if (is.data.frame(s)) {
			# Already a data frame - use first row
			study_id <- s$study_id[1] %||% s$Study[1] %||% "--"
			design <- s$design[1] %||% s$Design[1] %||% "--"
			n <- as.character(s$n[1] %||% s$N[1] %||% "--")
			treatment <- s$treatment[1] %||% s$Treatment[1] %||% "--"
			comparator <- s$comparator[1] %||% s$Comparator[1] %||% "--"
			population <- s$population[1] %||% s$Population[1] %||% "--"
		} else if (is.list(s)) {
			# List with named elements
			study_id <- s$study_id %||% s$Study %||% "--"
			design <- s$design %||% s$Design %||% "--"
			n <- as.character(s$n %||% s$N %||% "--")
			treatment <- s$treatment %||% s$Treatment %||% "--"
			comparator <- s$comparator %||% s$Comparator %||% "--"
			population <- s$population %||% s$Population %||% "--"
		} else {
			ph_abort(
				"Each element of 'studies' must be a Study object, list, or data.frame"
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
	})

	# Convert to data frame
	result_df <- as.data.frame(
		do.call(rbind.data.frame, study_rows),
		stringsAsFactors = FALSE
	)
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
		metadata = list(
			columns = columns,
			n_studies = length(studies)
		),
		col_widths = col_widths,
		autofit = autofit
	)
}


#' Export Evidence Table
#'
#' Exports an evidence summary table to Word, HTML, or Excel format.
#'
#' @param table ClinicalTable object from [create_evidence_summary_table()]
#'   or [create_study_characteristics_table()].
#' @param file Character string for output file path. Extension determines format:
#'   - `.docx`: Microsoft Word document
#'   - `.html`: HTML file
#'   - `.xlsx`: Microsoft Excel file
#' @param title Character string for document title (used in Word/HTML exports).
#' @param ... Additional arguments passed to export functions:
#'   - For Word: `template` (officer template path), `header_footer` (list with
#'     header/footer text)
#'   - For HTML: `css` (custom CSS string), `standalone` (wrap in HTML boilerplate)
#'   - For Excel: `sheet_name` (worksheet name), `append` (logical for appending)
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


# =============================================================================
# Internal Helper Functions
# =============================================================================

#' @keywords internal
.format_effect_ci <- function(result, conf_level, language) {
	# Extract effect measure and estimate
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
	# If grade is provided, extract RoB from domains
	if (!is.null(grade) && S7::S7_inherits(grade, EvidenceGrade)) {
		if (
			length(grade@domains) > 0 &&
				!is.null(grade@domains$limitations)
		) {
			rob_level <- grade@domains$limitations$level
		} else {
			rob_level <- "unknown"
		}
	} else if (!is.null(rob)) {
		# Extract from RoB object(s)
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
			switch(
				grade@grade,
				"proof" = "Proof",
				"indication" = "Indication",
				"hint" = "Hint",
				"none" = "No proof",
				grade@grade
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

	# Create flextable
	ft <- create_hta_table(
		data = data,
		title = title,
		footnotes = args$footnotes
	)

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
		officer::body_add_title(title, style = "Heading 1") |>
		officer::body_add_flextable(ft)

	# Save
	print(doc, target = file)
}


#' @keywords internal
.export_to_html <- function(data, title, file, ...) {
	args <- list(...)

	# Create flextable
	ft <- create_hta_table(
		data = data,
		title = title,
		footnotes = args$footnotes
	)

	# Convert to HTML
	html_content <- flextable::htmltools_value(ft)

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
		sheet_name <- args$sheet_name %||% "Evidence Summary"
		writexl::write_xlsx(
			excel_data,
			path = file,
			sheet_name = sheet_name
		)
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


# =============================================================================
# RoB Summary Data Frame
# =============================================================================

#' Create Risk of Bias Summary Table
#'
#' Creates a summary table of risk of bias assessments across studies
#' and domains, suitable for G-BA Module 4 requirements.
#'
#' @param rob_results List of RoB2Result objects (one per study).
#' @param title Table title (default: "Risk of Bias Assessment").
#' @param include_justification Logical, include justification column
#'   (default: FALSE).
#' @param footnotes Character vector of footnotes.
#' @param autofit Logical, autofit column widths (default: TRUE).
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
	rob_results,
	title = "Risk of Bias Assessment",
	include_justification = FALSE,
	footnotes = character(),
	autofit = TRUE
) {
	if (!is.list(rob_results) || length(rob_results) == 0) {
		ph_abort("'rob_results' must be a non-empty list of RoB2Result objects")
	}

	# Validate all elements are RoB2Result
	for (i in seq_along(rob_results)) {
		if (!S7::S7_inherits(rob_results[[i]], RoB2Result)) {
			ph_abort(sprintf(
				"Element %d of 'rob_results' must be a RoB2Result object",
				i
			))
		}
	}

	# Build summary data frame
	summary_data <- lapply(rob_results, function(r) {
		row <- list(
			Study = r@study_id,
			Outcome = r@outcome,
			D1_Randomization = r@domains$D1_randomization$judgment,
			D2_Deviations = r@domains$D2_deviations$judgment,
			D3_Missing_Data = r@domains$D3_missing_data$judgment,
			D4_Measurement = r@domains$D4_measurement$judgment,
			D5_Selection = r@domains$D5_selection$judgment,
			Overall = r@overall
		)
		if (include_justification) {
			row$Justification <- r@overall_justification
		}
		row
	})

	result_df <- as.data.frame(
		do.call(rbind.data.frame, summary_data),
		stringsAsFactors = FALSE
	)
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
		metadata = list(
			n_studies = length(rob_results),
			include_justification = include_justification
		),
		autofit = autofit
	)
}
