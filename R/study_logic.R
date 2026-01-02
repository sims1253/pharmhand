#' Study Logic
#'
#' Analyze one-arm and two-arm studies with S7 classes.
#'
#' @name study_logic
NULL

#' Analyze Study (S7 Method)
#'
#' @param x Study object (SingleArmStudy, TwoArmStudy, or MultiArmStudy)
#' @param ... Additional arguments
#'
#' @export
analyze_study <- S7::new_generic("analyze_study", "x")

#' Analyze SingleArmStudy
#'
#' Analyze SingleArmStudy objects.
#'
#' @param x A SingleArmStudy object
#' @param ... Additional arguments
#'
#' @return The modified SingleArmStudy object with results
#' @export
#' @name analyze_study_SingleArmStudy
analyze_study_SingleArmStudy <- S7::method(
	analyze_study,
	SingleArmStudy
) <- function(
	x,
	...
) {
	# Implementation using ADaMData and core analysis
	adam <- ADaMData(data = x@data, trt_var = "TRT01P")

	# Baseline analysis - exclude ID and treatment columns explicitly
	all_vars <- names(x@data)
	baseline_vars <- all_vars[!all_vars %in% c("USUBJID", "TRT01P")]
	baseline <- calculate_baseline(adam, vars = baseline_vars)

	results <- list(baseline = baseline)

	# Only analyze safety if AE columns exist
	if (all(c("AEBODSYS", "AEDECOD") %in% names(x@data))) {
		results$safety <- analyze_soc_pt(adam)
	}

	# Store results
	x@results <- results
	return(x)
}

#' Analyze TwoArmStudy
#'
#' S7 method for analyzing TwoArmStudy objects.
#'
#' @param x A TwoArmStudy object
#' @param ... Additional arguments (currently unused, for method consistency)
#'
#' @return The modified TwoArmStudy object with results
#' @export
#' @name analyze_study_TwoArmStudy
analyze_study_TwoArmStudy <- S7::method(analyze_study, TwoArmStudy) <- function(
	x,
	...
) {
	# Analyze
	adam <- ADaMData(data = x@data, trt_var = x@treatment_var)

	results <- list()

	# Only analyze columns that exist
	all_vars <- names(x@data)
	baseline_vars <- all_vars[!all_vars %in% c(x@treatment_var, "USUBJID")]

	results$baseline <- calculate_baseline(adam, vars = baseline_vars)

	# Only analyze safety if AE columns exist
	if (all(c("AEBODSYS", "AEDECOD") %in% names(x@data))) {
		results$safety <- analyze_soc_pt(adam)
	}

	x@results <- results
	return(x)
}

#' Create Report from Study
#'
#' @param x Study object
#' @param title Character string for report title
#'
#' @return ClinicalReport object
#' @export
create_study_report <- function(x, title = NULL) {
	if (is.null(title)) {
		title <- x@study_title
	}

	sections <- list()

	if ("baseline" %in% names(x@results)) {
		sections[[length(sections) + 1]] <- ReportSection(
			title = "Baseline Characteristics",
			section_type = "baseline",
			content = list(create_clinical_table(
				x@results$baseline,
				"Demographics Summary"
			))
		)
	}

	if ("safety" %in% names(x@results)) {
		sections[[length(sections) + 1]] <- ReportSection(
			title = "Safety Analysis",
			section_type = "safety",
			content = list(create_clinical_table(
				x@results$safety,
				"Adverse Events by SOC/PT"
			))
		)
	}

	ClinicalReport(
		study_id = x@study_id,
		study_title = title,
		sections = sections
	)
}
