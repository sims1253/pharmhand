#' Evidence Narrative Generation
#'
#' Functions for automatic generation of evidence narratives from structured
#' meta-analysis results, supporting IQWiG/G-BA style reporting.
#'
#' @name evidence_narrative
#' @references
#'   IQWiG Allgemeine Methoden Version 3.1.4, Section 3.1, p.51-56.
#'   \url{https://www.iqwig.de/methoden/allgemeine-methoden-version-3-1.pdf}
#'
NULL

# =============================================================================
# Template System
# =============================================================================

#' Narrative Template System
#'
#' Predefined templates for evidence narratives. Templates use placeholder
#' syntax compatible with [glue::glue()]: `{endpoint}`, `{estimate}`, `{ci}`,
#' `{p_value}`, `{n_studies}`, `{n_patients}`, `{effect_dir}`, etc.
#'
#' Available templates:
#' \describe{
#'   \item{"iqwig"}{German IQWiG style with formal terminology}
#'   \item{"clinical"}{Clinical study report style (English)}
#'   \item{"plain"}{Plain text without specialized terminology}
#' }
#'
#' @keywords internal
NARRATIVE_TEMPLATES <- list(
	# German IQWiG style template
	"iqwig" = list(
		effect_significant = paste0(
			"F\u00fcr den Endpunkt {endpoint} zeigt die Metaanalyse von ",
			"{n_studies} Studien (N={n_patients}) einen statistisch signifikanten ",
			"Effekt zugunsten der Pr\u00fcfintervention ({effect_measure_label} ",
			"{estimate} [{ci_level}%-KI: {ci_formatted}; p={p_formatted}]). "
		),
		effect_nonsignificant = paste0(
			"F\u00fcr den Endpunkt {endpoint} zeigt die Metaanalyse von ",
			"{n_studies} Studien (N={n_patients}) keinen statistisch signifikanten ",
			"Effekt ({effect_measure_label} {estimate} [{ci_level}%-KI: ",
			"{ci_formatted}; p={p_formatted}]). "
		),
		single_study = paste0(
			"F\u00fcr den Endpunkt {endpoint} zeigt die Studie mit {n_patients} ",
			"Patienten einen {effect_direction_adj} Effekt ({effect_measure_label} ",
			"{estimate} [{ci_level}%-KI: {ci_formatted}; p={p_formatted}]). "
		),
		heterogeneity_low = paste0(
			"Die Heterogenit\u00e4t zwischen den Studien war ",
			"gering (I\u00b2={i2}%)."
		),
		heterogeneity_moderate = paste0(
			"Die Heterogenit\u00e4t zwischen den Studien war ",
			"m\u00e4\u00dfig (I\u00b2={i2}%)."
		),
		heterogeneity_substantial = paste0(
			"Die Heterogenit\u00e4t zwischen den Studien war ",
			"substantiell (I\u00b2={i2}%)."
		),
		heterogeneity_considerable = paste0(
			"Die Heterogenit\u00e4t zwischen den Studien war ",
			"betr\u00e4chtlich (I\u00b2={i2}%)."
		),
		heterogeneity_single = paste0(
			"Die Heterogenit\u00e4t kann bei einer einzelnen ",
			"Studie nicht beurteilt werden."
		),
		rob_low = "Das Verzerrungsrisiko wurde als niedrig bewertet.",
		rob_moderate = "Das Verzerrungsrisiko wurde als moderat bewertet.",
		rob_high = "Das Verzerrungsrisiko wurde als hoch bewertet.",
		rob_unknown = "Das Verzerrungsrisiko wurde nicht systematisch beurteilt.",
		grade_proof = paste0(
			"Es ergibt sich ein Beleg f\u00fcr einen ",
			"{effect_direction_adj} Nutzen."
		),
		grade_indication = paste0(
			"Es ergibt sich ein Hinweis auf einen ",
			"{effect_direction_adj} Nutzen."
		),
		grade_hint = paste0(
			"Es ergibt sich ein Anhaltspunkt f\u00fcr einen ",
			"{effect_direction_adj} Nutzen."
		),
		grade_none = "Es ergibt sich kein {effect_direction_adj} Nutzen.",
		effect_direction_benefit = "positiven",
		effect_direction_harm = "negativen",
		effect_direction_neutral = "keinen"
	),

	# Clinical/English style template
	"clinical" = list(
		effect_significant = paste0(
			"For the endpoint {endpoint}, the meta-analysis of {n_studies} studies ",
			"(N={n_patients}) demonstrated a statistically significant ",
			"{effect_direction_adj} effect in favor of the investigational ",
			"intervention ({effect_measure_label} {estimate} [{ci_level}% CI: ",
			"{ci_formatted}; p={p_formatted}]). "
		),
		effect_nonsignificant = paste0(
			"For the endpoint {endpoint}, the meta-analysis of {n_studies} studies ",
			"(N={n_patients}) did not demonstrate a statistically significant ",
			"effect ({effect_measure_label} {estimate} [{ci_level}% CI: ",
			"{ci_formatted}; p={p_formatted}]). "
		),
		single_study = paste0(
			"For the endpoint {endpoint}, the study with {n_patients} patients ",
			"showed a {effect_direction_adj} effect ({effect_measure_label} {estimate} ",
			"[{ci_level}% CI: {ci_formatted}; p={p_formatted}]. "
		),
		heterogeneity_low = paste0(
			"The heterogeneity between studies was low (I\u00b2={i2}%)."
		),
		heterogeneity_moderate = paste0(
			"The heterogeneity between studies was moderate (I\u00b2={i2}%)."
		),
		heterogeneity_substantial = paste0(
			"The heterogeneity between studies was substantial (I\u00b2={i2}%)."
		),
		heterogeneity_considerable = paste0(
			"The heterogeneity between studies was considerable (I\u00b2={i2}%)."
		),
		heterogeneity_single = paste0(
			"Heterogeneity could not be assessed with a single study."
		),
		rob_low = "The risk of bias was rated as low.",
		rob_moderate = "Some concerns were identified regarding risk of bias.",
		rob_high = "The risk of bias was rated as high.",
		rob_unknown = "Risk of bias was not systematically assessed.",
		grade_proof = "This provides proof of {effect_direction_adj} benefit.",
		grade_indication = paste0(
			"This provides an indication of {effect_direction_adj} benefit."
		),
		grade_hint = "This provides a hint of {effect_direction_adj} benefit.",
		grade_none = "No {effect_direction_adj} benefit was demonstrated.",
		effect_direction_benefit = "favorable",
		effect_direction_harm = "unfavorable",
		effect_direction_neutral = "neutral"
	),

	# Plain/minimal style template
	"plain" = list(
		effect_significant = paste0(
			"{endpoint}: {n_studies} studies, N={n_patients}. Effect: ",
			"{effect_measure_label} {estimate} [{ci_level}% CI: {ci_formatted}]; ",
			"p={p_formatted}. "
		),
		effect_nonsignificant = paste0(
			"{endpoint}: {n_studies} studies, N={n_patients}. Effect: ",
			"{effect_measure_label} {estimate} [{ci_level}% CI: {ci_formatted}]; ",
			"p={p_formatted}. Not significant. "
		),
		single_study = paste0(
			"{endpoint}: {n_patients} patients. Effect: {effect_measure_label} ",
			"{estimate} [{ci_level}% CI: {ci_formatted}]. "
		),
		heterogeneity_low = "Low heterogeneity (I\u00b2={i2}%).",
		heterogeneity_moderate = "Moderate heterogeneity (I\u00b2={i2}%).",
		heterogeneity_substantial = "Substantial heterogeneity (I\u00b2={i2}%).",
		heterogeneity_considerable = "Considerable heterogeneity (I\u00b2={i2}%).",
		heterogeneity_single = "Heterogeneity: N/A (single study).",
		rob_low = "Risk of bias: low.",
		rob_moderate = "Risk of bias: some concerns.",
		rob_high = "Risk of bias: high.",
		rob_unknown = "Risk of bias: not assessed.",
		grade_proof = "Evidence grade: proof.",
		grade_indication = "Evidence grade: indication.",
		grade_hint = "Evidence grade: hint.",
		grade_none = "Evidence grade: none.",
		effect_direction_benefit = "",
		effect_direction_harm = "",
		effect_direction_neutral = ""
	)
)


#' Get Narrative Template
#'
#' Retrieves a named template component for narrative generation.
#'
#' @param template_name Character. Template name: "iqwig", "clinical",
#'   or "plain".
#' @param component Character. Template component: "effect_significant",
#'   "effect_nonsificant", "single_study", "heterogeneity_*", "rob_*",
#'   "grade_*".
#' @param language Character. Output language: "en" or "de". Default: "en".
#'
#' @return Character string template.
#'
#' @keywords internal
.get_template <- function(template_name, component, language = "en") {
	templates <- NARRATIVE_TEMPLATES[[template_name]]
	if (is.null(templates)) {
		ph_abort(sprintf(
			"Template '%s' not found. Available: %s",
			template_name,
			paste(names(NARRATIVE_TEMPLATES), collapse = ", ")
		))
	}

	template <- templates[[component]]
	if (is.null(template)) {
		ph_abort(sprintf(
			"Template component '%s' not found in '%s' template",
			component,
			template_name
		))
	}

	template
}


#' Narrative Template
#'
#' Manages narrative templates for evidence reporting. Allows access to
#' predefined templates or creation of custom templates.
#'
#' @param template Character. Template name: "iqwig", "clinical", "plain",
#'   or a custom template string.
#' @param component Character. Template component to retrieve
#'   (for predefined templates only).
#' @param language Character. Language for output: "en" or "de".
#'   Default: "en".
#' @param list_templates Logical. If TRUE, returns list of available templates.
#'   Default: FALSE.
#'
#' @return For `list_templates = TRUE`: list of available template names.
#'   For `list_templates = FALSE`: character template string.
#'
#' @export
#'
#' @examples
#' # List available templates
#' narrative_template(list_templates = TRUE)
#'
#' # Get specific template component
#' narrative_template("iqwig", "effect_significant")
#'
#' # Get German template
#' narrative_template("iqwig", "effect_significant", language = "de")
narrative_template <- function(
	template = "iqwig",
	component = NULL,
	language = c("en", "de"),
	list_templates = FALSE
) {
	language <- match.arg(language)

	if (list_templates) {
		return(names(NARRATIVE_TEMPLATES))
	}

	# If template is a predefined name, get the component
	if (template %in% names(NARRATIVE_TEMPLATES)) {
		if (is.null(component)) {
			ph_abort("component must be specified for predefined templates")
		}
		return(.get_template(template, component, language))
	}

	# Check if template name looks like it might be a template reference
	# (doesn't contain placeholders and isn't in predefined list)
	if (!grepl("\\{[^}]+\\}", template)) {
		ph_abort(sprintf(
			"Template '%s' not found. Available: %s",
			template,
			paste(names(NARRATIVE_TEMPLATES), collapse = ", ")
		))
	}

	template
}


# =============================================================================
# Helper Functions
# =============================================================================

#' Format Effect Measure Label
#'
#' @param effect_measure Character. Effect measure type.
#' @param language Character. Language: "en" or "de".
#'
#' @return Character label.
#'
#' @keywords internal
.format_effect_measure <- function(effect_measure, language = "en") {
	labels <- c(
		hr = "HR",
		or = "OR",
		rr = "RR",
		rd = "RD",
		md = "MD",
		smd = "SMD",
		irr = "IRR"
	)

	labels[effect_measure] %||% effect_measure
}


#' Format P-value
#'
#' @param p_value Numeric p-value.
#' @param threshold Numeric. Threshold for formatting (below this, use "<").
#'   Default: 0.001.
#'
#' @return Formatted character string.
#'
#' @keywords internal
.format_p_value <- function(p_value, threshold = 0.001) {
	if (is.na(p_value)) {
		return("NA")
	}
	if (p_value < threshold) {
		sprintf("< %.3f", threshold)
	} else {
		sprintf("%.3f", p_value)
	}
}


#' Format Confidence Interval
#'
#' @param ci Numeric vector of length 2: c(lower, upper).
#' @param decimals Integer. Number of decimal places.
#' @param language Character. Language: "en" or "de".
#'
#' @return Formatted CI string.
#'
#' @keywords internal
.format_ci <- function(ci, decimals = 2, language = "en") {
	if (length(ci) != 2 || anyNA(ci)) {
		return("NA")
	}
	if (language == "de") {
		# German: comma as decimal separator
		formatted <- sprintf("%.2f; %.2f", ci[1], ci[2])
		formatted <- gsub("\\.", ",", formatted)
		formatted
	} else {
		sprintf("%.2f; %.2f", ci[1], ci[2])
	}
}


#' Assess Heterogeneity Level
#'
#' @param i2 Numeric I-squared value.
#'
#' @return Character: "low", "moderate", "substantial", or "considerable".
#'
#' @keywords internal
.assess_heterogeneity_level <- function(i2) {
	if (is.na(i2)) {
		return("unknown")
	}
	if (i2 < 25) {
		"low"
	} else if (i2 < 50) {
		"moderate"
	} else if (i2 < 75) {
		"substantial"
	} else {
		"considerable"
	}
}


#' Summarize Risk of Bias
#'
#' @param rob_results List of RoB2Result objects or NULL.
#' @param n_studies Integer. Number of studies.
#'
#' @return List with level and text summary.
#'
#' @keywords internal
.summarize_rob <- function(rob_results, n_studies) {
	if (is.null(rob_results)) {
		return(list(
			level = "unknown",
			text = "unknown"
		))
	}

	# Handle single RoB2Result
	if (S7::S7_inherits(rob_results, RoB2Result)) {
		rob_results <- list(rob_results)
	}

	if (length(rob_results) == 0) {
		return(list(level = "unknown", text = "unknown"))
	}

	# Count judgments
	n_low <- sum(vapply(rob_results, function(r) r@overall == "Low", logical(1)))
	n_high <- sum(vapply(
		rob_results,
		function(r) r@overall == "High",
		logical(1)
	))
	n_concerns <- sum(vapply(
		rob_results,
		function(r) {
			r@overall == "Some concerns"
		},
		logical(1)
	))

	n_valid <- length(rob_results)
	prop_low <- n_low / n_valid

	# Determine overall level
	if (prop_low >= 0.8) {
		level <- "low"
	} else if (n_high / n_valid >= 0.5) {
		level <- "high"
	} else {
		level <- "moderate"
	}

	list(
		level = level,
		text = sprintf(
			"%d/%d studies low risk of bias",
			n_low,
			n_valid
		)
	)
}


#' Get Effect Direction Text
#'
#' @param estimate Numeric effect estimate.
#' @param effect_measure Character effect measure type.
#' @param direction Character. "benefit", "harm", or "none".
#' @param language Character language.
#'
#' @return List with direction text and adjective.
#'
#' @keywords internal
.get_effect_direction_text <- function(
	estimate,
	effect_measure,
	direction,
	language = "en"
) {
	if (direction == "none") {
		# Infer from estimate
		if (effect_measure %in% c("hr", "or", "rr")) {
			direction <- if (estimate < 1) "benefit" else "harm"
		} else {
			direction <- if (estimate > 0) "harm" else "benefit"
		}
	}

	dir_text <- if (language == "de") {
		switch(
			direction,
			benefit = "g\u00fcnstigen",
			harm = "sch\u00e4dlichen",
			neutral = "keinen"
		)
	} else {
		switch(direction, benefit = "favorable", harm = "unfavorable", neutral = "")
	}

	list(
		direction = direction,
		text = dir_text
	)
}


# =============================================================================
# Main Narrative Functions
# =============================================================================

#' Generate Evidence Narrative
#'
#' Generates a structured narrative describing meta-analysis results
#' including effect estimates, risk of bias, heterogeneity, and evidence grade.
#'
#' @param endpoint Character. Name of the endpoint being described.
#' @param result A MetaResult or ComparisonResult object containing
#'   effect estimate, confidence interval, p-value, and heterogeneity.
#' @param grade An EvidenceGrade object (optional). If provided,
#'   evidence grade information is included in the narrative.
#' @param rob_results A list of RoB2Result objects or a single RoB2Result
#'   for single-study analysis (optional).
#' @param n_patients Integer. Total number of patients across studies
#'   (required for meta-analysis; for single study, taken from result@n).
#' @param template Character. Template name: "iqwig", "clinical", or "plain".
#'   Default: "iqwig".
#' @param language Character. Output language: "en" or "de".
#'   Default: "en".
#' @param ci_level Numeric. Confidence level as decimal (e.g., 0.95).
#'   Default: 0.95.
#'
#' @return Character string containing the formatted narrative.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create meta-analysis result
#' meta_res <- MetaResult(
#'   estimate = 0.75,
#'   ci = c(0.60, 0.94),
#'   p_value = 0.012,
#'   n = 3L,
#'   effect_measure = "hr",
#'   heterogeneity = list(I2 = 25, tau2 = 0.02)
#' )
#'
#' # Create evidence grade
#' grade <- EvidenceGrade(
#'   grade = "indication",
#'   grade_de = "Hinweis",
#'   direction = "benefit",
#'   n_studies = 3L
#' )
#'
#' # Generate German narrative (IQWiG style)
#' narrative <- generate_evidence_narrative(
#'   endpoint = "Gesamt\u00fcberleben",
#'   result = meta_res,
#'   grade = grade,
#'   n_patients = 1245,
#'   template = "iqwig",
#'   language = "de"
#' )
#' cat(narrative)
#'
#' # Generate English clinical narrative
#' narrative_en <- generate_evidence_narrative(
#'   endpoint = "Overall Survival",
#'   result = meta_res,
#'   grade = grade,
#'   n_patients = 1245,
#'   template = "clinical"
#' )
#'
#' # Single study example
#' comp_res <- ComparisonResult(
#'   estimate = 0.78,
#'   ci = c(0.65, 0.94),
#'   p_value = 0.008,
#'   effect_measure = "hr",
#'   n = 245L
#' )
#' single_narrative <- generate_evidence_narrative(
#'   endpoint = "PFS",
#'   result = comp_res,
#'   n_patients = 245
#' )
#' }
generate_evidence_narrative <- function(
	endpoint,
	result,
	grade = NULL,
	rob_results = NULL,
	n_patients = NULL,
	template = "iqwig",
	language = c("en", "de"),
	ci_level = 0.95
) {
	language <- match.arg(language)

	# Validate and extract from result
	if (S7::S7_inherits(result, MetaResult)) {
		n_studies <- result@n
		estimate <- result@estimate
		ci <- result@ci
		p_value <- result@p_value
		effect_measure <- result@effect_measure
		heterogeneity <- result@heterogeneity
		is_meta <- TRUE
	} else if (S7::S7_inherits(result, ComparisonResult)) {
		n_studies <- 1L
		estimate <- result@estimate
		ci <- result@ci
		p_value <- result@p_value
		effect_measure <- result@effect_measure
		heterogeneity <- list(I2 = NA_real_, tau2 = NA_real_)
		is_meta <- FALSE

		# Use result@n for n_patients if not specified
		if (is.null(n_patients) && !is.na(result@n)) {
			n_patients <- result@n
		}
	} else {
		ph_abort(
			"result must be a MetaResult or ComparisonResult object"
		)
	}

	# Validate n_patients
	if (is.null(n_patients)) {
		ph_abort("n_patients is required and cannot be missing")
	}

	# Get templates
	templates <- NARRATIVE_TEMPLATES[[template]]
	if (is.null(templates)) {
		ph_abort(sprintf(
			"Template '%s' not found. Available: %s",
			template,
			paste(names(NARRATIVE_TEMPLATES), collapse = ", ")
		))
	}

	# Format numbers
	ci_formatted <- .format_ci(ci, language = language)
	p_formatted <- .format_p_value(p_value)
	ci_level_pct <- round(ci_level * 100)

	# Effect measure label
	effect_measure_label <- .format_effect_measure(effect_measure, language)

	# Determine effect direction
	if (!is.null(grade) && S7::S7_inherits(grade, EvidenceGrade)) {
		direction <- grade@direction
	} else {
		# Infer from estimate
		if (effect_measure %in% c("hr", "or", "rr")) {
			direction <- if (estimate < 1) "benefit" else "harm"
		} else {
			direction <- if (estimate > 0) "benefit" else "harm"
		}
	}

	# Effect direction text
	dir_info <- .get_effect_direction_text(
		estimate,
		effect_measure,
		direction,
		language
	)
	effect_direction <- dir_info$direction
	effect_direction_adj <- dir_info$text

	# Significance check
	is_significant <- !is.na(p_value) && p_value < (1 - ci_level)

	# Build narrative parts
	narrative_parts <- character()

	# 1. Effect narrative
	if (is_meta && n_studies > 1) {
		# Meta-analysis
		if (is_significant) {
			effect_template <- templates$effect_significant
		} else {
			effect_template <- templates$effect_nonsignificant
		}
	} else {
		# Single study
		effect_template <- templates$single_study
	}

	# Format numbers (German uses locale-aware formatting)
	if (language == "de") {
		# German: period as thousands separator, comma as decimal
		n_patients_formatted <- formatC(
			n_patients,
			format = "d",
			big.mark = ".",
			decimal.mark = ","
		)
		estimate_formatted <- gsub("\\.", ",", sprintf("%.2f", estimate))
	} else {
		# English: comma as thousands separator
		n_patients_formatted <- formatC(n_patients, format = "d", big.mark = ",")
		estimate_formatted <- sprintf("%.2f", estimate)
	}

	narrative_parts <- c(
		narrative_parts,
		glue::glue(
			effect_template,
			endpoint = endpoint,
			n_studies = n_studies,
			n_patients = n_patients_formatted,
			effect_measure_label = effect_measure_label,
			estimate = estimate_formatted,
			ci_level = ci_level_pct,
			ci_formatted = ci_formatted,
			p_formatted = p_formatted
		)
	)

	# 2. Heterogeneity (only for meta-analysis with 2+ studies)
	if (is_meta && n_studies >= 2) {
		i2 <- heterogeneity$I2 %||% NA_real_
		het_level <- .assess_heterogeneity_level(i2)

		if (!is.na(i2)) {
			het_template <- templates[[sprintf("heterogeneity_%s", het_level)]]
		} else {
			het_template <- templates$heterogeneity_single
		}

		narrative_parts <- c(
			narrative_parts,
			glue::glue(het_template, i2 = sprintf("%.1f", i2))
		)
	}

	# 3. Risk of bias
	rob_summary <- .summarize_rob(rob_results, n_studies)
	rob_template <- templates[[sprintf("rob_%s", rob_summary$level)]]
	narrative_parts <- c(narrative_parts, rob_template)

	# 4. Evidence grade (if provided)
	if (!is.null(grade) && S7::S7_inherits(grade, EvidenceGrade)) {
		grade_template <- templates[[sprintf("grade_%s", grade@grade)]]
		narrative_parts <- c(
			narrative_parts,
			glue::glue(
				grade_template,
				effect_direction_adj = effect_direction_adj
			)
		)
	}

	# Combine parts with spaces
	paste(narrative_parts, collapse = " ")
}


#' Generate Endpoint-Specific Narrative
#'
#' Creates a formatted paragraph for a single endpoint with all available
#' data including meta-analysis results, risk of bias, and evidence grading.
#'
#' @param endpoint Character. Name of the endpoint.
#' @param result A MetaResult or ComparisonResult object.
#' @param evidence_grade An EvidenceGrade object (optional).
#' @param rob_results List of RoB2Result objects or single RoB2Result
#'   (optional).
#' @param n_patients Integer. Total number of patients.
#' @param template Character. Template name. Default: "iqwig".
#' @param language Character. Output language. Default: "en".
#' @param ci_level Numeric. Confidence level. Default: 0.95.
#'
#' @return Character string with formatted endpoint narrative.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate endpoint narrative
#' meta_res <- MetaResult(
#'   estimate = 0.80,
#'   ci = c(0.70, 0.91),
#'   p_value = 0.001,
#'   n = 5L,
#'   effect_measure = "hr",
#'   heterogeneity = list(I2 = 32, tau2 = 0.015)
#' )
#'
#' grade <- EvidenceGrade(
#'   grade = "proof",
#'   grade_de = "Beleg",
#'   direction = "benefit",
#'   n_studies = 5L
#' )
#'
#' # German IQWiG style
#' narrative <- generate_endpoint_narrative(
#'   endpoint = "Gesamt\u00fcberleben",
#'   result = meta_res,
#'   evidence_grade = grade,
#'   n_patients = 2125,
#'   language = "de"
#' )
#' cat(narrative)
#'
#' # English clinical style
#' narrative_en <- generate_endpoint_narrative(
#'   endpoint = "Overall Survival",
#'   result = meta_res,
#'   evidence_grade = grade,
#'   n_patients = 2125,
#'   template = "clinical"
#' )
#' }
generate_endpoint_narrative <- function(
	endpoint,
	result,
	evidence_grade = NULL,
	rob_results = NULL,
	n_patients = NULL,
	template = "iqwig",
	language = c("en", "de"),
	ci_level = 0.95
) {
	language <- match.arg(language)

	# Call main narrative generator
	narrative <- generate_evidence_narrative(
		endpoint = endpoint,
		result = result,
		grade = evidence_grade,
		rob_results = rob_results,
		n_patients = n_patients,
		template = template,
		language = language,
		ci_level = ci_level
	)

	# Add period if not present
	if (!grepl("\\.$", narrative)) {
		narrative <- paste0(narrative, ".")
	}

	narrative
}


#' Generate Full Evidence Report
#'
#' Creates a comprehensive Word document with evidence narratives for
#' multiple endpoints, including overall results, individual endpoint
#' sections, and conclusions.
#'
#' @param endpoints List or data frame containing endpoint data. Each element
#'   should have: endpoint (name), result (MetaResult/ComparisonResult),
#'   evidence_grade (EvidenceGrade, optional), rob_results (RoB2Result,
#'   optional), n_patients (integer).
#' @param title Character. Report title.
#' @param subtitle Character. Report subtitle (optional).
#' @param author Character. Author name (optional).
#' @param date Character. Report date (optional, defaults to current date).
#' @param template Character. Narrative template. Default: "iqwig".
#' @param language Character. Output language. Default: "en".
#' @param ci_level Numeric. Confidence level. Default: 0.95.
#'
#' @return An officer rdocx object ready for writing to Word.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare endpoint data
#' endpoints <- list(
#'   list(
#'     endpoint = "Overall Survival",
#'     result = meta_os,
#'     evidence_grade = grade_os,
#'     n_patients = 2125
#'   ),
#'   list(
#'     endpoint = "Progression-Free Survival",
#'     result = meta_pfs,
#'     evidence_grade = grade_pfs,
#'     n_patients = 2100
#'   )
#' )
#'
#' # Generate report
#' doc <- generate_full_evidence_report(
#'   endpoints = endpoints,
#'   title = "Clinical Study Report - Efficacy Analysis",
#'   subtitle = "Study ABC-123",
#'   author = "Clinical Development",
#'   language = "en"
#' )
#'
#' # Write to Word file
#' print(doc, target = "efficacy_report.docx")
#'
#' # German IQWiG report
#' doc_de <- generate_full_evidence_report(
#'   endpoints = endpoints_de,
#'   title = "Nutzenbewertung - Wirksamkeitsanalyse",
#'   author = "Medizinische Abteilung",
#'   language = "de"
#' )
#' print(doc_de, target = "nutzenbewertung.docx")
#' }
generate_full_evidence_report <- function(
	endpoints,
	title = "Evidence Report",
	subtitle = NULL,
	author = NULL,
	date = format(Sys.Date(), "%d %B %Y"),
	template = "iqwig",
	language = c("en", "de"),
	ci_level = 0.95
) {
	language <- match.arg(language)

	# Validate endpoints
	if (!is.list(endpoints) || length(endpoints) == 0) {
		ph_abort("endpoints must be a non-empty list")
	}

	# Create Word document
	doc <- officer::read_docx()

	# Add title page
	if (language == "de") {
		doc <- officer::body_add_par(doc, title, style = "heading 1")
		if (!is.null(subtitle)) {
			doc <- officer::body_add_par(doc, subtitle, style = "heading 2")
		}
		if (!is.null(author)) {
			doc <- officer::body_add_par(
				doc,
				paste0("Autor: ", author),
				style = "Normal"
			)
		}
		doc <- officer::body_add_par(doc, paste0("Datum: ", date), style = "Normal")
		doc <- officer::body_add_break(doc)
	} else {
		doc <- officer::body_add_par(doc, title, style = "heading 1")
		if (!is.null(subtitle)) {
			doc <- officer::body_add_par(doc, subtitle, style = "heading 2")
		}
		if (!is.null(author)) {
			doc <- officer::body_add_par(
				doc,
				paste0("Author: ", author),
				style = "Normal"
			)
		}
		doc <- officer::body_add_par(doc, paste0("Date: ", date), style = "Normal")
		doc <- officer::body_add_break(doc)
	}

	# Section: Overall Results Summary
	if (language == "de") {
		doc <- officer::body_add_par(
			doc,
			"Zusammenfassung der Ergebnisse",
			style = "heading 1"
		)
		doc <- officer::body_add_par(doc, "", style = "Normal")

		# Calculate overall statistics
		n_total_studies <- sum(vapply(
			endpoints,
			function(e) {
				if (S7::S7_inherits(e$result, MetaResult)) {
					e$result@n
				} else {
					1L
				}
			},
			integer(1)
		))

		doc <- officer::body_add_par(
			doc,
			sprintf(
				"Diese Analyse umfasst %d Endpunkte aus %d Studien.",
				length(endpoints),
				n_total_studies
			),
			style = "Normal"
		)
	} else {
		doc <- officer::body_add_par(
			doc,
			"Overall Results Summary",
			style = "heading 1"
		)
		doc <- officer::body_add_par(doc, "", style = "Normal")

		# Calculate overall statistics
		n_total_studies <- sum(vapply(
			endpoints,
			function(e) {
				if (S7::S7_inherits(e$result, MetaResult)) {
					e$result@n
				} else {
					1L
				}
			},
			integer(1)
		))

		doc <- officer::body_add_par(
			doc,
			sprintf(
				"This analysis includes %d endpoints from %d studies.",
				length(endpoints),
				n_total_studies
			),
			style = "Normal"
		)
	}

	doc <- officer::body_add_par(doc, "", style = "Normal")

	# Section: Individual Endpoints
	if (language == "de") {
		doc <- officer::body_add_par(doc, "Einzelne Endpunkte", style = "heading 1")
	} else {
		doc <- officer::body_add_par(
			doc,
			"Individual Endpoints",
			style = "heading 1"
		)
	}

	doc <- officer::body_add_par(doc, "", style = "Normal")

	# Process each endpoint
	for (i in seq_along(endpoints)) {
		e <- endpoints[[i]]

		# Validate required fields
		if (is.null(e$endpoint) || is.null(e$result) || is.null(e$n_patients)) {
			ph_abort(sprintf(
				"Endpoint %d is missing required fields (endpoint, result, n_patients)",
				i
			))
		}

		# Generate narrative
		narrative <- generate_endpoint_narrative(
			endpoint = e$endpoint,
			result = e$result,
			evidence_grade = e$evidence_grade %||% NULL,
			rob_results = e$rob_results %||% NULL,
			n_patients = e$n_patients,
			template = template,
			language = language,
			ci_level = ci_level
		)

		# Add endpoint heading
		doc <- officer::body_add_par(doc, e$endpoint, style = "heading 2")
		doc <- officer::body_add_par(doc, narrative, style = "Normal")
		doc <- officer::body_add_par(doc, "", style = "Normal")
	}

	# Section: Conclusions
	if (language == "de") {
		doc <- officer::body_add_par(doc, "Schlussfolgerungen", style = "heading 1")
	} else {
		doc <- officer::body_add_par(doc, "Conclusions", style = "heading 1")
	}
	doc <- officer::body_add_par(doc, "", style = "Normal")

	# Count significant endpoints
	n_significant <- sum(vapply(
		endpoints,
		function(e) {
			!is.na(e$result@p_value) && e$result@p_value < (1 - ci_level)
		},
		logical(1)
	))

	n_proof <- sum(vapply(
		endpoints,
		function(e) {
			if (
				!is.null(e$evidence_grade) &&
					S7::S7_inherits(e$evidence_grade, EvidenceGrade)
			) {
				e$evidence_grade@grade == "proof"
			} else {
				FALSE
			}
		},
		logical(1)
	))

	n_indication <- sum(vapply(
		endpoints,
		function(e) {
			if (
				!is.null(e$evidence_grade) &&
					S7::S7_inherits(e$evidence_grade, EvidenceGrade)
			) {
				e$evidence_grade@grade == "indication"
			} else {
				FALSE
			}
		},
		logical(1)
	))

	n_hints <- sum(vapply(
		endpoints,
		function(e) {
			if (
				!is.null(e$evidence_grade) &&
					S7::S7_inherits(e$evidence_grade, EvidenceGrade)
			) {
				e$evidence_grade@grade == "hint"
			} else {
				FALSE
			}
		},
		logical(1)
	))

	if (language == "de") {
		conclusion_text <- sprintf(
			paste0(
				"Von den %d analysierten Endpunkten zeigten %d einen statistisch ",
				"signifikanten Effekt. Die Evidenzgraduierung ergab %d Beleg(e), ",
				"%d Hinweis(e) und %d Anhaltspunkt(e) f\u00fcr einen Zusatznutzen."
			),
			length(endpoints),
			n_significant,
			n_proof,
			n_indication,
			n_hints
		)
	} else {
		conclusion_text <- sprintf(
			paste0(
				"Of the %d endpoints analyzed, %d demonstrated a statistically ",
				"significant effect. Evidence grading identified %d proof(s), ",
				"%d indication(s), and %d hint(s) of benefit."
			),
			length(endpoints),
			n_significant,
			n_proof,
			n_indication,
			n_hints
		)
	}

	doc <- officer::body_add_par(doc, conclusion_text, style = "Normal")

	doc
}


# =============================================================================
# Utility Functions
# =============================================================================

#' Export Narrative to Text File
#'
#' Writes a narrative (or list of narratives) to a plain text file.
#'
#' @param narrative Character string or list of narratives to write.
#' @param path File path for output.
#' @param append Logical. Append to existing file. Default: FALSE.
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Export single narrative
#' export_narrative(narrative, "endpoint_os.txt")
#'
#' # Export multiple narratives
#' narratives <- list(os = narrative_os, pfs = narrative_pfs)
#' export_narrative(narratives, "all_narratives.txt")
#' }
export_narrative <- function(narrative, path, append = FALSE) {
	if (is.list(narrative)) {
		first_write <- TRUE
		# Write list with names as headers
		for (name in names(narrative)) {
			# Use append parameter only for first write, TRUE thereafter
			use_append <- if (first_write) append else TRUE
			first_write <- FALSE

			if (nzchar(name)) {
				cat(paste0("=== ", name, " ===\n"), file = path, append = use_append)
				use_append <- TRUE # Content always appends after header
			}
			cat(narrative[[name]], file = path, append = use_append)
			cat("\n\n", file = path, append = TRUE)
		}
	} else {
		cat(narrative, file = path, append = append)
	}

	invisible(path)
}


#' Generate Batch Narratives
#'
#' Generates narratives for multiple endpoints at once from a data frame.
#'
#' @param data Data frame with columns: endpoint, estimate, ci_lower, ci_upper,
#'   p_value, n_studies, n_patients, effect_measure, heterogeneity_i2
#'   (optional), grade (optional), direction (optional).
#' @param template Character. Template name. Default: "iqwig".
#' @param language Character. Output language. Default: "en".
#' @param ci_level Numeric. Confidence level. Default: 0.95.
#'
#' @return Character vector of narratives, named by endpoint.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create data frame with results
#' results_df <- data.frame(
#'   endpoint = c("OS", "PFS", "ORR"),
#'   estimate = c(0.75, 0.82, 1.45),
#'   ci_lower = c(0.60, 0.70, 1.20),
#'   ci_upper = c(0.94, 0.96, 1.75),
#'   p_value = c(0.012, 0.025, 0.001),
#'   n_studies = c(3, 3, 4),
#'   n_patients = c(1245, 1200, 1500),
#'   effect_measure = c("hr", "hr", "rr"),
#'   heterogeneity_i2 = c(25, 35, 45),
#'   grade = c("indication", "hint", "proof")
#' )
#'
#' # Generate all narratives
#' narratives <- generate_batch_narratives(results_df)
#' print(narratives)
#' }
generate_batch_narratives <- function(
	data,
	template = "iqwig",
	language = c("en", "de"),
	ci_level = 0.95
) {
	language <- match.arg(language)

	# Validate required columns
	required_cols <- c(
		"endpoint",
		"estimate",
		"ci_lower",
		"ci_upper",
		"p_value",
		"n_studies",
		"n_patients",
		"effect_measure"
	)
	missing <- setdiff(required_cols, names(data))
	if (length(missing) > 0) {
		ph_abort(sprintf(
			"Missing required columns: %s",
			paste(missing, collapse = ", ")
		))
	}

	# Build MetaResult-like structure for each row
	narratives <- lapply(seq_len(nrow(data)), function(i) {
		row <- data[i, ]

		# Use MetaResult when n_studies > 1, otherwise ComparisonResult
		if (!is.null(row$n_studies) && !is.na(row$n_studies) && row$n_studies > 1) {
			result <- MetaResult(
				estimate = row$estimate,
				ci = c(row$ci_lower, row$ci_upper),
				p_value = row$p_value,
				n = as.integer(row$n_studies),
				effect_measure = row$effect_measure,
				heterogeneity = list() # No heterogeneity data from batch
			)
		} else {
			result <- ComparisonResult(
				estimate = row$estimate,
				ci = c(row$ci_lower, row$ci_upper),
				p_value = row$p_value,
				effect_measure = row$effect_measure
			)
		}

		# Create pseudo-grade if present
		grade <- NULL
		if ("grade" %in% names(row) && !is.na(row$grade)) {
			grade <- EvidenceGrade(
				grade = as.character(row$grade),
				grade_de = switch(
					as.character(row$grade),
					proof = "Beleg",
					indication = "Hinweis",
					hint = "Anhaltspunkt",
					none = "Kein Beleg",
					""
				),
				direction = if ("direction" %in% names(row)) {
					as.character(row$direction)
				} else {
					"benefit"
				},
				n_studies = row$n_studies
			)
		}

		generate_endpoint_narrative(
			endpoint = as.character(row$endpoint),
			result = result,
			evidence_grade = grade,
			n_patients = row$n_patients,
			template = template,
			language = language,
			ci_level = ci_level
		)
	})

	names(narratives) <- data$endpoint
	narratives
}
