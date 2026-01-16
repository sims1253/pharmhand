#' IQWiG Evidence Grading Implementation
#'
#' Functions for grading evidence according to IQWiG methods
#' (IQWiG Allgemeine Methoden Version 3.1.4, p.51-56).
#'
#' @name evidence_grading
#' @references
#'   IQWiG Allgemeine Methoden Version 3.1.4, Section 3.1, p.51-56.
#'   \url{https://www.iqwig.de/methoden/allgemeine-methoden-version-3-1.pdf}
#'
NULL

# =============================================================================
# Evidence Grade Constants and Mappings
# =============================================================================

#' @keywords internal
EVIDENCE_GRADES <- c("proof", "indication", "hint", "none")

#' @keywords internal
EVIDENCE_GRADES_DE <- c(
	proof = "Beleg",
	indication = "Hinweis",
	hint = "Anhaltspunkt",
	none = "Kein Beleg"
)

#' @keywords internal
GRADE_CERTAINTY_LEVELS <- c(
	proof = 0.9,
	indication = 0.65,
	hint = 0.4,
	none = 0.2
)

# Domain assessment levels
#' @keywords internal
DOMAIN_LEVELS <- c("low", "some_concerns", "high", "unknown")

# =============================================================================
# Main Grading Function
# =============================================================================

#' Grade Evidence According to IQWiG Criteria
#'
#' Assigns an evidence grade (Beleg/Hinweis/Anhaltspunkt/Kein Beleg) based on
#' study quality, risk of bias, effect size, precision, heterogeneity, and
#' consistency across studies. This implements the IQWiG evidence grading system
#' as described in IQWiG Allgemeine Methoden Version 3.1.4, p.51-56.
#'
#' @param meta_result A MetaResult object from meta_analysis() containing
#'   pooled effect estimate, confidence interval, p-value, and heterogeneity
#'   statistics. Alternatively, a ComparisonResult object for single-study
#'   assessments.
#' @param rob_results A list of RoB2Result objects (one per study), or a single
#'   RoB2Result for single-study assessment. If NULL, defaults to "Low" risk
#'   of bias for all studies.
#' @param n_studies_override Integer. Override the number of studies for single
#'   study assessment. Default: NULL (use meta_result@n or 1 for
#'   ComparisonResult).
#' @param direction Character. Direction of effect: "benefit", "harm",
#'   or "none".
#'   Default: "none" (effect direction determined from effect estimate).
#' @param ci_level Numeric. Confidence level for significance assessment.
#'   Default: 0.95.
#' @param publication_bias List or NULL. Publication bias assessment from
#'   trim_and_fill() or egger's_test(). If provided, used in grading.
#'   Expected list elements: p_value, adjusted_estimates.
#' @param indirectness Numeric or NULL. Indirectness score (0-1, where 1 = no
#'   indirectness). Default: NULL (assumed 1 = no indirectness).
#' @param metadata List. Additional metadata for the EvidenceGrade object.
#'   Default: empty list.
#'
#' @return An EvidenceGrade object with grade, justification, and domain
#'   assessments populated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Meta-analysis with 5 RCTs, all low RoB
#' meta_res <- MetaResult(
#'   estimate = 0.72,
#'   ci = c(0.62, 0.84),
#'   p_value = 0.0001,
#'   n = 5L,
#'   effect_measure = "hr",
#'   heterogeneity = list(I2 = 25, tau2 = 0.01),
#'   method = "REML meta-analysis"
#' )
#'
#' # Create RoB results for all studies (all low)
#' rob_results <- list(
#'   assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study4", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study5", "Low", "Low", "Low", "Low", "Low")
#' )
#'
#' # Grade the evidence
#' grade <- grade_evidence(meta_res, rob_results, direction = "benefit")
#' grade@grade
#' grade@grade_de
#'
#' # Single study assessment
#' comp_res <- ComparisonResult(
#'   estimate = 0.75,
#'   ci = c(0.60, 0.93),
#'   p_value = 0.008,
#'   effect_measure = "hr"
#' )
#' single_grade <- grade_evidence(
#'   comp_res,
#'   rob_results = assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
#'   n_studies_override = 1L
#' )
#' }
grade_evidence <- function(
	meta_result,
	rob_results = NULL,
	n_studies_override = NULL,
	direction = "none",
	ci_level = 0.95,
	publication_bias = NULL,
	indirectness = NULL,
	metadata = list()
) {
	# Determine input type and extract relevant statistics
	if (S7::S7_inherits(meta_result, MetaResult)) {
		n_studies <- meta_result@n
		estimate <- meta_result@estimate
		ci <- meta_result@ci
		p_value <- meta_result@p_value
		heterogeneity <- meta_result@heterogeneity
		effect_measure <- meta_result@effect_measure
	} else if (S7::S7_inherits(meta_result, ComparisonResult)) {
		n_studies <- if (!is.null(n_studies_override)) {
			n_studies_override
		} else {
			1L
		}
		estimate <- meta_result@estimate
		ci <- meta_result@ci
		p_value <- meta_result@p_value
		heterogeneity <- list(I2 = NA_real_, tau2 = NA_real_, Q_pvalue = NA_real_)
		effect_measure <- meta_result@effect_measure
	} else {
		ph_abort(
			"meta_result must be a MetaResult or ComparisonResult object"
		)
	}

	# Determine effect direction if not specified
	if (direction == "none") {
		direction <- .determine_effect_direction(estimate, effect_measure)
	}

	# Assess individual evidence domains
	domains <- assess_evidence_domains(
		estimate = estimate,
		ci = ci,
		p_value = p_value,
		heterogeneity = heterogeneity,
		rob_results = rob_results,
		n_studies = n_studies,
		publication_bias = publication_bias,
		indirectness = indirectness,
		ci_level = ci_level,
		effect_measure = effect_measure
	)

	# Apply grading algorithm
	grade_result <- .apply_grade_algorithm(
		domains = domains,
		n_studies = n_studies,
		direction = direction
	)

	# Determine certainty score
	certainty <- .calculate_certainty(domains, grade_result$grade, n_studies)

	# Generate justification text
	justification <- .generate_grade_justification(
		grade = grade_result$grade,
		domains = domains,
		n_studies = n_studies,
		direction = direction
	)

	# Create and return EvidenceGrade object
	EvidenceGrade(
		grade = grade_result$grade,
		grade_de = unname(EVIDENCE_GRADES_DE[grade_result$grade]),
		direction = direction,
		certainty = certainty,
		n_studies = n_studies,
		domains = domains,
		justification = justification,
		metadata = metadata
	)
}


# =============================================================================
# Domain Assessment Functions
# =============================================================================

#' Assess Individual IQWiG Evidence Domains
#'
#' Evaluates each of the five GRADE/IQWiG evidence domains:
#' study limitations (risk of bias), inconsistency, imprecision,
#' indirectness, and publication bias.
#'
#' @param estimate Numeric. Pooled effect estimate.
#' @param ci Numeric vector of length 2: c(lower, upper) confidence interval.
#' @param p_value Numeric. P-value for the effect estimate.
#' @param heterogeneity List. Heterogeneity statistics from meta-analysis.
#'   Should contain I2, tau2, and optionally Q_pvalue.
#' @param rob_results List of RoB2Result objects or NULL. Risk of bias
#'   assessments for each study.
#' @param n_studies Integer. Number of studies contributing to the evidence.
#' @param publication_bias List or NULL. Publication bias assessment.
#' @param indirectness Numeric or NULL. Indirectness score (0-1).
#' @param ci_level Numeric. Confidence level for imprecision assessment.
#'   Default: 0.95.
#' @param effect_measure Character. Type of effect measure (hr, or, rr, etc.).
#'   Default: "hr".
#'
#' @return A list with five domain assessments, each containing:
#' \describe{
#'   \item{level}{Character: "low", "some_concerns", "high", or "unknown"}
#'   \item{rating}{Numeric rating (0-1, lower is worse)}
#'   \item{notes}{Character notes explaining the assessment}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' domains <- assess_evidence_domains(
#'   estimate = 0.72,
#'   ci = c(0.62, 0.84),
#'   p_value = 0.0001,
#'   heterogeneity = list(I2 = 25, tau2 = 0.01),
#'   n_studies = 5L
#' )
#' domains$limitations
#' domains$inconsistency
#' }
assess_evidence_domains <- function(
	estimate,
	ci,
	p_value,
	heterogeneity,
	rob_results = NULL,
	n_studies,
	publication_bias = NULL,
	indirectness = NULL,
	ci_level = 0.95,
	effect_measure = "hr"
) {
	# Study Limitations (Risk of Bias)
	limitations <- .assess_limitations(rob_results, n_studies)

	# Inconsistency (Heterogeneity)
	inconsistency <- .assess_inconsistency(
		heterogeneity = heterogeneity,
		n_studies = n_studies
	)

	# Imprecision (Confidence Interval Width)
	imprecision <- .assess_imprecision(
		estimate = estimate,
		ci = ci,
		n_studies = n_studies,
		ci_level = ci_level,
		effect_measure = effect_measure
	)

	# Indirectness
	indirectness_assess <- .assess_indirectness(indirectness)

	# Publication Bias
	publication_bias_assess <- .assess_publication_bias(
		publication_bias = publication_bias,
		n_studies = n_studies
	)

	list(
		limitations = limitations,
		inconsistency = inconsistency,
		imprecision = imprecision,
		indirectness = indirectness_assess,
		publication_bias = publication_bias_assess
	)
}


#' @keywords internal
.assess_limitations <- function(rob_results, n_studies) {
	if (is.null(rob_results)) {
		# Default: assume low risk of bias if not provided
		return(list(
			level = "low",
			rating = 1.0,
			notes = "Risk of bias not assessed; defaulting to low"
		))
	}

	# Handle single RoB2Result
	if (S7::S7_inherits(rob_results, RoB2Result)) {
		rob_results <- list(rob_results)
	}

	if (!is.list(rob_results) || length(rob_results) == 0) {
		return(list(
			level = "unknown",
			rating = NA,
			notes = "No risk of bias results provided"
		))
	}

	# Count judgments across all studies
	n_low <- 0
	n_concerns <- 0
	n_high <- 0
	n_studies_with_results <- 0

	for (result in rob_results) {
		if (S7::S7_inherits(result, RoB2Result)) {
			n_studies_with_results <- n_studies_with_results + 1
			if (result@overall == "Low") {
				n_low <- n_low + 1
			} else if (result@overall == "Some concerns") {
				n_concerns <- n_concerns + 1
			} else if (result@overall == "High") {
				n_high <- n_high + 1
			}
		}
	}

	# Handle case where n_studies doesn't match
	if (n_studies_with_results == 0) {
		return(list(
			level = "unknown",
			rating = NA,
			notes = "No valid RoB2Result objects provided"
		))
	}

	# Calculate proportion with each judgment
	prop_low <- n_low / n_studies_with_results
	prop_high <- n_high / n_studies_with_results
	prop_concerns <- n_concerns / n_studies_with_results

	# Determine level based on proportion of high-quality studies
	# Following IQWiG: high proportion of low RoB = low limitations
	if (prop_low >= 0.8) {
		level <- "low"
		rating <- 0.9 + 0.1 * prop_low
		notes <- sprintf(
			"%d/%d studies (%.0f%%) with low risk of bias",
			n_low,
			n_studies_with_results,
			100 * prop_low
		)
	} else if (prop_high >= 0.5) {
		level <- "high"
		rating <- 0.2 + 0.3 * (1 - prop_high)
		notes <- sprintf(
			"%d/%d studies (%.0f%%) with high risk of bias",
			n_high,
			n_studies_with_results,
			100 * prop_high
		)
	} else if (prop_concerns >= 0.5 || prop_high > 0) {
		level <- "some_concerns"
		rating <- 0.5 + 0.3 * (1 - prop_concerns - prop_high)
		notes <- sprintf(
			"%d studies low, %d with concerns, %d high risk of bias",
			n_low,
			n_concerns,
			n_high
		)
	} else {
		# Mixed but mostly low
		level <- "some_concerns"
		rating <- 0.6 + 0.3 * prop_low
		notes <- sprintf(
			"Mixed risk of bias: %d low, %d concerns, %d high",
			n_low,
			n_concerns,
			n_high
		)
	}

	list(level = level, rating = min(1, rating), notes = notes)
}


#' @keywords internal
.assess_inconsistency <- function(
	heterogeneity,
	n_studies
) {
	I2 <- heterogeneity$I2 %||% NA_real_
	tau2 <- heterogeneity$tau2 %||% NA_real_
	Q_pvalue <- heterogeneity$Q_pvalue %||% NA_real_

	# For single studies, inconsistency is not applicable
	if (n_studies < 2) {
		return(list(
			level = "unknown",
			rating = NA,
			notes = "Inconsistency not applicable for single study"
		))
	}

	# Assess based on I2 (Higgins et al. interpretation)
	# I2 0-40%: might not be important
	# I2 30-60%: may represent moderate heterogeneity
	# I2 50-90%: may represent substantial heterogeneity
	# I2 75-100%: considerable heterogeneity

	if (is.na(I2) || I2 < 0) {
		return(list(
			level = "unknown",
			rating = NA,
			notes = "Heterogeneity (I2) not calculated"
		))
	}

	if (I2 < 30) {
		level <- "low"
		rating <- 0.9 + 0.1 * (1 - I2 / 30)
		notes <- sprintf("Low heterogeneity (I2 = %.1f%%)", I2)
	} else if (I2 < 50) {
		level <- "some_concerns"
		rating <- 0.7 + 0.2 * (1 - (I2 - 30) / 20)
		notes <- sprintf("Moderate heterogeneity (I2 = %.1f%%)", I2)
	} else if (I2 < 75) {
		level <- "some_concerns"
		rating <- 0.5 + 0.2 * (1 - (I2 - 50) / 25)
		notes <- sprintf("Substantial heterogeneity (I2 = %.1f%%)", I2)
	} else {
		level <- "high"
		rating <- 0.2 + 0.3 * (1 - min(1, (I2 - 75) / 25))
		notes <- sprintf("Considerable heterogeneity (I2 = %.1f%%)", I2)
	}

	list(level = level, rating = rating, notes = notes)
}


#' @keywords internal
.assess_imprecision <- function(
	estimate,
	ci,
	n_studies,
	ci_level,
	effect_measure
) {
	# Check for valid CI
	if (length(ci) != 2 || anyNA(ci)) {
		return(list(
			level = "unknown",
			rating = NA,
			notes = "Confidence interval not available"
		))
	}

	# Calculate CI width relative to point estimate
	# For ratio measures, use log scale
	if (effect_measure %in% c("hr", "or", "rr")) {
		log_est <- log(estimate)
		log_ci_low <- log(ci[1])
		log_ci_high <- log(ci[2])
		ci_width <- log_ci_high - log_ci_low
		se <- ci_width / (2 * stats::qnorm(1 - (1 - ci_level) / 2))

		# Calculate CI ratio for imprecision assessment
		ci_ratio <- ci[2] / ci[1]
	} else {
		# For difference measures
		ci_width <- ci[2] - ci[1]
		ci_ratio <- if (estimate != 0) ci_width / abs(estimate) else NA
		se <- ci_width / (2 * stats::qnorm(1 - (1 - ci_level) / 2))
	}

	# Assess precision based on CI width and whether CI includes null
	if (effect_measure %in% c("hr", "or", "rr")) {
		ci_includes_null <- (ci[1] < 1 && ci[2] > 1)
	} else {
		ci_includes_null <- (ci[1] < 0 && ci[2] > 0)
	}

	if (ci_includes_null) {
		# CI includes null - imprecise
		if (n_studies >= 3) {
			level <- "high"
			rating <- 0.2
			notes <- sprintf(
				"CI includes null value (%.3f to %.3f)",
				ci[1],
				ci[2]
			)
		} else {
			# Single study or few studies - can't downgrade for imprecision alone
			level <- "some_concerns"
			rating <- 0.4
			notes <- sprintf(
				"CI includes null with limited studies (%.3f to %.3f)",
				ci[1],
				ci[2]
			)
		}
	} else {
		# CI does not include null - assess width
		# Wide CI if ratio > 2 or < 0.5 for ratio measures
		# For difference measures, wide if CI width > effect size

		is_wide <- FALSE
		if (effect_measure %in% c("hr", "or", "rr")) {
			is_wide <- ci_ratio > 2.5 || ci_ratio < 0.4
		} else {
			is_wide <- abs(ci_width) > 2 * abs(estimate)
		}

		if (!is_wide) {
			level <- "low"
			rating <- 0.95
			notes <- sprintf("Precise estimate (CI: %.3f to %.3f)", ci[1], ci[2])
		} else {
			level <- "some_concerns"
			rating <- 0.6
			notes <- sprintf(
				"Imprecise estimate - wide CI (%.3f to %.3f, ratio: %.2f)",
				ci[1],
				ci[2],
				ci_ratio
			)
		}
	}

	list(
		level = level,
		rating = rating,
		notes = notes,
		ci_includes_null = ci_includes_null
	)
}


#' @keywords internal
.assess_indirectness <- function(indirectness) {
	if (is.null(indirectness)) {
		# Default: assume no indirectness
		return(list(
			level = "low",
			rating = 1.0,
			notes = "Indirectness not assessed; assumed low"
		))
	}

	# indirectness should be 0-1 (1 = no indirectness)
	if (indirectness >= 0.9) {
		level <- "low"
		rating <- indirectness
		notes <- sprintf("Low indirectness (score: %.2f)", indirectness)
	} else if (indirectness >= 0.7) {
		level <- "some_concerns"
		rating <- indirectness
		notes <- sprintf(
			"Some concerns about indirectness (score: %.2f)",
			indirectness
		)
	} else {
		level <- "high"
		rating <- indirectness
		notes <- sprintf("High indirectness (score: %.2f)", indirectness)
	}

	list(level = level, rating = rating, notes = notes)
}


#' @keywords internal
.assess_publication_bias <- function(publication_bias, n_studies) {
	if (is.null(publication_bias)) {
		return(list(
			level = "unknown",
			rating = NA,
			notes = "Publication bias not assessed"
		))
	}

	if (n_studies < 5) {
		return(list(
			level = "unknown",
			rating = NA,
			notes = sprintf(
				"Publication bias not reliably assessable with only %d studies",
				n_studies
			)
		))
	}

	# Check for publication bias indicators
	p_value <- publication_bias$p_value %||% NA_real_
	adjusted_est <- publication_bias$adjusted_estimates %||% NULL

	if (is.na(p_value) || p_value >= 0.05) {
		level <- "low"
		rating <- 0.9
		notes <- "No evidence of publication bias (p > 0.05)"
	} else if (p_value >= 0.01) {
		level <- "some_concerns"
		rating <- 0.6
		notes <- sprintf(
			"Some evidence of publication bias (Egger's test p = %.3f)",
			p_value
		)
	} else {
		level <- "high"
		rating <- 0.3
		notes <- sprintf(
			"Strong evidence of publication bias (Egger's test p = %.3f)",
			p_value
		)

		# Add info about adjusted estimate if available
		if (!is.null(adjusted_est)) {
			notes <- paste(notes, "; adjusted estimate:", round(adjusted_est, 2))
		}
	}

	list(level = level, rating = rating, notes = notes)
}


# =============================================================================
# Grading Algorithm
# =============================================================================

#' @keywords internal
.apply_grade_algorithm <- function(domains, n_studies, direction) {
	# Extract domain levels and ratings
	lim_level <- domains$limitations$level
	inc_level <- domains$inconsistency$level
	imp_level <- domains$imprecision$level
	ind_level <- domains$indirectness$level
	pb_level <- domains$publication_bias$level

	# Get ratings (NA treated as 1 = no concern)
	lim_rating <- domains$limitations$rating %||% 1
	inc_rating <- domains$inconsistency$rating %||% 1
	imp_rating <- domains$imprecision$rating %||% 1
	ind_rating <- domains$indirectness$rating %||% 1
	pb_rating <- domains$publication_bias$rating %||% 1

	# Calculate overall concern score
	# Each domain contributes to downgrade
	concern_score <- mean(
		c(
			if (lim_level == "high") {
				2
			} else if (lim_level == "some_concerns") {
				1
			} else {
				0
			},
			if (inc_level == "high") {
				2
			} else if (inc_level == "some_concerns") {
				1
			} else {
				0
			},
			if (imp_level == "high") {
				2
			} else if (imp_level == "some_concerns") {
				1
			} else {
				0
			},
			if (ind_level == "high") {
				2
			} else if (ind_level == "some_concerns") {
				1
			} else {
				0
			},
			if (pb_level == "high") {
				2
			} else if (pb_level == "some_concerns") {
				1
			} else {
				0
			}
		),
		na.rm = TRUE
	)

	# Number of studies factor (more studies = stronger evidence)
	study_factor <- min(1, (n_studies - 1) / 4) # 0 for 1 study, 1 for 5+ studies

	# Determine grade based on concern score and study factor
	# Proof (Beleg): Low concerns across all domains, multiple studies
	# Indication (Hinweis): Some concerns but no major issues
	# Hint (Anhaltspunkt): Significant concerns
	# None (Kein Beleg): Major issues or no effect

	# Special case: if CI includes null, downgrade at least one level
	ci_includes_null <- domains$imprecision$ci_includes_null

	if (concern_score == 0 && !ci_includes_null && n_studies >= 4) {
		# No concerns at all, sufficient studies -> Proof
		grade <- "proof"
	} else if (concern_score <= 1.5 && !ci_includes_null) {
		# Some concerns -> Indication
		grade <- "indication"
	} else if (concern_score <= 2.5 || (ci_includes_null && n_studies >= 2)) {
		# More concerns or borderline significance -> Hint
		grade <- "hint"
	} else {
		# Major concerns or no evidence -> No proof
		grade <- "none"
	}

	# Additional check: if direction is "none" (no effect) AND CI includes null,
	# downgrade to none
	if (direction == "none" && ci_includes_null && grade != "none") {
		grade <- "none"
	}

	list(
		grade = grade,
		concern_score = concern_score,
		study_factor = study_factor
	)
}


#' @keywords internal
.calculate_certainty <- function(domains, grade, n_studies) {
	# Get domain ratings (NA treated as 1)
	ratings <- c(
		domains$limitations$rating %||% 1,
		domains$inconsistency$rating %||% 1,
		domains$imprecision$rating %||% 1,
		domains$indirectness$rating %||% 1,
		domains$publication_bias$rating %||% 1
	)

	# Calculate geometric mean of ratings
	certainty_prod <- prod(ratings, na.rm = TRUE)
	n_valid <- sum(!is.na(ratings))
	certainty <- if (n_valid > 0) certainty_prod^(1 / n_valid) else 0.5

	# Adjust for number of studies
	study_bonus <- min(0.15, 0.03 * (n_studies - 1))
	certainty <- min(1, certainty + study_bonus)

	# Adjust based on grade
	grade_certainty <- GRADE_CERTAINTY_LEVELS[grade]
	certainty <- 0.7 * certainty + 0.3 * grade_certainty

	round(certainty, 2)
}


#' @keywords internal
.generate_grade_justification <- function(
	grade,
	domains,
	n_studies,
	direction
) {
	# Build justification text
	just_parts <- character()

	# Start with grade
	just_parts <- c(
		just_parts,
		sprintf(
			"Evidence grade: %s (%s)",
			EVIDENCE_GRADES_DE[grade],
			toupper(grade)
		)
	)

	# Add number of studies
	if (n_studies == 1) {
		just_parts <- c(just_parts, "Based on single study")
	} else {
		just_parts <- c(just_parts, sprintf("Based on %d studies", n_studies))
	}

	# Add domain-specific notes
	domain_notes <- c(
		limitations = domains$limitations$notes,
		inconsistency = domains$inconsistency$notes,
		imprecision = domains$imprecision$notes,
		indirectness = domains$indirectness$notes,
		publication_bias = domains$publication_bias$notes
	)

	# Only include notes that are informative (not default "unknown" messages)
	for (d in names(domain_notes)) {
		note <- domain_notes[d]
		if (
			nzchar(note) &&
				!grepl("not assessed|not applicable", note, ignore.case = TRUE)
		) {
			just_parts <- c(just_parts, sprintf("%s: %s", d, note))
		}
	}

	# Add effect direction
	if (direction != "none") {
		just_parts <- c(just_parts, sprintf("Effect direction: %s", direction))
	}

	paste(just_parts, collapse = "; ")
}


#' @keywords internal
.determine_effect_direction <- function(estimate, effect_measure) {
	if (effect_measure %in% c("hr", "or", "rr")) {
		# For ratio measures, < 1 means benefit (for harm outcomes) or harm
		# Need clinical context - default to "none" for now
		if (estimate < 0.95) {
			"benefit" # Assuming beneficial effect
		} else if (estimate > 1.05) {
			"harm" # Assuming harmful effect
		} else {
			"none"
		}
	} else {
		# For difference measures
		if (estimate > 0) {
			"benefit"
		} else if (estimate < 0) {
			"harm"
		} else {
			"none"
		}
	}
}


# =============================================================================
# Formatting Functions
# =============================================================================

#' Format Evidence Grade for Display
#'
#' Formats an EvidenceGrade object for display in reports, converting
#' between German and English terminology as requested.
#'
#' @param grade An EvidenceGrade object.
#' @param language Character. Output language: "en" for English,
#'   "de" for German.
#'   Default: "en".
#' @param include_details Logical. Include detailed justification.
#'   Default: FALSE.
#' @param format Character. Output format: "text", "html", or "latex".
#'   Default: "text".
#'
#' @return A character string with the formatted grade.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' grade <- EvidenceGrade(
#'   grade = "indication",
#'   grade_de = "Hinweis",
#'   direction = "benefit",
#'   n_studies = 5L,
#'   justification = "Consistent results from 5 studies"
#' )
#'
#' # English output
#' format_evidence_grade(grade, language = "en")
#'
#' # German output with details
#' format_evidence_grade(grade, language = "de", include_details = TRUE)
#'
#' # HTML format
#' format_evidence_grade(grade, format = "html")
#' }
format_evidence_grade <- function(
	grade,
	language = c("en", "de"),
	include_details = FALSE,
	format = c("text", "html", "latex")
) {
	language <- match.arg(language)
	format <- match.arg(format)

	if (!S7::S7_inherits(grade, EvidenceGrade)) {
		ph_abort("grade must be an EvidenceGrade object")
	}

	# Get grade name
	if (language == "de") {
		grade_name <- grade@grade_de
	} else {
		grade_name <- switch(
			grade@grade,
			"proof" = "Proof",
			"indication" = "Indication",
			"hint" = "Hint",
			"none" = "No proof",
			grade@grade
		)
	}

	# Build output
	if (!include_details) {
		out <- grade_name
	} else {
		# Include details
		details <- c(
			sprintf("Grade: %s", grade_name),
			sprintf("Certainty: %.0f%%", grade@certainty * 100),
			sprintf("Studies: %d", grade@n_studies),
			sprintf("Direction: %s", grade@direction),
			if (nzchar(grade@justification)) {
				gsub("^Evidence grade: [^;]+; ", "", grade@justification)
			} else {
				NULL
			}
		)

		if (format == "text") {
			out <- paste(details, collapse = "\n")
		} else if (format == "html") {
			# Each detail in its own <p class='grade'> tag
			# Escape content before wrapping in tags
			escaped_details <- details
			escaped_details <- gsub("&", "&amp;", escaped_details, fixed = TRUE)
			escaped_details <- gsub("<", "&lt;", escaped_details, fixed = TRUE)
			escaped_details <- gsub(">", "&gt;", escaped_details, fixed = TRUE)
			out <- paste0(
				"<div class='evidence-grade'>\n",
				paste0("  <p class='grade'>", escaped_details, "</p>\n", collapse = ""),
				"</div>"
			)
		} else if (format == "latex") {
			out <- paste0(
				"\\begin{description}\n",
				paste0(
					"  \\item[",
					gsub(":.*", ":", details),
					"] ",
					gsub("^[^:]+: ", "", details),
					"\n",
					collapse = ""
				),
				"\\end{description}"
			)
		}
	}

	out
}


# =============================================================================
# Summary and Print Methods
# =============================================================================

#' Print Method for EvidenceGrade
#'
#' @param x An EvidenceGrade object.
#' @param ... Additional arguments passed to print.
#' @keywords internal
print.EvidenceGrade <- function(x, ...) {
	cat("IQWiG Evidence Grade\n")
	cat("====================\n")
	cat(sprintf("Grade: %s (%s)\n", x@grade_de, toupper(x@grade)))
	cat(sprintf("Certainty: %.0f%%\n", x@certainty * 100))
	cat(sprintf("Studies: %d\n", x@n_studies))
	cat(sprintf("Direction: %s\n", x@direction))

	if (nzchar(x@justification)) {
		cat("\nJustification:\n")
		cat(strwrap(x@justification, indent = 2), sep = "\n")
	}

	if (length(x@domains) > 0) {
		cat("\nDomain assessments:\n")
		for (d in names(x@domains)) {
			domain <- x@domains[[d]]
			if (is.na(domain$rating)) {
				cat(sprintf("  %s: %s (not assessed)\n", d, domain$level))
			} else {
				cat(sprintf("  %s: %s (rating=%.2f)\n", d, domain$level, domain$rating))
			}
		}
	}

	invisible(x)
}

# Create an external generic for print to work with S7 classes
# This ensures S3 print method is called for EvidenceGrade objects
print_external <- S7::new_external_generic("base", "print", "x")
S7::method(print_external, EvidenceGrade) <- function(x, ...) {
	print.EvidenceGrade(x, ...)
	invisible(x)
}


# =============================================================================
# Helper: Get Evidence Summary Table
# =============================================================================

#' Create Evidence Summary Table
#'
#' Generates a summary table of evidence grades for multiple outcomes
#' or endpoints.
#'
#' @param grades List of EvidenceGrade objects, or a data frame with
#'   grade information.
#' @param outcomes Character vector of outcome names (required if grades
#'   is a list without names).
#' @param language Output language for grade names: "en" or "de".
#'   Default: "en".
#'
#' @return A data frame with outcome, grade, certainty, and study count.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' grades <- list(
#'   grade_evidence(meta1, rob1, outcome = "OS"),
#'   grade_evidence(meta2, rob2, outcome = "PFS")
#' )
#' summary_table <- evidence_summary_table(grades)
#' }
evidence_summary_table <- function(
	grades,
	outcomes = NULL,
	language = c("en", "de")
) {
	language <- match.arg(language)

	if (is.data.frame(grades)) {
		# Already a data frame
		return(grades)
	}

	if (!is.list(grades)) {
		ph_abort("grades must be a list of EvidenceGrade objects or a data frame")
	}

	# Process list
	if (is.null(names(grades)) && is.null(outcomes)) {
		ph_abort("Either name the grades list or provide outcomes vector")
	}

	out_names <- if (!is.null(outcomes)) {
		outcomes
	} else {
		names(grades)
	}

	# Extract data from each grade
	grade_data <- lapply(seq_along(grades), function(i) {
		g <- grades[[i]]
		if (S7::S7_inherits(g, EvidenceGrade)) {
			# Select grade based on language parameter
			grade_display <- if (language == "de") g@grade_de else g@grade
			list(
				outcome = out_names[i],
				grade = grade_display,
				grade_en = g@grade,
				grade_de = g@grade_de,
				certainty = g@certainty,
				n_studies = g@n_studies,
				direction = g@direction
			)
		} else {
			# Select grade based on language parameter
			grade_val <- g$grade %||% NA_character_
			grade_de_val <- g$grade_de %||% NA_character_
			grade_display <- if (language == "de") grade_de_val else grade_val
			list(
				outcome = out_names[i],
				grade = grade_display,
				grade_en = grade_val,
				grade_de = grade_de_val,
				certainty = g$certainty %||% NA_real_,
				n_studies = g$n_studies %||% NA_integer_,
				direction = g$direction %||% NA_character_
			)
		}
	})

	# Convert to data frame
	result <- do.call(rbind.data.frame, grade_data)
	rownames(result) <- NULL

	result
}
