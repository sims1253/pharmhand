#' ROBINS-I (Risk Of Bias In Non-randomized Studies - of Interventions)
#'
#' Functions for assessing risk of bias in non-randomized studies
#' using the ROBINS-I tool.
#'
#' @name bias_robins_i
#' @references
#'   Sterne JAC, et al. (2016). ROBINS-I: a tool for assessing risk of bias
#'   in non-randomized studies of interventions. BMJ, 354, i4919.
#'
#'   IQWiG Allgemeine Methoden Version 10.1, Section 10.1.4.
#'
#'   Cochrane Handbook for Systematic Reviews of Interventions, Chapter 25.
NULL

# ROBINS-I domain names constant
#' @keywords internal
ROBINSI_DOMAINS <- c(
	"D1_confounding",
	"D2_selection",
	"D3_classification",
	"D4_deviations",
	"D5_missing_data",
	"D6_measurement",
	"D7_selection_report"
)

# Valid ROBINS-I judgments
#' @keywords internal
ROBINSI_JUDGMENTS <- c(
	"Low",
	"Moderate",
	"Serious",
	"Critical",
	"No information"
)


#' ROBINSIResult S7 Class
#'
#' S7 class for storing ROBINS-I assessment results for a single study.
#'
#' @param study_id Character. Study identifier.
#' @param domains List. Named list of domain judgments with supporting text.
#' @param overall Character. Overall risk of bias judgment.
#' @param overall_justification Character. Justification for overall judgment.
#' @param outcome Character. Outcome being assessed (optional).
#' @param intervention Character. Intervention being assessed (optional).
#' @param comparator Character. Comparator intervention (optional).
#' @param metadata List. Additional metadata (optional).
#'
#' @return A ROBINSIResult object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- ROBINSIResult(
#'   study_id = "OBS001",
#'   domains = list(
#'     D1_confounding = list(
#'       judgment = "Serious", support = "No adjustment for confounders"
#'     ),
#'     D2_selection = list(judgment = "Low", support = "Appropriate selection"),
#'     D3_classification = list(
#'       judgment = "Low", support = "Intervention clearly defined"
#'     ),
#'     D4_deviations = list(
#'       judgment = "Moderate", support = "Some deviations noted"
#'     ),
#'     D5_missing_data = list(judgment = "Low", support = "No missing data"),
#'     D6_measurement = list(
#'       judgment = "Moderate", support = "Blinded outcome assessment"
#'     ),
#'     D7_selection_report = list(
#'       judgment = "Low", support = "Pre-specified outcomes reported"
#'     )
#'   ),
#'   overall = "Serious",
#'   overall_justification = "D1 rated Serious due to unadjusted confounding",
#'   outcome = "Mortality",
#'   intervention = "Drug A",
#'   comparator = "Standard of Care"
#' )
#' }
ROBINSIResult <- S7::new_class(
	"ROBINSIResult",
	package = "pharmhand",
	properties = list(
		study_id = S7::new_property(
			S7::class_character,
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		domains = S7::new_property(
			S7::class_list,
			validator = function(value) {
				# Validate that all required domains are present
				if (!all(ROBINSI_DOMAINS %in% names(value))) {
					missing <- setdiff(ROBINSI_DOMAINS, names(value))
					return(sprintf(
						"domains must contain all required domains: %s",
						paste(missing, collapse = ", ")
					))
				}
				# Validate each domain has judgment
				for (domain in ROBINSI_DOMAINS) {
					if (!"judgment" %in% names(value[[domain]])) {
						return(sprintf("domain %s must have a 'judgment' element", domain))
					}
					judgment <- value[[domain]]$judgment
					if (!judgment %in% ROBINSI_JUDGMENTS) {
						return(sprintf(
							"domain %s judgment must be one of: %s",
							domain,
							paste(ROBINSI_JUDGMENTS, collapse = ", ")
						))
					}
				}
				NULL
			}
		),
		overall = S7::new_property(
			S7::class_character,
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				if (!value %in% ROBINSI_JUDGMENTS) {
					return(sprintf(
						"overall must be one of: %s",
						paste(ROBINSI_JUDGMENTS, collapse = ", ")
					))
				}
				NULL
			}
		),
		overall_justification = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		outcome = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		intervention = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		comparator = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		metadata = S7::new_property(S7::class_list, default = list()),
		# Computed property: summary as data frame
		summary_df = S7::new_property(
			class = S7::class_data.frame,
			getter = function(self) {
				df <- data.frame(
					domain = ROBINSI_DOMAINS,
					domain_label = c(
						"D1: Confounding",
						"D2: Selection of participants",
						"D3: Classification of interventions",
						"D4: Deviations from intended interventions",
						"D5: Missing data",
						"D6: Measurement of outcomes",
						"D7: Selection of reported result"
					),
					judgment = vapply(
						ROBINSI_DOMAINS,
						function(d) self@domains[[d]]$judgment,
						character(1)
					),
					support = vapply(
						ROBINSI_DOMAINS,
						function(d) self@domains[[d]]$support %||% "",
						character(1)
					),
					stringsAsFactors = FALSE
				)
				df
			}
		),
		# Computed property: count of judgments
		judgment_counts = S7::new_property(
			class = S7::class_list,
			getter = function(self) {
				judgments <- vapply(
					ROBINSI_DOMAINS,
					function(d) self@domains[[d]]$judgment,
					character(1)
				)
				list(
					n_low = sum(judgments == "Low"),
					n_moderate = sum(judgments == "Moderate"),
					n_serious = sum(judgments == "Serious"),
					n_critical = sum(judgments == "Critical"),
					n_no_info = sum(judgments == "No information")
				)
			}
		),
		# Computed property: worst domain
		worst_domain = S7::new_property(
			class = S7::class_character,
			getter = function(self) {
				# Order of severity: Critical > Serious > Moderate > No information > Low
				severity_order <- c(
					"Critical",
					"Serious",
					"Moderate",
					"No information",
					"Low"
				)
				judgments <- vapply(
					ROBINSI_DOMAINS,
					function(d) self@domains[[d]]$judgment,
					character(1)
				)
				# Find the first (most severe) judgment in the severity order
				for (severity in severity_order) {
					if (severity %in% judgments) {
						domain <- ROBINSI_DOMAINS[judgments == severity][1]
						return(domain)
					}
				}
				ROBINSI_DOMAINS[1]
			}
		)
	)
)


#' Assess Risk of Bias using ROBINS-I
#'
#' Assesses the risk of bias for a single non-randomized study using the
#' ROBINS-I tool. The overall judgment is automatically calculated based on
#' domain judgments using the following algorithm:
#' \itemize{
#'   \item "Low": All domains rated "Low"
#'   \item "Moderate": At least one "Moderate" but no "Serious" or "Critical"
#'   \item "Serious": At least one "Serious" but no "Critical"
#'   \item "Critical": Any domain rated "Critical"
#'   \item "No information": At least one "No information" with all others
#'     "Low" or "Moderate"
#' }
#'
#' @param study_id Character. Study identifier.
#' @param d1_confounding Character. Domain 1 judgment: "Low", "Moderate",
#'   "Serious", "Critical", or "No information". For confounding.
#' @param d2_selection Character. Domain 2 judgment: "Low", "Moderate",
#'   "Serious", "Critical", or "No information". For selection of participants.
#' @param d3_classification Character. Domain 3 judgment: "Low", "Moderate",
#'   "Serious", "Critical", or "No information". For classification of
#'   interventions.
#' @param d4_deviations Character. Domain 4 judgment: "Low", "Moderate",
#'   "Serious", "Critical", or "No information". For deviations from
#'   interventions.
#' @param d5_missing_data Character. Domain 5 judgment: "Low", "Moderate",
#'   "Serious", "Critical", or "No information". For missing data.
#' @param d6_measurement Character. Domain 6 judgment: "Low", "Moderate",
#'   "Serious", "Critical", or "No information". For measurement of outcomes.
#' @param d7_selection_report Character. Domain 7 judgment: "Low", "Moderate",
#'   "Serious", "Critical", or "No information". For selection of reported
#'   result.
#' @param d1_support Character. Supporting text for Domain 1 (optional).
#' @param d2_support Character. Supporting text for Domain 2 (optional).
#' @param d3_support Character. Supporting text for Domain 3 (optional).
#' @param d4_support Character. Supporting text for Domain 4 (optional).
#' @param d5_support Character. Supporting text for Domain 5 (optional).
#' @param d6_support Character. Supporting text for Domain 6 (optional).
#' @param d7_support Character. Supporting text for Domain 7 (optional).
#' @param outcome Character. Outcome being assessed (optional).
#' @param intervention Character. Intervention being assessed (optional).
#' @param comparator Character. Comparator intervention (optional).
#' @param overall Character. Overall judgment. If NULL, auto-calculated.
#' @param overall_justification Character. Justification for overall judgment.
#'   If NULL and overall is auto-calculated, a default justification is
#'   generated.
#' @param metadata List. Additional metadata (optional).
#'
#' @return A ROBINSIResult object with the assessment results.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic assessment with auto-calculated overall
#' result <- assess_robins_i(
#'   study_id = "OBS001",
#'   d1_confounding = "Serious",
#'   d2_selection = "Low",
#'   d3_classification = "Low",
#'   d4_deviations = "Low",
#'   d5_missing_data = "Low",
#'   d6_measurement = "Moderate",
#'   d7_selection_report = "Low",
#'   d6_support = "Outcome assessor not fully blinded",
#'   outcome = "Overall Survival",
#'   intervention = "Drug A",
#'   comparator = "Standard of Care"
#' )
#' result@overall
#' result@judgment_counts
#'
#' # Assessment with all Low risk
#' result2 <- assess_robins_i(
#'   study_id = "OBS002",
#'   d1_confounding = "Low",
#'   d2_selection = "Low",
#'   d3_classification = "Low",
#'   d4_deviations = "Low",
#'   d5_missing_data = "Low",
#'   d6_measurement = "Low",
#'   d7_selection_report = "Low",
#'   outcome = "Response Rate"
#' )
#' }
assess_robins_i <- function(
	study_id,
	d1_confounding,
	d2_selection,
	d3_classification,
	d4_deviations,
	d5_missing_data,
	d6_measurement,
	d7_selection_report,
	d1_support = "",
	d2_support = "",
	d3_support = "",
	d4_support = "",
	d5_support = "",
	d6_support = "",
	d7_support = "",
	outcome = "",
	intervention = "",
	comparator = "",
	overall = NULL,
	overall_justification = NULL,
	metadata = list()
) {
	# Validate study_id
	if (missing(study_id) || is.null(study_id)) {
		ph_abort("study_id is required and cannot be missing or NULL")
	}
	admiraldev::assert_character_scalar(study_id)

	# Use lists to preserve NULL values (c() drops NULLs)
	judgments <- list(
		d1 = d1_confounding,
		d2 = d2_selection,
		d3 = d3_classification,
		d4 = d4_deviations,
		d5 = d5_missing_data,
		d6 = d6_measurement,
		d7 = d7_selection_report
	)

	# Collect supporting text
	supports <- list(
		d1 = d1_support,
		d2 = d2_support,
		d3 = d3_support,
		d4 = d4_support,
		d5 = d5_support,
		d6 = d6_support,
		d7 = d7_support
	)

	# Validate all judgments
	for (i in seq_along(ROBINSI_DOMAINS)) {
		domain_name <- ROBINSI_DOMAINS[i]
		judgment <- judgments[[i]]
		if (is.null(judgment) || is.na(judgment) || identical(judgment, "")) {
			ph_abort(sprintf("%s judgment is required", domain_name))
		}
		if (!judgment %in% ROBINSI_JUDGMENTS) {
			ph_abort(sprintf(
				"%s must be one of: %s",
				domain_name,
				paste(ROBINSI_JUDGMENTS, collapse = ", ")
			))
		}
	}

	# Build domains list
	domains <- list()
	for (i in seq_along(ROBINSI_DOMAINS)) {
		support_val <- supports[[i]]
		if (is.null(support_val) || is.na(support_val)) {
			support_val <- ""
		}
		domains[[ROBINSI_DOMAINS[i]]] <- list(
			judgment = judgments[[i]],
			support = support_val
		)
	}

	# Auto-calculate overall if not provided
	if (is.null(overall)) {
		overall <- .calculate_robins_i_overall(judgments)
	}

	# Generate justification if not provided
	if (is.null(overall_justification) || overall_justification == "") {
		overall_justification <- .generate_robins_i_justification(
			judgments,
			overall
		)
	}

	# Validate overall judgment
	if (!overall %in% ROBINSI_JUDGMENTS) {
		ph_abort(sprintf(
			"overall must be one of: %s",
			paste(ROBINSI_JUDGMENTS, collapse = ", ")
		))
	}

	# Create and return ROBINSIResult
	ROBINSIResult(
		study_id = study_id,
		domains = domains,
		overall = overall,
		overall_justification = overall_justification,
		outcome = outcome,
		intervention = intervention,
		comparator = comparator,
		metadata = metadata
	)
}


#' Assess Multiple Studies with ROBINS-I
#'
#' Performs ROBINS-I risk of bias assessment for multiple non-randomized studies
#' from a data frame.
#'
#' @param data Data frame with columns for study_id, domain judgments, and
#'   optionally supporting text. Required columns:
#'   \itemize{
#'     \item study_id: Study identifier
#'     \item d1_confounding: Domain 1 judgment
#'     \item d2_selection: Domain 2 judgment
#'     \item d3_classification: Domain 3 judgment
#'     \item d4_deviations: Domain 4 judgment
#'     \item d5_missing_data: Domain 5 judgment
#'     \item d6_measurement: Domain 6 judgment
#'     \item d7_selection_report: Domain 7 judgment
#'   }
#'   Optional columns: d1_support, d2_support, d3_support, d4_support,
#'   d5_support, d6_support, d7_support, outcome, intervention, comparator,
#'   overall, overall_justification
#' @param .suppress_messages Logical. If TRUE, suppresses individual assessment
#'   messages. Default: FALSE
#'
#' @return A list of ROBINSIResult objects, named by study_id.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create data frame with study assessments
#' robins_data <- data.frame(
#'   study_id = c("OBS001", "OBS002", "OBS003"),
#'   d1_confounding = c("Serious", "Moderate", "Low"),
#'   d2_selection = c("Low", "Low", "Low"),
#'   d3_classification = c("Low", "Low", "Low"),
#'   d4_deviations = c("Low", "Moderate", "Low"),
#'   d5_missing_data = c("Low", "Low", "Low"),
#'   d6_measurement = c("Moderate", "Low", "Low"),
#'   d7_selection_report = c("Low", "Low", "Low"),
#'   d1_support = c("No adjustment for confounders", "", ""),
#'   outcome = c("OS", "PFS", "ORR"),
#'   stringsAsFactors = FALSE
#' )
#'
#' results <- assess_robins_i_batch(robins_data)
#' results[["OBS001"]]@overall
#'
#' # Get summary of all assessments
#' summary_df <- do.call(rbind, lapply(results, function(r) r@summary_df))
#' }
assess_robins_i_batch <- function(data, .suppress_messages = FALSE) {
	# Validate input
	admiraldev::assert_data_frame(data)

	required_cols <- c(
		"study_id",
		"d1_confounding",
		"d2_selection",
		"d3_classification",
		"d4_deviations",
		"d5_missing_data",
		"d6_measurement",
		"d7_selection_report"
	)

	missing_cols <- setdiff(required_cols, names(data))
	if (length(missing_cols) > 0) {
		ph_abort(sprintf(
			"data must contain columns: %s",
			paste(required_cols, collapse = ", ")
		))
	}

	# Ensure study_id is character
	data$study_id <- as.character(data$study_id)

	# Process each row
	results <- lapply(seq_len(nrow(data)), function(i) {
		row <- data[i, ]

		# Get support columns if present
		d1_support <- .get_optional_col(row, "d1_support")
		d2_support <- .get_optional_col(row, "d2_support")
		d3_support <- .get_optional_col(row, "d3_support")
		d4_support <- .get_optional_col(row, "d4_support")
		d5_support <- .get_optional_col(row, "d5_support")
		d6_support <- .get_optional_col(row, "d6_support")
		d7_support <- .get_optional_col(row, "d7_support")

		# Get optional parameters
		outcome <- if ("outcome" %in% names(row) && !is.na(row$outcome)) {
			as.character(row$outcome)
		} else {
			""
		}

		intervention <- if (
			"intervention" %in% names(row) && !is.na(row$intervention)
		) {
			as.character(row$intervention)
		} else {
			""
		}

		comparator <- if ("comparator" %in% names(row) && !is.na(row$comparator)) {
			as.character(row$comparator)
		} else {
			""
		}

		overall <- if (
			"overall" %in% names(row) && !is.na(row$overall) && nzchar(row$overall)
		) {
			as.character(row$overall)
		} else {
			NULL
		}

		overall_justification <- if (
			"overall_justification" %in%
				names(row) &&
				!is.na(row$overall_justification) &&
				nzchar(row$overall_justification)
		) {
			as.character(row$overall_justification)
		} else {
			NULL
		}

		# Call assess_robins_i for each study
		if (!.suppress_messages) {
			ph_inform(sprintf("Assessing study: %s", row$study_id))
		}

		assess_robins_i(
			study_id = row$study_id,
			d1_confounding = row$d1_confounding,
			d2_selection = row$d2_selection,
			d3_classification = row$d3_classification,
			d4_deviations = row$d4_deviations,
			d5_missing_data = row$d5_missing_data,
			d6_measurement = row$d6_measurement,
			d7_selection_report = row$d7_selection_report,
			d1_support = d1_support,
			d2_support = d2_support,
			d3_support = d3_support,
			d4_support = d4_support,
			d5_support = d5_support,
			d6_support = d6_support,
			d7_support = d7_support,
			outcome = outcome,
			intervention = intervention,
			comparator = comparator,
			overall = overall,
			overall_justification = overall_justification
		)
	})

	# Name the list by study_id
	names(results) <- data$study_id

	results
}


#' Create Summary Table of ROBINS-I Assessments
#'
#' Combines results from multiple ROBINS-I assessments into a summary table.
#'
#' @param results List of ROBINSIResult objects.
#' @param include_justification Logical. Include justification text.
#'   Default: FALSE.
#'
#' @return A data frame with study-level summary.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- list(
#'   assess_robins_i("S1", "Serious", "Low", "Low", "Low", "Low",
#'                  "Moderate", "Low",
#'                  d6_support = "Unblinded", outcome = "OS"),
#'   assess_robins_i("S2", "Low", "Low", "Low", "Low", "Low", "Low", "Low",
#'                  outcome = "OS"),
#'   assess_robins_i("S3", "Moderate", "Low", "Low", "Low", "Low", "Low", "Low",
#'                  outcome = "PFS")
#' )
#' summary_table <- robins_i_summary(results)
#' print(summary_table)
#' }
robins_i_summary <- function(results, include_justification = FALSE) {
	# Validate input
	if (!is.list(results)) {
		ph_abort("results must be a list of ROBINSIResult objects")
	}

	if (length(results) == 0) {
		empty_df <- data.frame(
			study_id = character(),
			outcome = character(),
			intervention = character(),
			comparator = character(),
			d1_confounding = character(),
			d2_selection = character(),
			d3_classification = character(),
			d4_deviations = character(),
			d5_missing_data = character(),
			d6_measurement = character(),
			d7_selection_report = character(),
			overall = character(),
			stringsAsFactors = FALSE
		)
		if (include_justification) {
			empty_df$overall_justification <- character()
		}
		return(empty_df)
	}

	# Check all elements are ROBINSIResult
	for (i in seq_along(results)) {
		if (!S7::S7_inherits(results[[i]], ROBINSIResult)) {
			ph_abort(sprintf(
				"Element %d of results is not a ROBINSIResult object",
				i
			))
		}
	}

	# Build summary data frame
	summary_list <- lapply(results, function(r) {
		row <- data.frame(
			study_id = r@study_id,
			outcome = r@outcome,
			intervention = r@intervention,
			comparator = r@comparator,
			d1_confounding = r@domains$D1_confounding$judgment,
			d2_selection = r@domains$D2_selection$judgment,
			d3_classification = r@domains$D3_classification$judgment,
			d4_deviations = r@domains$D4_deviations$judgment,
			d5_missing_data = r@domains$D5_missing_data$judgment,
			d6_measurement = r@domains$D6_measurement$judgment,
			d7_selection_report = r@domains$D7_selection_report$judgment,
			overall = r@overall,
			stringsAsFactors = FALSE
		)

		if (include_justification) {
			row$overall_justification <- r@overall_justification
		}

		row
	})

	do.call(rbind, summary_list)
}


#' Print Method for ROBINSIResult
#'
#' @param x A ROBINSIResult object.
#' @param ... Additional arguments passed to print.
#' @keywords internal
print.ROBINSIResult <- function(x, ...) {
	cat("ROBINS-I Assessment Results\n")
	cat("============================\n")
	cat(sprintf("Study: %s\n", x@study_id))
	if (nzchar(x@outcome)) {
		cat(sprintf("Outcome: %s\n", x@outcome))
	}
	if (nzchar(x@intervention)) {
		cat(sprintf("Intervention: %s\n", x@intervention))
	}
	if (nzchar(x@comparator)) {
		cat(sprintf("Comparator: %s\n", x@comparator))
	}
	cat("\nDomain Judgments:\n")

	judgments <- x@judgment_counts
	cat(sprintf(
		"  Low: %d, Moderate: %d, Serious: %d, Critical: %d, No information: %d\n",
		judgments$n_low,
		judgments$n_moderate,
		judgments$n_serious,
		judgments$n_critical,
		judgments$n_no_info
	))

	cat("\n")
	print(x@summary_df)

	cat("\nOverall Judgment:", x@overall, "\n")
	if (nzchar(x@overall_justification)) {
		cat("Justification:", x@overall_justification, "\n")
	}

	invisible(x)
}


#' Internal Helper: Calculate Overall ROBINS-I Judgment
#'
#' Calculates the overall risk of bias judgment based on domain judgments
#' using ROBINS-I algorithm.
#'
#' @param judgments Character vector of 7 domain judgments.
#'
#' @return Character string: "Low", "Moderate", "Serious", "Critical",
#'   or "No information".
#'
#' @keywords internal
.calculate_robins_i_overall <- function(judgments) {
	# Validate input
	if (length(judgments) != 7) {
		ph_abort("judgments must be a character vector of length 7")
	}

	# Count judgments
	n_critical <- sum(judgments == "Critical")
	n_serious <- sum(judgments == "Serious")
	n_moderate <- sum(judgments == "Moderate")
	n_no_info <- sum(judgments == "No information")
	n_low <- sum(judgments == "Low")

	# Apply ROBINS-I algorithm:
	# - Critical: any "Critical"
	# - Serious: any "Serious" but no "Critical"
	# - Moderate: any "Moderate" but no "Serious" or "Critical"
	# - No information: any "No information" with all others Low/Moderate
	# - Low: all 7 domains are "Low"

	if (n_critical >= 1) {
		return("Critical")
	} else if (n_serious >= 1) {
		return("Serious")
	} else if (n_moderate >= 1) {
		return("Moderate")
	} else if (n_no_info >= 1) {
		return("No information")
	} else if (n_low == 7) {
		return("Low")
	} else {
		# Fallback - should not occur with valid judgments
		return("No information")
	}
}


#' Internal Helper: Generate ROBINS-I Justification Text
#'
#' Generates a justification string based on domain judgments.
#'
#' @param judgments Character vector of 7 domain judgments.
#' @param overall Character. Overall judgment.
#'
#' @return Character string justification.
#'
#' @keywords internal
.generate_robins_i_justification <- function(judgments, overall) {
	n_critical <- sum(judgments == "Critical")
	n_serious <- sum(judgments == "Serious")
	n_moderate <- sum(judgments == "Moderate")
	n_no_info <- sum(judgments == "No information")

	# Helper to get domain names from judgments
	get_domain_names <- function(judgment_val) {
		domains <- ROBINSI_DOMAINS[judgments == judgment_val]
		if (length(domains) == 0) {
			return(character())
		}
		# Extract short domain names
		short_names <- gsub("^D\\d+_", "", domains)
		short_names
	}

	if (overall == "Low") {
		return("All domains rated Low risk of bias")
	}

	if (overall == "Critical") {
		critical_domains <- get_domain_names("Critical")
		return(sprintf(
			"Critical risk due to: %s domain%s",
			paste(critical_domains, collapse = ", "),
			if (n_critical > 1) "s" else ""
		))
	}

	if (overall == "Serious") {
		serious_domains <- get_domain_names("Serious")
		return(sprintf(
			"Serious risk due to: %s domain%s",
			paste(serious_domains, collapse = ", "),
			if (n_serious > 1) "s" else ""
		))
	}

	if (overall == "Moderate") {
		moderate_domains <- get_domain_names("Moderate")
		return(sprintf(
			"Moderate risk due to: %s domain%s",
			paste(moderate_domains, collapse = ", "),
			if (n_moderate > 1) "s" else ""
		))
	}

	if (overall == "No information") {
		no_info_domains <- get_domain_names("No information")
		return(sprintf(
			"No information for: %s domain%s; all other domains Low/Moderate",
			paste(no_info_domains, collapse = ", "),
			if (n_no_info > 1) "s" else ""
		))
	}

	# Fallback
	return("Assessment completed")
}


#' Create ROBINS-I Summary Plot
#'
#' Generates a visualization of ROBINS-I assessments across multiple studies
#' using a stacked bar chart showing the distribution of domain judgments.
#'
#' @param results List of ROBINSIResult objects.
#' @param title Character. Plot title.
#'   Default: "Risk of Bias Assessment (ROBINS-I)".
#' @param colors Character vector. Colors for judgments in order:
#'   Low, Moderate, Serious, Critical, No information.
#'   Default: c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#95a5a6").
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- list(
#'   assess_robins_i("S1", "Serious", "Low", "Low", "Low", "Low",
#'                  "Moderate", "Low",
#'                  outcome = "OS"),
#'   assess_robins_i("S2", "Low", "Low", "Low", "Low", "Low", "Low", "Low",
#'                  outcome = "OS"),
#'   assess_robins_i("S3", "Moderate", "Low", "Low", "Low", "Low", "Low", "Low",
#'                  outcome = "PFS")
#' )
#' plot <- robins_i_plot(results)
#' print(plot)
#' }
robins_i_plot <- function(
	results,
	title = "Risk of Bias Assessment (ROBINS-I)",
	colors = c(
		"Low" = "#2ecc71",
		"Moderate" = "#f1c40f",
		"Serious" = "#e67e22",
		"Critical" = "#e74c3c",
		"No information" = "#95a5a6"
	)
) {
	# Validate input
	if (!is.list(results)) {
		ph_abort("results must be a list of ROBINSIResult objects")
	}

	if (length(results) == 0) {
		ph_abort("results list cannot be empty")
	}

	# Check all elements are ROBINSIResult
	for (i in seq_along(results)) {
		if (!S7::S7_inherits(results[[i]], ROBINSIResult)) {
			ph_abort(sprintf(
				"Element %d of results is not a ROBINSIResult object",
				i
			))
		}
	}

	# Build data frame for plotting
	plot_data <- lapply(results, function(r) {
		study_id <- r@study_id
		overall <- r@overall
		judgments <- vapply(
			ROBINSI_DOMAINS,
			function(d) r@domains[[d]]$judgment,
			character(1)
		)
		domains <- ROBINSI_DOMAINS

		data.frame(
			study_id = study_id,
			domain = domains,
			domain_label = c(
				"D1: Confounding",
				"D2: Selection",
				"D3: Classification",
				"D4: Deviations",
				"D5: Missing data",
				"D6: Measurement",
				"D7: Selection"
			),
			judgment = judgments,
			overall = overall,
			stringsAsFactors = FALSE
		)
	})

	plot_df <- do.call(rbind, plot_data)

	# Convert to factor for proper ordering
	judgment_order <- c(
		"Low",
		"Moderate",
		"Serious",
		"Critical",
		"No information"
	)
	plot_df$judgment <- factor(
		plot_df$judgment,
		levels = judgment_order,
		ordered = TRUE
	)
	plot_df$study_id <- factor(
		plot_df$study_id,
		levels = unique(plot_df$study_id)
	)

	# Create the plot
	p <- ggplot2::ggplot(
		plot_df,
		ggplot2::aes(x = .data$study_id, y = 1, fill = .data$judgment)
	) +
		ggplot2::geom_bar(position = "fill", stat = "identity", color = "white") +
		ggplot2::facet_wrap(~ .data$domain_label, nrow = 1) +
		ggplot2::scale_fill_manual(
			values = colors,
			drop = FALSE
		) +
		ggplot2::labs(
			title = title,
			x = "Study",
			y = "Proportion",
			fill = "Risk of Bias"
		) +
		ggplot2::theme_minimal() +
		ggplot2::theme(
			axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
			legend.position = "bottom",
			strip.text = ggplot2::element_text(size = 8)
		)

	p
}


#' Export ROBINS-I Assessment to Data Frame
#'
#' Converts ROBINS-I assessment results to a tidy data frame suitable for
#' export to CSV or inclusion in reports.
#'
#' @param result A ROBINSIResult object.
#' @param wide_format Logical. If TRUE, returns wide format with one row per
#'   study. If FALSE, returns long format. Default: FALSE.
#'
#' @return A data frame with assessment details.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- assess_robins_i(
#'   study_id = "OBS001",
#'   d1_confounding = "Serious",
#'   d2_selection = "Low",
#'   d3_classification = "Low",
#'   d4_deviations = "Low",
#'   d5_missing_data = "Low",
#'   d6_measurement = "Moderate",
#'   d7_selection_report = "Low",
#'   d6_support = "Outcome assessor not blinded",
#'   outcome = "Overall Survival"
#' )
#'
#' # Long format (default)
#' long_df <- robins_i_to_df(result)
#' print(long_df)
#'
#' # Wide format
#' wide_df <- robins_i_to_df(result, wide_format = TRUE)
#' print(wide_df)
#' }
robins_i_to_df <- function(result, wide_format = FALSE) {
	if (!S7::S7_inherits(result, ROBINSIResult)) {
		ph_abort("result must be a ROBINSIResult object")
	}

	if (isTRUE(wide_format)) {
		# Wide format: one row per study with all judgments as columns
		data.frame(
			study_id = result@study_id,
			outcome = result@outcome,
			intervention = result@intervention,
			comparator = result@comparator,
			d1_confounding_judgment = result@domains$D1_confounding$judgment,
			d1_confounding_support = result@domains$D1_confounding$support %||% "",
			d2_selection_judgment = result@domains$D2_selection$judgment,
			d2_selection_support = result@domains$D2_selection$support %||% "",
			d3_classification_judgment = result@domains$D3_classification$judgment,
			d3_classification_support = result@domains$D3_classification$support %||%
				"",
			d4_deviations_judgment = result@domains$D4_deviations$judgment,
			d4_deviations_support = result@domains$D4_deviations$support %||% "",
			d5_missing_data_judgment = result@domains$D5_missing_data$judgment,
			d5_missing_data_support = result@domains$D5_missing_data$support %||% "",
			d6_measurement_judgment = result@domains$D6_measurement$judgment,
			d6_measurement_support = result@domains$D6_measurement$support %||% "",
			d7_selection_report_judgment = result@domains$D7_selection_report$judgment,
			d7_selection_report_support = result@domains$D7_selection_report$support %||%
				"",
			overall = result@overall,
			overall_justification = result@overall_justification,
			stringsAsFactors = FALSE
		)
	} else {
		# Long format: one row per domain
		domain_labels <- c(
			D1_confounding = "Confounding",
			D2_selection = "Selection of participants",
			D3_classification = "Classification of interventions",
			D4_deviations = "Deviations from intended interventions",
			D5_missing_data = "Missing data",
			D6_measurement = "Measurement of outcomes",
			D7_selection_report = "Selection of reported result"
		)

		rows <- lapply(ROBINSI_DOMAINS, function(domain) {
			data.frame(
				study_id = result@study_id,
				outcome = result@outcome,
				intervention = result@intervention,
				comparator = result@comparator,
				domain = domain,
				domain_label = domain_labels[[domain]],
				judgment = result@domains[[domain]]$judgment,
				support = result@domains[[domain]]$support %||% "",
				stringsAsFactors = FALSE
			)
		})

		do.call(rbind, rows)
	}
}
