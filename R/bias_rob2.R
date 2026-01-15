#' Risk of Bias 2 (RoB 2) Assessment for RCTs
#'
#' Functions for assessing risk of bias in randomized controlled trials
#' using the Cochrane RoB 2 tool.
#'
#' @name bias_rob2
#' @references
#'   Sterne JAC, et al. (2019). RoB 2: a revised tool for assessing risk
#'   of bias in randomised trials. BMJ, 366, l4898.
#'
#'   IQWiG Allgemeine Methoden Version 7.0, Section 10.1.4, p.202-205.
NULL

# RoB 2 domain names constant
#' @keywords internal
ROB2_DOMAINS <- c(
	"D1_randomization",
	"D2_deviations",
	"D3_missing_data",
	"D4_measurement",
	"D5_selection"
)

# Valid RoB 2 judgments
#' @keywords internal
ROB2_JUDGMENTS <- c("Low", "Some concerns", "High")


#' RoB2Result S7 Class
#'
#' S7 class for storing RoB 2 assessment results for a single study.
#'
#' @param study_id Character. Study identifier.
#' @param domains List. Named list of domain judgments with supporting text.
#' @param overall Character. Overall risk of bias judgment.
#' @param overall_justification Character. Justification for overall judgment.
#' @param outcome Character. Outcome being assessed (optional).
#' @param metadata List. Additional metadata (optional).
#'
#' @return A RoB2Result object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- RoB2Result(
#'   study_id = "STUDY001",
#'   domains = list(
#'     D1_randomization = list(
#'       judgment = "Low", support = "Allocation concealed"
#'     ),
#'     D2_deviations = list(judgment = "Low", support = "No deviations"),
#'     D3_missing_data = list(judgment = "Low", support = "No missing data"),
#'     D4_measurement = list(
#'       judgment = "Some concerns", support = "Blinded assessment"
#'     ),
#'     D5_selection = list(judgment = "Low", support = "Pre-specified analysis")
#'   ),
#'   overall = "Some concerns",
#'   overall_justification = "One domain rated as Some concerns",
#'   outcome = "OS"
#' )
#' }
RoB2Result <- S7::new_class(
	"RoB2Result",
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
				if (!all(ROB2_DOMAINS %in% names(value))) {
					missing <- setdiff(ROB2_DOMAINS, names(value))
					return(sprintf(
						"domains must contain all required domains: %s",
						paste(missing, collapse = ", ")
					))
				}
				# Validate each domain has judgment
				for (domain in ROB2_DOMAINS) {
					if (!"judgment" %in% names(value[[domain]])) {
						return(sprintf("domain %s must have a 'judgment' element", domain))
					}
					judgment <- value[[domain]]$judgment
					if (!judgment %in% ROB2_JUDGMENTS) {
						return(sprintf(
							"domain %s judgment must be one of: %s",
							domain,
							paste(ROB2_JUDGMENTS, collapse = ", ")
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
				if (!value %in% ROB2_JUDGMENTS) {
					return(sprintf(
						"overall must be one of: %s",
						paste(ROB2_JUDGMENTS, collapse = ", ")
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
		metadata = S7::new_property(S7::class_list, default = list()),
		# Computed property: summary as data frame
		summary_df = S7::new_property(
			class = S7::class_data.frame,
			getter = function(self) {
				df <- data.frame(
					domain = ROB2_DOMAINS,
					domain_label = c(
						"D1: Randomization",
						"D2: Deviations from intended intervention",
						"D3: Missing outcome data",
						"D4: Measurement of the outcome",
						"D5: Selection of the reported result"
					),
					judgment = vapply(
						ROB2_DOMAINS,
						function(d) self@domains[[d]]$judgment,
						character(1)
					),
					support = vapply(
						ROB2_DOMAINS,
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
					ROB2_DOMAINS,
					function(d) self@domains[[d]]$judgment,
					character(1)
				)
				list(
					n_low = sum(judgments == "Low"),
					n_concerns = sum(judgments == "Some concerns"),
					n_high = sum(judgments == "High")
				)
			}
		)
	)
)


#' Assess Risk of Bias using RoB 2
#'
#' Assesses the risk of bias for a single study using the Cochrane RoB 2 tool.
#' The overall judgment is automatically calculated based on domain judgments
#' using the following rules:
#' \itemize{
#'   \item "Low": All domains rated "Low"
#'   \item "High": Any domain rated "High" OR 2+ domains rated "Some concerns"
#'   \item "Some concerns": All other combinations
#' }
#'
#' @param study_id Character. Study identifier.
#' @param d1_randomization Character. Domain 1 judgment: "Low", "Some concerns",
#'   or "High". For randomization process.
#' @param d2_deviations Character. Domain 2 judgment: "Low", "Some concerns",
#'   or "High". For deviations from intended interventions.
#' @param d3_missing_data Character. Domain 3 judgment: "Low", "Some concerns",
#'   or "High". For missing outcome data.
#' @param d4_measurement Character. Domain 4 judgment: "Low", "Some concerns",
#'   or "High". For measurement of the outcome.
#' @param d5_selection Character. Domain 5 judgment: "Low", "Some concerns",
#'   or "High". For selection of the reported result.
#' @param d1_support Character. Supporting text for Domain 1 (optional).
#' @param d2_support Character. Supporting text for Domain 2 (optional).
#' @param d3_support Character. Supporting text for Domain 3 (optional).
#' @param d4_support Character. Supporting text for Domain 4 (optional).
#' @param d5_support Character. Supporting text for Domain 5 (optional).
#' @param outcome Character. Outcome being assessed (optional).
#' @param overall Character. Overall judgment. If NULL, auto-calculated.
#' @param overall_justification Character. Justification for overall judgment.
#'   If NULL and overall is auto-calculated, a default
#'   justification is generated.
#' @param metadata List. Additional metadata (optional).
#'
#' @return A RoB2Result object with the assessment results.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic assessment with auto-calculated overall
#' result <- assess_rob2(
#'   study_id = "STUDY001",
#'   d1_randomization = "Low",
#'   d2_deviations = "Low",
#'   d3_missing_data = "Low",
#'   d4_measurement = "Some concerns",
#'   d5_selection = "Low",
#'   d4_support = "Outcome assessor not blinded",
#'   outcome = "Overall Survival"
#' )
#' result@overall
#' result@judgment_counts
#'
#' # Manual overall judgment
#' result2 <- assess_rob2(
#'   study_id = "STUDY002",
#'   d1_randomization = "High",
#'   d2_deviations = "Low",
#'   d3_missing_data = "Low",
#'   d4_measurement = "Low",
#'   d5_selection = "Low",
#'   overall = "High",
#'   overall_justification = "Inadequate randomization procedure"
#' )
#' }
assess_rob2 <- function(
	study_id,
	d1_randomization,
	d2_deviations,
	d3_missing_data,
	d4_measurement,
	d5_selection,
	d1_support = "",
	d2_support = "",
	d3_support = "",
	d4_support = "",
	d5_support = "",
	outcome = "",
	overall = NULL,
	overall_justification = NULL,
	metadata = list()
) {
	# Validate study_id
	if (missing(study_id) || is.null(study_id)) {
		ph_abort("study_id is required and cannot be missing or NULL")
	}
	admiraldev::assert_character_scalar(study_id)

	# Collect domain judgments
	judgments <- c(
		d1 = d1_randomization,
		d2 = d2_deviations,
		d3 = d3_missing_data,
		d4 = d4_measurement,
		d5 = d5_selection
	)

	# Validate all judgments
	for (i in seq_along(judgments)) {
		domain_name <- names(judgments)[i]
		judgment <- judgments[i]
		if (is.null(judgment) || is.na(judgment) || judgment == "") {
			ph_abort(sprintf("%s judgment is required", ROB2_DOMAINS[i]))
		}
		if (!judgment %in% ROB2_JUDGMENTS) {
			ph_abort(sprintf(
				"%s must be one of: %s",
				ROB2_DOMAINS[i],
				paste(ROB2_JUDGMENTS, collapse = ", ")
			))
		}
	}

	# Collect supporting text
	supports <- c(
		d1 = d1_support,
		d2 = d2_support,
		d3 = d3_support,
		d4 = d4_support,
		d5 = d5_support
	)

	# Build domains list
	domains <- list()
	for (i in seq_along(ROB2_DOMAINS)) {
		domains[[ROB2_DOMAINS[i]]] <- list(
			judgment = judgments[i],
			support = supports[i] %||% ""
		)
	}

	# Auto-calculate overall if not provided
	if (is.null(overall)) {
		overall <- .calculate_rob2_overall(judgments)
	}

	# Generate justification if not provided
	if (is.null(overall_justification) || overall_justification == "") {
		overall_justification <- .generate_rob2_justification(judgments, overall)
	}

	# Validate overall judgment
	if (!overall %in% ROB2_JUDGMENTS) {
		ph_abort(sprintf(
			"overall must be one of: %s",
			paste(ROB2_JUDGMENTS, collapse = ", ")
		))
	}

	# Create and return RoB2Result
	RoB2Result(
		study_id = study_id,
		domains = domains,
		overall = overall,
		overall_justification = overall_justification,
		outcome = outcome,
		metadata = metadata
	)
}


#' Assess Multiple Studies with RoB 2
#'
#' Performs RoB 2 risk of bias assessment for multiple studies
#' from a data frame.
#'
#' @param data Data frame with columns for study_id, domain judgments, and
#'   optionally supporting text. Required columns:
#'   \itemize{
#'     \item study_id: Study identifier
#'     \item d1_randomization: Domain 1 judgment
#'     \item d2_deviations: Domain 2 judgment
#'     \item d3_missing_data: Domain 3 judgment
#'     \item d4_measurement: Domain 4 judgment
#'     \item d5_selection: Domain 5 judgment
#'   }
#'   Optional columns: d1_support, d2_support, d3_support,
#'   d4_support, d5_support, outcome, overall,
#'   overall_justification
#' @param .suppress_messages Logical. If TRUE, suppresses individual assessment
#'   messages. Default: FALSE
#'
#' @return A list of RoB2Result objects, named by study_id.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create data frame with study assessments
#' rob_data <- data.frame(
#'   study_id = c("STUDY001", "STUDY002", "STUDY003"),
#'   d1_randomization = c("Low", "Low", "High"),
#'   d2_deviations = c("Low", "Some concerns", "Low"),
#'   d3_missing_data = c("Low", "Low", "Low"),
#'   d4_measurement = c("Some concerns", "Low", "Low"),
#'   d5_selection = c("Low", "Low", "Low"),
#'   d4_support = c("Unblinded assessment", "", ""),
#'   outcome = c("OS", "PFS", "OS"),
#'   stringsAsFactors = FALSE
#' )
#'
#' results <- assess_rob2_batch(rob_data)
#' results[["STUDY001"]]@overall
#'
#' # Get summary of all assessments
#' summary_df <- do.call(rbind, lapply(results, function(r) r@summary_df))
#' }
assess_rob2_batch <- function(data, .suppress_messages = FALSE) {
	# Validate input
	admiraldev::assert_data_frame(data)

	required_cols <- c(
		"study_id",
		"d1_randomization",
		"d2_deviations",
		"d3_missing_data",
		"d4_measurement",
		"d5_selection"
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
		d1_support <- if ("d1_support" %in% names(row) && !is.na(row$d1_support)) {
			row$d1_support
		} else {
			""
		}
		d2_support <- if ("d2_support" %in% names(row) && !is.na(row$d2_support)) {
			row$d2_support
		} else {
			""
		}
		d3_support <- if ("d3_support" %in% names(row) && !is.na(row$d3_support)) {
			row$d3_support
		} else {
			""
		}
		d4_support <- if ("d4_support" %in% names(row) && !is.na(row$d4_support)) {
			row$d4_support
		} else {
			""
		}
		d5_support <- if ("d5_support" %in% names(row) && !is.na(row$d5_support)) {
			row$d5_support
		} else {
			""
		}

		# Get optional parameters
		outcome <- if ("outcome" %in% names(row) && !is.na(row$outcome)) {
			as.character(row$outcome)
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

		# Call assess_rob2 for each study
		if (!.suppress_messages) {
			message(sprintf("Assessing study: %s", row$study_id))
		}

		assess_rob2(
			study_id = row$study_id,
			d1_randomization = row$d1_randomization,
			d2_deviations = row$d2_deviations,
			d3_missing_data = row$d3_missing_data,
			d4_measurement = row$d4_measurement,
			d5_selection = row$d5_selection,
			d1_support = d1_support,
			d2_support = d2_support,
			d3_support = d3_support,
			d4_support = d4_support,
			d5_support = d5_support,
			outcome = outcome,
			overall = overall,
			overall_justification = overall_justification
		)
	})

	# Name the list by study_id
	names(results) <- data$study_id

	results
}


#' Create Summary Table of RoB 2 Assessments
#'
#' Combines results from multiple RoB 2 assessments into a summary table.
#'
#' @param results List of RoB2Result objects.
#' @param include_justification Logical. Include justification text.
#'   Default: FALSE.
#'
#' @return A data frame with study-level summary.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- list(
#'   assess_rob2("S1", "Low", "Low", "Low", "Some concerns", "Low",
#'               d4_support = "Unblinded", outcome = "OS"),
#'   assess_rob2("S2", "Low", "Low", "Low", "Low", "Low", outcome = "OS")
#' )
#' summary_table <- rob2_summary(results)
#' }
rob2_summary <- function(results, include_justification = FALSE) {
	# Validate input
	if (!is.list(results)) {
		ph_abort("results must be a list of RoB2Result objects")
	}

	if (length(results) == 0) {
		return(data.frame(
			study_id = character(),
			outcome = character(),
			d1_randomization = character(),
			d2_deviations = character(),
			d3_missing_data = character(),
			d4_measurement = character(),
			d5_selection = character(),
			overall = character(),
			stringsAsFactors = FALSE
		))
	}

	# Check all elements are RoB2Result
	for (i in seq_along(results)) {
		if (!S7::S7_inherits(results[[i]], RoB2Result)) {
			ph_abort(sprintf(
				"Element %d of results is not a RoB2Result object",
				i
			))
		}
	}

	# Build summary data frame
	summary_list <- lapply(results, function(r) {
		row <- data.frame(
			study_id = r@study_id,
			outcome = r@outcome,
			d1_randomization = r@domains$D1_randomization$judgment,
			d2_deviations = r@domains$D2_deviations$judgment,
			d3_missing_data = r@domains$D3_missing_data$judgment,
			d4_measurement = r@domains$D4_measurement$judgment,
			d5_selection = r@domains$D5_selection$judgment,
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


#' Print Method for RoB2Result
#'
#' @param x A RoB2Result object.
#' @param ... Additional arguments passed to print.
#' @keywords internal
print.RoB2Result <- function(x, ...) {
	cat("RoB 2 Assessment Results\n")
	cat("========================\n")
	cat(sprintf("Study: %s\n", x@study_id))
	if (nzchar(x@outcome)) {
		cat(sprintf("Outcome: %s\n", x@outcome))
	}
	cat("\nDomain Judgments:\n")

	judgments <- x@judgment_counts
	cat(sprintf(
		"  Low: %d, Some concerns: %d, High: %d\n",
		judgments$n_low,
		judgments$n_concerns,
		judgments$n_high
	))

	cat("\n")
	print(x@summary_df)

	cat("\nOverall Judgment:", x@overall, "\n")
	if (nzchar(x@overall_justification)) {
		cat("Justification:", x@overall_justification, "\n")
	}

	invisible(x)
}


#' Internal Helper: Calculate Overall RoB 2 Judgment
#'
#' Calculates the overall risk of bias judgment based on domain judgments
#' using RoB 2 algorithm.
#'
#' @param judgments Character vector of 5 domain judgments.
#'
#' @return Character string: "Low", "Some concerns", or "High".
#'
#' @keywords internal
.calculate_rob2_overall <- function(judgments) {
	# Validate input
	if (length(judgments) != 5) {
		ph_abort("judgments must be a character vector of length 5")
	}

	# Count judgments
	n_high <- sum(judgments == "High")
	n_concerns <- sum(judgments == "Some concerns")
	n_low <- sum(judgments == "Low")

	# Apply RoB 2 algorithm:
	# - Low: all 5 domains are "Low"
	# - High: any "High" OR 2+ "Some concerns"
	# - Some concerns: everything else

	if (n_low == 5) {
		return("Low")
	} else if (n_high >= 1 || n_concerns >= 2) {
		return("High")
	} else {
		return("Some concerns")
	}
}


#' Internal Helper: Generate RoB 2 Justification Text
#'
#' Generates a justification string based on domain judgments.
#'
#' @param judgments Character vector of 5 domain judgments.
#' @param overall Character. Overall judgment.
#'
#' @return Character string justification.
#'
#' @keywords internal
.generate_rob2_justification <- function(judgments, overall) {
	n_high <- sum(judgments == "High")
	n_concerns <- sum(judgments == "Some concerns")

	if (overall == "Low") {
		return("All domains rated Low risk of bias")
	}

	if (overall == "High") {
		reasons <- character()
		if (n_high >= 1) {
			high_domains <- ROB2_DOMAINS[judgments == "High"]
			domain_names <- gsub("^D\\d+_", "", high_domains)
			reasons <- c(
				reasons,
				sprintf(
					"%s domain%s rated High",
					paste(domain_names, collapse = ", "),
					if (n_high > 1) "s" else ""
				)
			)
		}
		if (n_concerns >= 2) {
			reasons <- c(
				reasons,
				sprintf(
					"%d domains rated Some concerns",
					n_concerns
				)
			)
		}
		return(paste(reasons, collapse = "; "))
	}

	# Some concerns
	concerns_domains <- ROB2_DOMAINS[judgments == "Some concerns"]
	domain_names <- gsub("^D\\d+_", "", concerns_domains)
	sprintf(
		"%s domain%s rated Some concerns",
		paste(domain_names, collapse = ", "),
		if (length(domain_names) > 1) "s" else ""
	)
}
