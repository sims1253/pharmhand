#' @title Subgroup Analysis Tables
#' @name efficacy_subgroup
#' @description Functions for subgroup analysis tables and
#'   credibility assessment.
#' @importFrom tidyselect all_of
NULL

#' Create Subgroup Analysis Table
#'
#' @param adsl ADSL data frame. Subgroup variables can be sourced from adsl if
#'   not present in advs. Must contain USUBJID column for joining.
#' @param advs ADVS data frame
#' @param paramcd Parameter code to analyze
#' @param visit Visit to analyze
#' @param subgroups Named list of subgroup variables with display labels
#'   (e.g. list(AGEGR1="Age Group", SEX="Sex")). Variables can come from
#'   advs or adsl (joined by USUBJID).
#' @param min_subgroup_size Numeric. Minimum subgroup size required before
#'   warnings are issued. Use NULL to disable warnings. Default is 20.
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_subgroup_analysis_table <- function(
	adsl,
	advs,
	paramcd = "SYSBP",
	visit = "End of Treatment",
	subgroups = list(AGEGR1 = "Age Group", SEX = "Sex"),
	min_subgroup_size = 20,
	trt_var = "TRT01P",
	title = "Subgroup Analysis",
	autofit = TRUE
) {
	assert_data_frame(adsl, "adsl")
	assert_data_frame(advs, "advs")

	if (!is.null(min_subgroup_size)) {
		assert_numeric_scalar(min_subgroup_size, "min_subgroup_size")
		if (min_subgroup_size < 1) {
			ph_abort("'min_subgroup_size' must be >= 1")
		}
	}

	required_cols <- c("PARAMCD", "AVISIT", trt_var, "AVAL", "USUBJID")
	missing_cols <- setdiff(required_cols, names(advs))
	if (length(missing_cols) > 0) {
		ph_abort(
			sprintf(
				"'advs' is missing required columns: %s. Required: %s",
				paste(missing_cols, collapse = ", "),
				paste(required_cols, collapse = ", ")
			)
		)
	}

	# Validate adsl has USUBJID for joining
	if (!"USUBJID" %in% names(adsl)) {
		ph_abort("'adsl' is missing required column: USUBJID")
	}

	# Compute subgroup variables and handle missing ones
	subgroup_vars <- names(subgroups)
	missing_from_advs <- setdiff(subgroup_vars, names(advs))

	# Check which missing variables are in adsl
	in_adsl <- intersect(missing_from_advs, names(adsl))
	missing_from_both <- setdiff(missing_from_advs, names(adsl))

	if (length(missing_from_both) > 0) {
		ph_abort(
			sprintf(
				"Subgroup variables not found in advs or adsl: %s",
				paste(missing_from_both, collapse = ", ")
			)
		)
	}

	# Join subgroup columns from adsl if needed
	if (length(in_adsl) > 0) {
		if (nrow(adsl) > dplyr::n_distinct(adsl$USUBJID)) {
			ph_abort("'adsl' must have unique USUBJID (one row per subject)")
		}

		advs <- advs |>
			dplyr::left_join(
				adsl[, c("USUBJID", in_adsl), drop = FALSE],
				by = dplyr::join_by(USUBJID)
			)
	}

	subgroup_data_raw <- advs |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			.data$AVISIT == visit
		)

	subgroup_counts_list <- list()
	results_list <- list()

	# Iterate through subgroups
	for (var_name in names(subgroups)) {
		label <- subgroups[[var_name]]

		# Check if subgroup variable exists in data
		assert_column_exists(subgroup_data_raw, var_name, "subgroup_data_raw")

		if (!is.null(min_subgroup_size)) {
			subgroup_counts <- subgroup_data_raw |>
				dplyr::summarise(
					n = dplyr::n_distinct(.data$USUBJID),
					.by = all_of(var_name)
				) |>
				dplyr::mutate(
					subgroup = paste0(label, ": ", .data[[var_name]])
				)
			subgroup_counts_list[[var_name]] <- subgroup_counts
		}

		res <- subgroup_data_raw |>
			dplyr::summarise(
				n = dplyr::n(),
				Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
				SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
				.by = c(all_of(trt_var), all_of(var_name))
			) |>
			dplyr::mutate(
				Subgroup = label,
				Category = .data[[var_name]],
				display = paste0(
					.data$n,
					" / ",
					.data$Mean,
					" (",
					.data$SD,
					")"
				)
			)
		results_list[[var_name]] <- res
	}

	warn_small_subgroups(subgroup_counts_list, min_subgroup_size)

	all_subgroups <- dplyr::bind_rows(results_list) |>
		dplyr::select("Subgroup", "Category", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "--"
		)

	subgroup_ft <- create_hta_table(
		all_subgroups,
		title = title,
		footnotes = c(
			"Safety Population",
			"Format: n / Mean (SD)"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = all_subgroups,
		flextable = subgroup_ft,
		type = "subgroup",
		title = title,
		metadata = list(
			subgroup_counts = if (length(subgroup_counts_list) > 0) {
				dplyr::bind_rows(subgroup_counts_list)
			} else {
				NULL
			},
			min_subgroup_size = min_subgroup_size
		)
	)
}

#' Create Subgroup Analysis Table
#'
#' Generates a tabular summary of subgroup analyses for HTA dossiers and
#' Word documents. Provides the same information as `create_forest_plot()`
#' in table format showing treatment effects (HR or OR) by subgroup with
#' confidence intervals and interaction p-values.
#'
#' @param data ADaMData object or data frame
#' @param subgroups Named list mapping variable names to display labels,
#'   e.g., `list(AGEGR1 = "Age Group", SEX = "Sex", RACE = "Race")`
#' @param min_subgroup_size Numeric. Minimum subgroup size required before
#'   warnings are issued. Use NULL to disable warnings. Default is 20.
#' @param endpoint_type "tte" for time-to-event (HR) or "binary" for
#'   binary outcomes (OR)
#' @param time_var Time variable for TTE endpoints (default: "AVAL")
#' @param event_var Event variable for TTE endpoints. If "CNSR", will be
#'   inverted automatically. Default: "CNSR"
#' @param response_var Response variable for binary endpoints (default: "AVALC")
#' @param response_values Values indicating response for binary endpoints
#'   (default: c("CR", "PR"))
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param ref_group Reference treatment group. If NULL, uses first level.
#' @param conf_level Confidence level (default: 0.95)
#' @param show_interaction Logical, calculate and show interaction p-values
#'   (default: TRUE)
#' @param adjust_method Character. Method for adjusting interaction p-values
#'   for multiple comparisons. One of:
#'   - "none" (default): No adjustment
#'   - "holm": Holm-Bonferroni step-down (recommended for FWER control)
#'   - "hochberg": Hochberg step-up (controls FWER)
#'   - "hommel": Hommel's method (closed testing procedure, good for
#'     correlated tests)
#'   - "bonferroni": Bonferroni correction (conservative)
#'   - "BH" or "fdr": Benjamini-Hochberg (controls FDR)
#'   - "BY": Benjamini-Yekutieli (controls FDR under dependency)
#'   See \code{\link{adjust_pvalues}} for details.
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' # TTE subgroup table
#' sg_table <- create_subgroup_table(
#'   data = adtte,
#'   subgroups = list(
#'     AGEGR1 = "Age Group",
#'     SEX = "Sex"
#'   ),
#'   endpoint_type = "tte",
#'   title = "Subgroup Analysis - Overall Survival"
#' )
#'
#' # Binary endpoint subgroup table
#' sg_table <- create_subgroup_table(
#'   data = adrs,
#'   subgroups = list(SEX = "Sex"),
#'   endpoint_type = "binary",
#'   response_values = c("CR", "PR"),
#'   title = "Response Rate by Subgroup"
#' )
#'
#' # With multiplicity adjustment for GBA dossiers
#' sg_table <- create_subgroup_table(
#'   data = adtte,
#'   subgroups = list(
#'     AGEGR1 = "Age Group",
#'     SEX = "Sex",
#'     RACE = "Race"
#'   ),
#'   endpoint_type = "tte",
#'   adjust_method = "holm",
#'   title = "Subgroup Analysis with Multiplicity Adjustment"
#' )
#' }
create_subgroup_table <- function(
	data,
	subgroups,
	min_subgroup_size = 20,
	endpoint_type = c("tte", "binary"),
	time_var = "AVAL",
	event_var = "CNSR",
	response_var = "AVALC",
	response_values = c("CR", "PR"),
	trt_var = "TRT01P",
	ref_group = NULL,
	conf_level = 0.95,
	show_interaction = TRUE,
	adjust_method = c(
		"none",
		"holm",
		"hochberg",
		"hommel",
		"bonferroni",
		"BH",
		"fdr",
		"BY"
	),
	title = "Subgroup Analysis",
	autofit = TRUE
) {
	adjust_method <- match.arg(adjust_method)
	endpoint_type <- match.arg(endpoint_type)

	if (!is.null(min_subgroup_size)) {
		assert_numeric_scalar(min_subgroup_size, "min_subgroup_size")
		if (min_subgroup_size < 1) {
			ph_abort("'min_subgroup_size' must be >= 1")
		}
	}

	# Get filtered data
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)
	subject_var <- get_subject_var(data, default = "USUBJID")

	# Handle CNSR inversion for TTE
	if (endpoint_type == "tte" && event_var == "CNSR") {
		df$event <- 1 - df[[event_var]]
		event_var_use <- "event"
	} else if (endpoint_type == "tte") {
		event_var_use <- event_var
	} else {
		event_var_use <- NULL
	}

	# Handle binary response
	if (endpoint_type == "binary") {
		df$responder <- as.integer(df[[response_var]] %in% response_values)
	}

	# Ensure treatment is factor
	df[[trt_var_actual]] <- as.factor(df[[trt_var_actual]])
	trt_levels <- levels(df[[trt_var_actual]])

	if (is.null(ref_group)) {
		ref_group <- trt_levels[1]
	}

	# Determine other treatment (for column headers)
	other_trt <- setdiff(trt_levels, ref_group)
	if (length(other_trt) > 1) {
		ph_warn(
			"Multiple treatment arms vs reference. Using first non-reference arm."
		)
		other_trt <- other_trt[1]
	}

	# Estimate label
	estimate_label <- if (endpoint_type == "tte") "HR" else "OR"

	# Calculate overall estimate
	overall_result <- calculate_subgroup_effect_table(
		df = df,
		subgroup_var = NULL,
		subgroup_level = "Overall",
		endpoint_type = endpoint_type,
		time_var = time_var,
		event_var_use = event_var_use,
		trt_var = trt_var_actual,
		ref_group = ref_group,
		conf_level = conf_level
	)

	# Build results list
	results_list <- list(
		data.frame(
			Subgroup = "Overall",
			Category = "",
			n_ref = overall_result$n_ref,
			n_trt = overall_result$n_trt,
			estimate = overall_result$estimate,
			lcl = overall_result$lcl,
			ucl = overall_result$ucl,
			interaction_p = NA_real_,
			stringsAsFactors = FALSE
		)
	)

	subgroup_counts_list <- list()

	# Calculate estimates for each subgroup
	for (var_name in names(subgroups)) {
		label <- subgroups[[var_name]]

		if (!var_name %in% names(df)) {
			ph_warn(sprintf("Subgroup variable '%s' not found, skipping", var_name))
			next
		}

		if (!is.null(min_subgroup_size)) {
			subgroup_data <- df |>
				dplyr::filter(!is.na(.data[[var_name]]))
			if (subject_var %in% names(df)) {
				subgroup_counts <- subgroup_data |>
					dplyr::summarise(
						n = dplyr::n_distinct(.data[[subject_var]]),
						.by = all_of(var_name)
					) |>
					dplyr::mutate(
						subgroup = paste0(label, ": ", .data[[var_name]])
					)
			} else {
				subgroup_counts <- subgroup_data |>
					dplyr::summarise(
						n = dplyr::n(),
						.by = all_of(var_name)
					) |>
					dplyr::mutate(
						subgroup = paste0(label, ": ", .data[[var_name]])
					)
			}
			subgroup_counts_list[[var_name]] <- subgroup_counts
		}

		levels_var <- unique(df[[var_name]])
		levels_var <- levels_var[!is.na(levels_var)]

		# Calculate interaction p-value for this variable
		interaction_p <- NA_real_
		if (show_interaction) {
			interaction_p <- calculate_interaction_pvalue_table(
				df = df,
				subgroup_var = var_name,
				endpoint_type = endpoint_type,
				time_var = time_var,
				event_var_use = event_var_use,
				trt_var = trt_var_actual
			)
		}

		for (i in seq_along(levels_var)) {
			lvl <- levels_var[i]
			result <- calculate_subgroup_effect_table(
				df = df,
				subgroup_var = var_name,
				subgroup_level = as.character(lvl),
				endpoint_type = endpoint_type,
				time_var = time_var,
				event_var_use = event_var_use,
				trt_var = trt_var_actual,
				ref_group = ref_group,
				conf_level = conf_level
			)

			# Only show interaction p for first level of this subgroup
			show_int_p <- if (i == 1) interaction_p else NA_real_

			results_list <- c(
				results_list,
				list(data.frame(
					Subgroup = label,
					Category = as.character(lvl),
					n_ref = result$n_ref,
					n_trt = result$n_trt,
					estimate = result$estimate,
					lcl = result$lcl,
					ucl = result$ucl,
					interaction_p = show_int_p,
					stringsAsFactors = FALSE
				))
			)
		}
	}

	warn_small_subgroups(subgroup_counts_list, min_subgroup_size)

	# Combine results
	results_df <- do.call(rbind, results_list)

	# Apply multiplicity adjustment to interaction p-values if requested
	if (show_interaction && adjust_method != "none") {
		# Extract unique interaction p-values (one per subgroup variable)
		# These are in rows where interaction_p is not NA
		pval_rows <- which(!is.na(results_df$interaction_p))

		# Adjustment only needed when >1 p-value; skip even if
		# adjust_method != "none".
		if (length(pval_rows) > 1) {
			# Get the original p-values
			original_pvals <- results_df$interaction_p[pval_rows]

			# Adjust p-values
			adjusted <- adjust_pvalues(original_pvals, method = adjust_method)

			# Store both original and adjusted values
			results_df$interaction_p_original <- results_df$interaction_p
			results_df$interaction_p[pval_rows] <- adjusted$adjusted_p
			results_df$interaction_p_adjusted <- NA_real_
			results_df$interaction_p_adjusted[pval_rows] <- adjusted$adjusted_p
		}
	}

	# Format for display
	results_df$`n (Reference)` <- as.character(results_df$n_ref)
	results_df$`n (Treatment)` <- as.character(results_df$n_trt)

	results_df[[paste0(estimate_label, " (95% CI)")]] <- ifelse(
		is.na(results_df$estimate),
		"NE",
		sprintf(
			"%.2f (%.2f, %.2f)",
			results_df$estimate,
			results_df$lcl,
			results_df$ucl
		)
	)

	results_df$`Interaction p` <- ifelse(
		is.na(results_df$interaction_p),
		"",
		format_pvalue(results_df$interaction_p)
	)

	# Select display columns
	display_cols <- c(
		"Subgroup",
		"Category",
		"n (Reference)",
		"n (Treatment)",
		paste0(estimate_label, " (95% CI)")
	)

	if (show_interaction) {
		display_cols <- c(display_cols, "Interaction p")
	}

	display_df <- results_df[, display_cols, drop = FALSE]

	# Rename treatment columns with group names
	names(display_df)[names(display_df) == "n (Reference)"] <-
		paste0("n (", ref_group, ")")
	names(display_df)[names(display_df) == "n (Treatment)"] <-
		paste0("n (", other_trt, ")")

	# Build footnotes
	footnotes <- c(
		paste(
			estimate_label,
			"=",
			if (endpoint_type == "tte") {
				"Hazard Ratio"
			} else {
				"Odds Ratio"
			}
		),
		paste("Reference group:", ref_group)
	)

	if (show_interaction) {
		if (adjust_method != "none" && length(names(subgroups)) > 1) {
			method_name <- switch(
				adjust_method,
				"holm" = "Holm-Bonferroni",
				"hochberg" = "Hochberg",
				"hommel" = "Hommel",
				"bonferroni" = "Bonferroni",
				"BH" = "Benjamini-Hochberg (FDR)",
				"fdr" = "Benjamini-Hochberg (FDR)",
				"BY" = "Benjamini-Yekutieli",
				adjust_method
			)
			footnotes <- c(
				footnotes,
				paste0(
					"Interaction p-values adjusted for multiple comparisons (",
					method_name,
					" method)"
				)
			)
		} else {
			footnotes <- c(
				footnotes,
				"Interaction p-value from likelihood ratio test"
			)
		}
	}

	footnotes <- c(footnotes, "NE = Not Estimable")

	# Create flextable
	ft <- create_hta_table(
		display_df,
		title = title,
		footnotes = footnotes,
		autofit = autofit
	)

	ClinicalTable(
		data = display_df,
		flextable = ft,
		type = "subgroup",
		title = title,
		metadata = list(
			subgroups = subgroups,
			endpoint_type = endpoint_type,
			ref_group = ref_group,
			adjust_method = adjust_method,
			full_results = results_df
		)
	)
}

#' @keywords internal
warn_small_subgroups <- function(subgroup_counts_list, min_subgroup_size) {
	if (is.null(min_subgroup_size) || length(subgroup_counts_list) == 0) {
		return(invisible(NULL))
	}

	subgroup_counts <- dplyr::bind_rows(subgroup_counts_list)
	small_subgroups <- subgroup_counts |>
		dplyr::filter(.data$n < min_subgroup_size)

	if (nrow(small_subgroups) > 0) {
		ph_warn(sprintf(
			paste(
				"Small subgroup warning: %d subgroup(s) have fewer than %d subjects.",
				"Results may be unreliable for: %s.",
				"Consider combining small subgroups or interpreting with caution."
			),
			nrow(small_subgroups),
			min_subgroup_size,
			paste(small_subgroups$subgroup, collapse = ", ")
		))
	}

	invisible(NULL)
}

#' Calculate Subgroup Effect for Table (HR or OR)
#'
#' @param df Data frame
#' @param subgroup_var Subgroup variable name (NULL for overall)
#' @param subgroup_level Subgroup level value
#' @param endpoint_type "tte" or "binary"
#' @param time_var Time variable for TTE
#' @param event_var_use Event variable for TTE
#' @param trt_var Treatment variable
#' @param ref_group Reference group
#' @param conf_level Confidence level
#'
#' @return List with estimate, CI, counts
#' @keywords internal
calculate_subgroup_effect_table <- function(
	df,
	subgroup_var,
	subgroup_level,
	endpoint_type,
	time_var,
	event_var_use,
	trt_var,
	ref_group,
	conf_level
) {
	# Filter to subgroup if specified
	if (!is.null(subgroup_var)) {
		df <- df[df[[subgroup_var]] == subgroup_level, ]
	}

	# Get counts
	n_trt <- sum(df[[trt_var]] != ref_group)
	n_ref <- sum(df[[trt_var]] == ref_group)

	# Default NA result
	result <- list(
		n_trt = n_trt,
		n_ref = n_ref,
		estimate = NA_real_,
		lcl = NA_real_,
		ucl = NA_real_
	)

	# Need at least some subjects in each arm
	if (n_trt < 2 || n_ref < 2) {
		return(result)
	}

	if (endpoint_type == "tte") {
		# Cox model for HR
		df[[trt_var]] <- stats::relevel(factor(df[[trt_var]]), ref = ref_group)
		surv_obj <- survival::Surv(df[[time_var]], df[[event_var_use]])

		tryCatch(
			{
				cox_fit <- survival::coxph(
					stats::as.formula(paste("surv_obj ~", trt_var)),
					data = df
				)
				cox_summary <- summary(cox_fit, conf.int = conf_level)

				result$estimate <- cox_summary$conf.int[1, "exp(coef)"]
				# Build column names from conf_level
				conf_pct <- sub("^0", "", as.character(conf_level))
				lower_col <- paste0("lower ", conf_pct)
				upper_col <- paste0("upper ", conf_pct)
				result$lcl <- cox_summary$conf.int[1, lower_col]
				result$ucl <- cox_summary$conf.int[1, upper_col]
			},
			error = function(e) {
				# Keep NA values
			}
		)
	} else {
		# Logistic regression for OR
		df[[trt_var]] <- stats::relevel(factor(df[[trt_var]]), ref = ref_group)

		tryCatch(
			{
				glm_fit <- stats::glm(
					stats::as.formula(paste("responder ~", trt_var)),
					data = df,
					family = stats::binomial()
				)

				result$estimate <- exp(stats::coef(glm_fit)[2])
				ci <- exp(stats::confint.default(glm_fit, level = conf_level)[2, ])
				result$lcl <- ci[1]
				result$ucl <- ci[2]
			},
			error = function(e) {
				# Keep NA values
			}
		)
	}

	result
}

#' Calculate Interaction P-value for Table
#'
#' @param df Data frame
#' @param subgroup_var Subgroup variable
#' @param endpoint_type "tte" or "binary"
#' @param time_var Time variable for TTE
#' @param event_var_use Event variable for TTE
#' @param trt_var Treatment variable
#'
#' @return Interaction p-value
#' @keywords internal
calculate_interaction_pvalue_table <- function(
	df,
	subgroup_var,
	endpoint_type,
	time_var,
	event_var_use,
	trt_var
) {
	tryCatch(
		{
			if (endpoint_type == "tte") {
				surv_obj <- survival::Surv(df[[time_var]], df[[event_var_use]])

				# Model without interaction
				formula_main <- stats::as.formula(
					paste("surv_obj ~", trt_var, "+", subgroup_var)
				)
				# Model with interaction
				formula_int <- stats::as.formula(
					paste("surv_obj ~", trt_var, "*", subgroup_var)
				)

				fit_main <- survival::coxph(formula_main, data = df)
				fit_int <- survival::coxph(formula_int, data = df)

				# Likelihood ratio test
				lr_test <- stats::anova(fit_main, fit_int)
				pvalue <- lr_test[["Pr(>|Chi|)"]][2]
			} else {
				# Model without interaction
				formula_main <- stats::as.formula(
					paste("responder ~", trt_var, "+", subgroup_var)
				)
				# Model with interaction
				formula_int <- stats::as.formula(
					paste("responder ~", trt_var, "*", subgroup_var)
				)

				fit_main <- stats::glm(
					formula_main,
					data = df,
					family = stats::binomial()
				)
				fit_int <- stats::glm(
					formula_int,
					data = df,
					family = stats::binomial()
				)

				# Likelihood ratio test
				lr_test <- stats::anova(fit_main, fit_int, test = "Chisq")
				pvalue <- lr_test[["Pr(>Chi)"]][2]
			}

			pvalue
		},
		error = function(e) {
			NA_real_
		}
	)
}

#' Assess Subgroup Credibility Using ICEMAN Criteria
#'
#' Evaluates the credibility of subgroup analyses using the ICEMAN criteria.
#' The 10 criteria assess whether an apparent subgroup effect is likely to
#' be a true effect modification.
#'
#' @param subgroup_result Subgroup analysis result (from create_subgroup_table)
#' @param is_prespecified Logical. Was the subgroup prespecified?
#' @param hypothesis_direction Character. Was direction prespecified?
#'   Options: "correct", "opposite", "none"
#' @param n_subgroups Integer. Total number of subgroups analyzed
#' @param biological_rationale Character. Strength of biological rationale:
#'   "strong", "moderate", "weak", "none"
#' @param effect_measure Character. Consistent with overall effect?
#'   "consistent", "opposite", "uncertain"
#' @param within_study Logical. Is this within-study comparison?
#' @param statistical_test Character. Type of interaction test used:
#'   "formal", "informal", "none"
#' @param interaction_pvalue Numeric. P-value for interaction (if tested)
#' @param replication Character. Has finding been replicated?
#'   "yes", "no", "not_applicable"
#' @param other_evidence Character. Supporting evidence from other sources:
#'   "strong", "moderate", "weak", "none"
#'
#' @return ICEMANResult object with criteria assessments and overall credibility
#' @export
assess_iceman <- function(
	subgroup_result = NULL,
	is_prespecified = FALSE,
	hypothesis_direction = c("none", "correct", "opposite"),
	n_subgroups = 1,
	biological_rationale = c("none", "weak", "moderate", "strong"),
	effect_measure = c("uncertain", "consistent", "opposite"),
	within_study = TRUE,
	statistical_test = c("none", "informal", "formal"),
	interaction_pvalue = NA_real_,
	replication = c("not_applicable", "no", "yes"),
	other_evidence = c("none", "weak", "moderate", "strong")
) {
	hypothesis_direction <- match.arg(hypothesis_direction)
	biological_rationale <- match.arg(biological_rationale)
	effect_measure <- match.arg(effect_measure)
	statistical_test <- match.arg(statistical_test)
	replication <- match.arg(replication)
	other_evidence <- match.arg(other_evidence)

	# Note: subgroup_result parameter reserved for future automatic extraction
	# of subgroup analysis metrics. Currently requires manual input of criteria.

	if (
		!is.logical(is_prespecified) ||
			length(is_prespecified) != 1 ||
			is.na(is_prespecified)
	) {
		ph_abort("'is_prespecified' must be TRUE or FALSE")
	}
	if (
		!is.logical(within_study) ||
			length(within_study) != 1 ||
			is.na(within_study)
	) {
		ph_abort("'within_study' must be TRUE or FALSE")
	}
	if (
		!is.numeric(n_subgroups) ||
			length(n_subgroups) != 1 ||
			is.na(n_subgroups) ||
			n_subgroups < 1
	) {
		ph_abort("'n_subgroups' must be a positive integer")
	}
	if (
		!is.na(interaction_pvalue) &&
			(interaction_pvalue < 0 || interaction_pvalue > 1)
	) {
		ph_abort("'interaction_pvalue' must be between 0 and 1")
	}

	# ICEMAN Criteria scoring (based on published instrument)
	# Each criterion scored as: definitely yes, probably yes,
	# probably no, definitely no
	# We'll use numeric scores: 3 = definitely yes, 2 = probably yes,
	# 1 = probably no, 0 = definitely no

	criteria <- list()

	# Criterion 1: Was the subgroup variable prespecified?
	criteria$prespecified <- list(
		name = "Subgroup prespecified",
		score = if (is_prespecified) 3 else 0,
		assessment = if (is_prespecified) "Yes" else "No",
		weight = 1
	)

	# Criterion 2: Was the direction of subgroup effect prespecified?
	dir_score <- switch(
		hypothesis_direction,
		"correct" = 3,
		"opposite" = 0,
		"none" = 1
	)
	criteria$direction <- list(
		name = "Direction prespecified",
		score = dir_score,
		assessment = hypothesis_direction,
		weight = 1
	)

	# Criterion 3: Was the subgroup effect one of a small number tested?
	subgroup_score <- if (n_subgroups <= 3) {
		3
	} else if (n_subgroups <= 5) {
		2
	} else if (n_subgroups <= 10) {
		1
	} else {
		0
	}
	criteria$n_subgroups <- list(
		name = "Limited number of subgroups",
		score = subgroup_score,
		assessment = sprintf("%d subgroups analyzed", n_subgroups),
		weight = 1
	)

	# Criterion 4: Is there biological rationale?
	bio_score <- switch(
		biological_rationale,
		"strong" = 3,
		"moderate" = 2,
		"weak" = 1,
		"none" = 0
	)
	criteria$biological <- list(
		name = "Biological plausibility",
		score = bio_score,
		assessment = biological_rationale,
		weight = 1
	)

	# Criterion 5: Is the subgroup effect consistent in direction with overall?
	effect_score <- switch(
		effect_measure,
		"consistent" = 3,
		"uncertain" = 1,
		"opposite" = 0
	)
	criteria$consistent <- list(
		name = "Consistent with overall effect",
		score = effect_score,
		assessment = effect_measure,
		weight = 1
	)

	# Criterion 6: Is this a within-study comparison?
	criteria$within_study <- list(
		name = "Within-study comparison",
		score = if (within_study) 3 else 1,
		assessment = if (within_study) "Yes" else "No (across-study)",
		weight = 0.5
	)

	# Criterion 7: Was a formal statistical test performed?
	test_score <- switch(
		statistical_test,
		"formal" = 3,
		"informal" = 1,
		"none" = 0
	)
	criteria$statistical <- list(
		name = "Formal interaction test",
		score = test_score,
		assessment = statistical_test,
		weight = 1
	)

	# Criterion 8: Is the interaction p-value convincing? (p < 0.005 suggested)
	if (!is.na(interaction_pvalue)) {
		p_score <- if (interaction_pvalue < 0.005) {
			3
		} else if (interaction_pvalue < 0.05) {
			2
		} else {
			0
		}
		p_assess <- sprintf("p = %s", format_pvalue(interaction_pvalue))
	} else {
		p_score <- NA
		p_assess <- "Not tested"
	}
	criteria$pvalue <- list(
		name = "Interaction p-value",
		score = p_score,
		assessment = p_assess,
		weight = 1
	)

	# Criterion 9: Has the finding been replicated?
	rep_score <- switch(replication, "yes" = 3, "no" = 0, "not_applicable" = NA)
	criteria$replication <- list(
		name = "Replication",
		score = rep_score,
		assessment = replication,
		weight = 1
	)

	# Criterion 10: Is there other supporting evidence?
	other_score <- switch(
		other_evidence,
		"strong" = 3,
		"moderate" = 2,
		"weak" = 1,
		"none" = 0
	)
	criteria$other <- list(
		name = "Other supporting evidence",
		score = other_score,
		assessment = other_evidence,
		weight = 0.5
	)

	# Calculate overall credibility
	scores <- sapply(criteria, function(x) x$score)
	weights <- sapply(criteria, function(x) x$weight)
	valid_idx <- !is.na(scores)

	weighted_sum <- sum(scores[valid_idx] * weights[valid_idx])
	max_possible <- sum(3 * weights[valid_idx])

	# Determine credibility level
	if (is.na(max_possible) || max_possible == 0) {
		overall_score <- NA_real_
		credibility <- "Not assessable"
	} else {
		overall_score <- weighted_sum / max_possible
		credibility <- if (overall_score >= 0.75) {
			"High"
		} else if (overall_score >= 0.50) {
			"Moderate"
		} else if (overall_score >= 0.25) {
			"Low"
		} else {
			"Very Low"
		}
	}

	# Create summary table
	summary_df <- data.frame(
		Criterion = sapply(criteria, function(x) x$name),
		Assessment = sapply(criteria, function(x) x$assessment),
		Score = sapply(criteria, function(x) {
			if (is.na(x$score)) "N/A" else as.character(x$score)
		}),
		stringsAsFactors = FALSE
	)

	list(
		criteria = criteria,
		summary = summary_df,
		overall_score = overall_score,
		credibility = credibility,
		interpretation = if (is.na(overall_score)) {
			sprintf("Subgroup credibility: %s", credibility)
		} else {
			sprintf(
				"Subgroup credibility: %s (score: %.1f%%)",
				credibility,
				overall_score * 100
			)
		}
	)
}
