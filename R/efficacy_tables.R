#' Efficacy Analysis Tables
#'
#' Functions for efficacy tables: endpoints, change from baseline,
#' lab summaries.
#' @name efficacy_tables
#' @keywords internal
NULL

#' Create Primary Endpoint Summary Table
#'
#' @param advs ADVS data frame
#' @param trt_n Treatment group counts
#' @param paramcd Parameter code to analyze (default: "SYSBP")
#' @param visit Visit to analyze (default: "End of Treatment")
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_primary_endpoint_table <- function(
	advs,
	trt_n,
	paramcd = "SYSBP",
	visit = "End of Treatment",
	trt_var = "TRT01P",
	title = "Primary Endpoint Summary",
	autofit = TRUE
) {
	assert_data_frame(advs, "advs")
	assert_data_frame(trt_n, "trt_n")

	# Filter and summarize
	primary_data <- advs |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			.data$AVISIT == visit
		) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
		dplyr::summarise(
			n = dplyr::n(),
			Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
			SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
			Median = round(median(.data$AVAL, na.rm = TRUE), 1),
			Min = round(min(.data$AVAL, na.rm = TRUE), 1),
			Max = round(max(.data$AVAL, na.rm = TRUE), 1),
			.groups = "drop"
		) |>
		dplyr::mutate(
			`Mean (SD)` = paste0(.data$Mean, " (", .data$SD, ")"),
			`Min, Max` = paste0(.data$Min, ", ", .data$Max),
			n = as.character(.data$n),
			Median = as.character(.data$Median)
		) |>
		dplyr::select(
			dplyr::all_of(trt_var),
			"n",
			"Mean (SD)",
			"Median",
			"Min, Max"
		)

	# Transpose
	primary_wide <- primary_data |>
		tidyr::pivot_longer(
			cols = c("n", "Mean (SD)", "Median", "Min, Max"),
			names_to = "Statistic"
		) |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "value"
		)

	# Styling
	primary_ft <- create_hta_table(
		primary_wide,
		title = title,
		footnotes = c(
			"Safety Population",
			paste(paramcd, "=", paramcd, "(analyzed at", visit, ")")
		),
		autofit = autofit
	)

	ClinicalTable(
		data = primary_wide,
		flextable = primary_ft,
		type = "primary_endpoint",
		title = title,
		metadata = list(param = paramcd, visit = visit)
	)
}

#' Create Change from Baseline Summary Table
#'
#' Generates a summary table showing change from baseline statistics for vital
#' signs or other continuous parameters. The table displays mean change,
#' standard deviation, and other descriptive statistics by treatment group for a
#' analysis visit.
#'
#' @param advs An ADaM ADVS (Analysis Data Vital Signs) data frame. Required
#'   columns include: USUBJID, PARAMCD (parameter code), PARAM (parameter name),
#'   AVISIT (analysis visit), CHG (change from baseline), and the treatment
#'   variable (typically TRT01P).
#' @param trt_n A data frame or named vector containing treatment group counts.
#'   If a data frame, should contain treatment variable column and N column.
#'   Used for displaying "N=X" in column headers.
#' @param params Character vector of PARAMCD values identifying which vital sign
#'   parameters to include in the table (e.g., c("SYSBP", "DIABP", "PULSE")).
#'   Must match values in the PARAMCD column of advs.
#' @param visit Character string specifying the analysis visit to summarize.
#'   Must match a value in the AVISIT column of advs (e.g., "End of Treatment",
#'   "Week 12").
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Character string for the table title.
#' @param autofit Logical, whether to automatically adjust column widths to fit
#'   content. Default is TRUE.
#'
#' @return A ClinicalTable S7 object with the formatted change-from-baseline
#'   summary statistics table. The object includes the underlying data frame,
#'   a formatted flextable for rendering, and metadata about the analysis.
#'
#' @export
create_cfb_summary_table <- function(
	advs,
	trt_n,
	params = c("SYSBP", "DIABP", "PULSE"),
	visit = "End of Treatment",
	trt_var = "TRT01P",
	title = "Change from Baseline Summary",
	autofit = TRUE
) {
	assert_data_frame(advs, "advs")
	assert_data_frame(trt_n, "trt_n")

	cfb_data <- advs |>
		dplyr::filter(
			.data$PARAMCD %in% params,
			!is.na(.data$CHG),
			.data$AVISIT == visit
		) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$PARAM) |>
		dplyr::summarise(
			n = dplyr::n(),
			Mean_CFB = round(mean(.data$CHG, na.rm = TRUE), 2),
			SD_CFB = round(sd(.data$CHG, na.rm = TRUE), 2),
			.groups = "drop"
		) |>
		dplyr::mutate(
			display = paste0(.data$Mean_CFB, " (", .data$SD_CFB, ")"),
			n = as.character(.data$n)
		) |>
		dplyr::select("PARAM", dplyr::all_of(trt_var), "n", "display")

	cfb_wide <- cfb_data |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = c("n", "display"),
			names_glue = paste0("{", trt_var, "}_{.value}")
		)

	# Rename columns for display
	names(cfb_wide) <- gsub("_display$", " Mean (SD)", names(cfb_wide))
	names(cfb_wide) <- gsub("_n$", " n", names(cfb_wide))
	names(cfb_wide) <- gsub("^PARAM$", "Parameter", names(cfb_wide))

	cfb_ft <- create_hta_table(
		cfb_wide,
		title = title,
		footnotes = c(
			"Safety Population",
			"Mean (SD) presented for change from baseline"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = cfb_wide,
		flextable = cfb_ft,
		type = "cfb",
		title = title
	)
}

#' Create Vital Signs by Visit Table
#'
#' @param advs ADVS data frame
#' @param trt_n Treatment group counts
#' @param paramcd Parameter code to analyze
#' @param visits Vector of visits to include
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_vs_by_visit_table <- function(
	advs,
	trt_n,
	paramcd = "SYSBP",
	visits = c("Baseline", "Week 2", "Week 4", "Week 8", "End of Treatment"),
	trt_var = "TRT01P",
	title = "Vital Signs by Visit",
	autofit = TRUE
) {
	assert_data_frame(advs, "advs")
	assert_data_frame(trt_n, "trt_n")

	required_cols <- c("PARAMCD", "AVISIT", trt_var, "AVAL")
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

	vs_by_visit <- advs |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			.data$AVISIT %in% visits
		) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$AVISIT) |>
		dplyr::summarise(
			n = dplyr::n(),
			Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
			SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
			.groups = "drop"
		) |>
		dplyr::mutate(
			display = paste0(.data$n, " / ", .data$Mean, " (", .data$SD, ")")
		) |>
		dplyr::select("AVISIT", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "--"
		) |>
		dplyr::rename(Visit = "AVISIT")

	# Order visits
	vs_by_visit <- vs_by_visit |>
		dplyr::mutate(
			Visit = factor(.data$Visit, levels = visits)
		) |>
		dplyr::arrange(.data$Visit) |>
		dplyr::mutate(Visit = as.character(.data$Visit))

	vs_ft <- create_hta_table(
		vs_by_visit,
		title = title,
		footnotes = c(
			"Safety Population",
			"Format: n / Mean (SD)"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = vs_by_visit,
		flextable = vs_ft,
		type = "vs_by_visit",
		title = title
	)
}

#' Create Laboratory Summary Table
#'
#' @param adlb ADLB data frame
#' @param trt_n Treatment group counts
#' @param params Vector of parameter codes to analyze
#' @param visit Visit to analyze
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_lab_summary_table <- function(
	adlb,
	trt_n,
	params = c("HGB", "WBC", "PLAT", "ALT", "AST", "BILI", "CREAT"),
	visit = "Week 24",
	trt_var = "TRT01P",
	title = "Laboratory Parameters Summary",
	autofit = TRUE
) {
	# Input validation
	if (!is.data.frame(adlb)) {
		ph_abort("'adlb' must be a data frame")
	}
	if (!is.data.frame(trt_n)) {
		ph_abort("'trt_n' must be a data frame")
	}

	required_cols <- c("PARAMCD", "AVISIT", trt_var, "PARAM", "AVAL")
	missing_cols <- setdiff(required_cols, names(adlb))
	if (length(missing_cols) > 0) {
		ph_abort(
			sprintf(
				"'adlb' is missing required columns: %s. Required: %s",
				paste(missing_cols, collapse = ", "),
				paste(required_cols, collapse = ", ")
			)
		)
	}

	lab_data <- adlb |>
		dplyr::filter(
			.data$PARAMCD %in% params,
			.data$AVISIT == visit
		) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$PARAM) |>
		dplyr::summarise(
			n = dplyr::n(),
			Mean = round(mean(.data$AVAL, na.rm = TRUE), 2),
			SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
			.groups = "drop"
		) |>
		dplyr::mutate(
			display = paste0(.data$Mean, " (", .data$SD, ")"),
			n = as.character(.data$n)
		) |>
		dplyr::select("PARAM", dplyr::all_of(trt_var), "n", "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = c("n", "display"),
			names_glue = paste0("{", trt_var, "}_{.value}")
		)

	names(lab_data) <- gsub("_display$", " Mean (SD)", names(lab_data))
	names(lab_data) <- gsub("_n$", " n", names(lab_data))
	names(lab_data) <- gsub("^PARAM$", "Parameter", names(lab_data))

	lab_ft <- create_hta_table(
		lab_data,
		title = title,
		footnotes = c("Safety Population", "Mean (SD) presented"),
		autofit = autofit
	)

	ClinicalTable(
		data = lab_data,
		flextable = lab_ft,
		type = "lab_summary",
		title = title
	)
}

#' Create Laboratory Shift Table
#'
#' @param adlb ADLB data frame
#' @param trt_n Treatment group counts
#' @param paramcd Parameter code to analyze
#' @param visit Visit to analyze
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_lab_shift_table <- function(
	adlb,
	trt_n,
	paramcd = "ALT",
	visit = "Week 24",
	trt_var = "TRT01P",
	title = "Laboratory Shift Table",
	autofit = TRUE
) {
	# Input validation
	if (!is.data.frame(adlb)) {
		ph_abort("'adlb' must be a data frame")
	}
	if (!is.data.frame(trt_n)) {
		ph_abort("'trt_n' must be a data frame")
	}

	shift_data <- adlb |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			!is.na(.data$BNRIND),
			!is.na(.data$ANRIND),
			.data$AVISIT == visit
		) |>
		dplyr::group_by(
			dplyr::across(dplyr::all_of(trt_var)),
			.data$BNRIND,
			.data$ANRIND
		) |>
		dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
		tidyr::pivot_wider(
			names_from = "ANRIND",
			values_from = "n",
			values_fill = 0
		) |>
		dplyr::rename(`Baseline Status` = "BNRIND")

	shift_wide <- shift_data |>
		dplyr::mutate(Treatment = !!rlang::sym(trt_var)) |>
		dplyr::select(
			"Treatment",
			"Baseline Status",
			dplyr::everything(),
			-dplyr::all_of(trt_var)
		)

	shift_ft <- create_hta_table(
		shift_wide,
		title = title,
		footnotes = c(
			"Safety Population",
			"Shift from baseline normal range indicator"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = shift_wide,
		flextable = shift_ft,
		type = "lab_shift",
		title = title
	)
}

#' Create Subgroup Analysis Table
#'
#' @param adsl ADSL data frame
#' @param advs ADVS data frame
#' @param paramcd Parameter code to analyze
#' @param visit Visit to analyze
#' @param subgroups List of subgroup variables (e.g. list(AGEGR1="Age Group"))
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
	trt_var = "TRT01P",
	title = "Subgroup Analysis",
	autofit = TRUE
) {
	# Input validation
	if (!is.data.frame(adsl)) {
		ph_abort("'adsl' must be a data frame")
	}
	if (!is.data.frame(advs)) {
		ph_abort("'advs' must be a data frame")
	}

	required_cols <- c("PARAMCD", "AVISIT", trt_var, "AVAL")
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

	subgroup_data_raw <- advs |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			.data$AVISIT == visit
		)

	results_list <- list()

	# Iterate through subgroups
	for (var_name in names(subgroups)) {
		label <- subgroups[[var_name]]

		# Check if subgroup variable exists in data
		assert_column_exists(subgroup_data_raw, var_name, "subgroup_data_raw")

		res <- subgroup_data_raw |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(trt_var)),
				.data[[var_name]]
			) |>
			dplyr::summarise(
				n = dplyr::n(),
				Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
				SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
				.groups = "drop"
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
		title = title
	)
}

#' Create Time-to-Event Summary Table
#'
#' Generates a standard TTE summary table with median survival, confidence
#' intervals, hazard ratios, and optional landmark estimates for efficacy
#' endpoints like OS, PFS, EFS.
#'
#' @param data ADaMData object or data frame containing TTE data
#' @param time_var Time variable name (default: "AVAL")
#' @param event_var Event indicator variable. If "CNSR" (ADaM censoring flag),
#'   it will be automatically inverted (0=event becomes 1=event).
#'   Otherwise expects 1=event, 0=censor. Default: "CNSR"
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param ref_group Reference group for HR calculation. If NULL, uses first
#'   level of treatment variable.
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param landmarks Numeric vector of timepoints for landmark survival
#'   estimates (e.g., c(12, 24) for 12 and 24 month rates). NULL for none.
#' @param time_unit Character string for time unit display (default: "months")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable object with TTE summary statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' tte_table <- create_tte_summary_table(
#'   data = adtte,
#'   time_var = "AVAL",
#'   event_var = "CNSR",
#'   title = "Overall Survival Summary"
#' )
#'
#' # With landmark estimates
#' tte_table <- create_tte_summary_table(
#'   data = adtte,
#'   landmarks = c(12, 24),
#'   time_unit = "months",
#'   title = "Progression-Free Survival"
#' )
#' }
create_tte_summary_table <- function(
	data,
	time_var = "AVAL",
	event_var = "CNSR",
	trt_var = "TRT01P",
	ref_group = NULL,
	conf_level = 0.95,
	landmarks = NULL,
	time_unit = "months",
	title = "Time-to-Event Summary",
	autofit = TRUE
) {
	# Get filtered data and treatment variable
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

	# Handle CNSR inversion (ADaM: 0=event, 1=censor -> survival: 1=event)
	if (event_var == "CNSR") {
		df$event <- 1 - df[[event_var]]
	} else {
		df$event <- df[[event_var]]
	}

	# Ensure treatment is factor for consistent ordering
	df[[trt_var_actual]] <- as.factor(df[[trt_var_actual]])
	trt_levels <- levels(df[[trt_var_actual]])

	if (is.null(ref_group)) {
		ref_group <- trt_levels[1]
	}

	# Build survival formula
	surv_formula <- stats::as.formula(
		paste0("survival::Surv(", time_var, ", event) ~ ", trt_var_actual)
	)

	# Fit Kaplan-Meier
	km_fit <- survival::survfit(surv_formula, data = df, conf.int = conf_level)

	# Extract median and CI from survfit
	km_summary <- summary(km_fit)$table
	if (is.null(dim(km_summary))) {
		# Single treatment arm
		km_summary <- matrix(km_summary, nrow = 1)
		rownames(km_summary) <- trt_levels[1]
	}

	# Build results data frame
	results <- data.frame(
		Treatment = gsub(paste0(trt_var_actual, "="), "", rownames(km_summary)),
		stringsAsFactors = FALSE
	)

	# N and events
	results$N <- as.integer(km_summary[, "records"])
	results$Events <- as.integer(km_summary[, "events"])
	results$Events_pct <- sprintf(
		"%.1f",
		results$Events / results$N * 100
	)
	results$`Events n (%)` <- paste0(
		results$Events,
		" (",
		results$Events_pct,
		")"
	)

	# Median survival with CI
	median_col <- if ("median" %in% colnames(km_summary)) {
		"median"
	} else {
		"*rmean"
	}
	ci_lower_name <- paste0(
		"0.",
		gsub("0\\.", "", as.character(conf_level)),
		"LCL"
	)
	ci_upper_name <- paste0(
		"0.",
		gsub("0\\.", "", as.character(conf_level)),
		"UCL"
	)

	# Handle different column naming conventions
	if (!ci_lower_name %in% colnames(km_summary)) {
		ci_lower_name <- grep("LCL$", colnames(km_summary), value = TRUE)[1]
		ci_upper_name <- grep("UCL$", colnames(km_summary), value = TRUE)[1]
	}

	results$Median <- sprintf("%.1f", km_summary[, median_col])
	results$Median_LCL <- sprintf("%.1f", km_summary[, ci_lower_name])
	results$Median_UCL <- sprintf("%.1f", km_summary[, ci_upper_name])
	results$`Median (95% CI)` <- ifelse(
		is.na(km_summary[, median_col]),
		"NE",
		paste0(
			results$Median,
			" (",
			results$Median_LCL,
			", ",
			results$Median_UCL,
			")"
		)
	)

	# Landmark estimates if requested
	if (!is.null(landmarks) && length(landmarks) > 0) {
		landmark_summary <- summary(km_fit, times = landmarks, extend = TRUE)

		for (i in seq_along(landmarks)) {
			time_pt <- landmarks[i]
			col_name <- paste0(time_pt, "-", time_unit, " Rate (95% CI)")

			# Extract survival estimates for this timepoint
			if (length(trt_levels) > 1) {
				idx <- which(landmark_summary$time == time_pt)
				surv_vals <- landmark_summary$surv[idx]
				lower_vals <- landmark_summary$lower[idx]
				upper_vals <- landmark_summary$upper[idx]
			} else {
				surv_vals <- landmark_summary$surv[i]
				lower_vals <- landmark_summary$lower[i]
				upper_vals <- landmark_summary$upper[i]
			}

			results[[col_name]] <- sprintf(
				"%.1f (%.1f, %.1f)",
				surv_vals * 100,
				lower_vals * 100,
				upper_vals * 100
			)
		}
	}

	# Hazard Ratio from Cox model (only if >1 treatment)
	if (length(trt_levels) > 1) {
		# Relevel to set reference group
		df[[trt_var_actual]] <- stats::relevel(
			df[[trt_var_actual]],
			ref = ref_group
		)

		cox_fit <- survival::coxph(surv_formula, data = df)
		cox_summary <- summary(cox_fit)

		# Extract HR and CI
		hr_table <- cox_summary$conf.int
		hr_coefs <- cox_summary$coefficients

		results$`HR (95% CI)` <- NA_character_
		results$`p-value` <- NA_character_

		# Reference group gets "Reference"
		ref_idx <- which(results$Treatment == ref_group)
		results$`HR (95% CI)`[ref_idx] <- "Reference"
		results$`p-value`[ref_idx] <- "-"

		# Other groups get HR values
		for (j in seq_len(nrow(hr_table))) {
			trt_name <- gsub(trt_var_actual, "", rownames(hr_table)[j])
			trt_idx <- which(results$Treatment == trt_name)

			if (length(trt_idx) > 0) {
				results$`HR (95% CI)`[trt_idx] <- sprintf(
					"%.2f (%.2f, %.2f)",
					hr_table[j, "exp(coef)"],
					hr_table[j, "lower .95"],
					hr_table[j, "upper .95"]
				)
				results$`p-value`[trt_idx] <- format_pvalue(
					hr_coefs[j, "Pr(>|z|)"]
				)
			}
		}
	}

	# Select columns for display
	display_cols <- c(
		"Treatment",
		"N",
		"Events n (%)",
		"Median (95% CI)"
	)

	# Add landmark columns if present
	if (!is.null(landmarks)) {
		landmark_cols <- paste0(landmarks, "-", time_unit, " Rate (95% CI)")
		display_cols <- c(display_cols, landmark_cols)
	}

	# Add HR columns if present
	if ("HR (95% CI)" %in% names(results)) {
		display_cols <- c(display_cols, "HR (95% CI)", "p-value")
	}

	display_df <- results[, display_cols, drop = FALSE]

	# Convert N to character for consistent pivoting
	display_df$N <- as.character(display_df$N)

	# Transpose for display (rows = statistics, columns = treatments)
	display_transposed <- display_df |>
		tidyr::pivot_longer(
			cols = -"Treatment",
			names_to = "Statistic",
			values_to = "Value"
		) |>
		tidyr::pivot_wider(
			names_from = "Treatment",
			values_from = "Value"
		)

	# Create flextable
	ft <- create_hta_table(
		display_transposed,
		title = title,
		footnotes = c(
			paste("Time unit:", time_unit),
			if (length(trt_levels) > 1) {
				paste("HR reference group:", ref_group)
			} else {
				NULL
			},
			"NE = Not Estimable"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = display_transposed,
		flextable = ft,
		type = "tte_summary",
		title = title,
		metadata = list(
			km_fit = km_fit,
			cox_fit = if (exists("cox_fit")) cox_fit else NULL,
			ref_group = ref_group,
			landmarks = landmarks,
			time_unit = time_unit
		)
	)
}

#' Create Responder Summary Table
#'
#' Generates a response rate table with confidence intervals and
#' treatment comparisons (odds ratio, risk ratio, or risk difference)
#' for binary endpoints like ORR, CR, PR.
#'
#' @param data ADaMData object or data frame
#' @param response_var Variable containing response values
#' @param response_values Character vector of values indicating response
#'   (default: c("CR", "PR") for tumor response)
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param ref_group Reference group for comparison. If NULL, uses first
#'   level of treatment variable.
#' @param ci_method Method for CI calculation: "wilson" (recommended),
#'   "exact" (Clopper-Pearson), or "wald"
#' @param comparison_type "OR" for odds ratio, "RR" for risk ratio,
#'   "RD" for risk difference
#' @param conf_level Confidence level (default: 0.95)
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable object with response summary
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic tumor response table
#' resp_table <- create_responder_table(
#'   data = adrs,
#'   response_var = "AVALC",
#'   response_values = c("CR", "PR"),
#'   title = "Best Overall Response"
#' )
#'
#' # With risk ratio instead of odds ratio
#' resp_table <- create_responder_table(
#'   data = adrs,
#'   comparison_type = "RR",
#'   title = "Response Rate Summary"
#' )
#' }
create_responder_table <- function(
	data,
	response_var = "AVALC",
	response_values = c("CR", "PR"),
	trt_var = "TRT01P",
	ref_group = NULL,
	ci_method = c("wilson", "exact", "wald"),
	comparison_type = c("OR", "RR", "RD"),
	conf_level = 0.95,
	title = "Response Rate Summary",
	autofit = TRUE
) {
	ci_method <- match.arg(ci_method)
	comparison_type <- match.arg(comparison_type)

	# Get filtered data
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

	# Define responder
	df$responder <- as.integer(df[[response_var]] %in% response_values)

	# Ensure treatment is factor
	df[[trt_var_actual]] <- as.factor(df[[trt_var_actual]])
	trt_levels <- levels(df[[trt_var_actual]])

	if (is.null(ref_group)) {
		ref_group <- trt_levels[1]
	}

	# Response rate by treatment
	response_summary <- df |>
		dplyr::group_by(!!rlang::sym(trt_var_actual)) |>
		dplyr::summarise(
			N = dplyr::n(),
			responders = sum(.data$responder, na.rm = TRUE),
			rate = .data$responders / .data$N,
			.groups = "drop"
		)

	# Add confidence intervals
	response_summary$ci_lower <- NA_real_
	response_summary$ci_upper <- NA_real_

	for (i in seq_len(nrow(response_summary))) {
		ci <- calculate_proportion_ci(
			response_summary$responders[i],
			response_summary$N[i],
			method = ci_method,
			conf_level = conf_level
		)
		response_summary$ci_lower[i] <- ci$lower
		response_summary$ci_upper[i] <- ci$upper
	}

	# Format for display
	response_summary$`n/N` <- paste0(
		response_summary$responders,
		"/",
		response_summary$N
	)
	response_summary$`Rate (%)` <- sprintf("%.1f", response_summary$rate * 100)
	response_summary$`95% CI` <- sprintf(
		"(%.1f, %.1f)",
		response_summary$ci_lower * 100,
		response_summary$ci_upper * 100
	)

	# Calculate treatment comparison (only if >1 treatment)
	if (length(trt_levels) > 1) {
		comparison <- calculate_response_comparison(
			df = df,
			trt_var = trt_var_actual,
			ref_group = ref_group,
			comparison_type = comparison_type,
			conf_level = conf_level
		)

		response_summary[[paste0(comparison_type, " (95% CI)")]] <- NA_character_
		response_summary$`p-value` <- NA_character_

		# Reference group
		ref_idx <- which(response_summary[[trt_var_actual]] == ref_group)
		response_summary[[paste0(comparison_type, " (95% CI)")]][ref_idx] <-
			"Reference"
		response_summary$`p-value`[ref_idx] <- "-"

		# Other groups
		for (trt in names(comparison)) {
			trt_idx <- which(response_summary[[trt_var_actual]] == trt)
			if (length(trt_idx) > 0) {
				comp <- comparison[[trt]]
				if (comparison_type == "RD") {
					response_summary[[paste0(comparison_type, " (95% CI)")]][trt_idx] <-
						sprintf(
							"%.1f (%.1f, %.1f)",
							comp$estimate * 100,
							comp$lcl * 100,
							comp$ucl * 100
						)
				} else {
					response_summary[[paste0(comparison_type, " (95% CI)")]][trt_idx] <-
						sprintf(
							"%.2f (%.2f, %.2f)",
							comp$estimate,
							comp$lcl,
							comp$ucl
						)
				}
				response_summary$`p-value`[trt_idx] <- format_pvalue(comp$pvalue)
			}
		}
	}

	# Select and rename columns for display
	display_cols <- c(trt_var_actual, "n/N", "Rate (%)", "95% CI")
	if (paste0(comparison_type, " (95% CI)") %in% names(response_summary)) {
		display_cols <- c(
			display_cols,
			paste0(comparison_type, " (95% CI)"),
			"p-value"
		)
	}

	display_df <- response_summary[, display_cols, drop = FALSE]
	names(display_df)[1] <- "Treatment"

	# Create flextable
	ft <- create_hta_table(
		display_df,
		title = title,
		footnotes = c(
			paste("Response defined as:", paste(response_values, collapse = ", ")),
			paste("95% CI calculated using", ci_method, "method"),
			if (length(trt_levels) > 1) {
				paste(comparison_type, "compared to", ref_group)
			} else {
				NULL
			}
		),
		autofit = autofit
	)

	ClinicalTable(
		data = display_df,
		flextable = ft,
		type = "responder",
		title = title,
		metadata = list(
			response_values = response_values,
			ci_method = ci_method,
			comparison_type = comparison_type,
			ref_group = ref_group
		)
	)
}

#' Calculate Proportion Confidence Interval
#'
#' @param x Number of successes
#' @param n Total number of trials
#' @param method CI method: "wilson", "exact", or "wald"
#' @param conf_level Confidence level
#'
#' @return List with lower and upper bounds
#' @keywords internal
calculate_proportion_ci <- function(
	x,
	n,
	method = "wilson",
	conf_level = 0.95
) {
	alpha <- 1 - conf_level
	p <- x / n

	if (method == "wilson") {
		# Wilson score interval
		z <- stats::qnorm(1 - alpha / 2)
		denom <- 1 + z^2 / n
		center <- (p + z^2 / (2 * n)) / denom
		margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
		list(lower = max(0, center - margin), upper = min(1, center + margin))
	} else if (method == "exact") {
		# Clopper-Pearson exact
		result <- stats::binom.test(x, n, conf.level = conf_level)
		list(lower = result$conf.int[1], upper = result$conf.int[2])
	} else {
		# Wald interval
		se <- sqrt(p * (1 - p) / n)
		z <- stats::qnorm(1 - alpha / 2)
		list(lower = max(0, p - z * se), upper = min(1, p + z * se))
	}
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

	# Get filtered data
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

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

	# Calculate estimates for each subgroup
	for (var_name in names(subgroups)) {
		label <- subgroups[[var_name]]

		if (!var_name %in% names(df)) {
			ph_warn(sprintf("Subgroup variable '%s' not found, skipping", var_name))
			next
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

	# Combine results
	results_df <- do.call(rbind, results_list)

	# Apply multiplicity adjustment to interaction p-values if requested
	if (show_interaction && adjust_method != "none") {
		# Extract unique interaction p-values (one per subgroup variable)
		# These are in rows where interaction_p is not NA
		pval_rows <- which(!is.na(results_df$interaction_p))

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
				cox_summary <- summary(cox_fit)

				result$estimate <- cox_summary$conf.int[1, "exp(coef)"]
				result$lcl <- cox_summary$conf.int[1, "lower .95"]
				result$ucl <- cox_summary$conf.int[1, "upper .95"]
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

#' Calculate Response Comparison (OR, RR, or RD)
#'
#' @param df Data frame with responder column
#' @param trt_var Treatment variable name
#' @param ref_group Reference group name
#' @param comparison_type "OR", "RR", or "RD"
#' @param conf_level Confidence level
#'
#' @return Named list of comparison results per treatment group
#' @keywords internal
calculate_response_comparison <- function(
	df,
	trt_var,
	ref_group,
	comparison_type,
	conf_level
) {
	alpha <- 1 - conf_level
	z <- stats::qnorm(1 - alpha / 2)

	trt_levels <- levels(df[[trt_var]])
	other_trts <- setdiff(trt_levels, ref_group)

	# Reference group stats
	ref_data <- df[df[[trt_var]] == ref_group, ]
	n_ref <- nrow(ref_data)
	x_ref <- sum(ref_data$responder)
	p_ref <- x_ref / n_ref

	results <- list()

	for (trt in other_trts) {
		trt_data <- df[df[[trt_var]] == trt, ]
		n_trt <- nrow(trt_data)
		x_trt <- sum(trt_data$responder)
		p_trt <- x_trt / n_trt

		if (comparison_type == "OR") {
			# Odds Ratio via logistic regression
			df_subset <- df[df[[trt_var]] %in% c(ref_group, trt), ]
			df_subset[[trt_var]] <- stats::relevel(
				factor(df_subset[[trt_var]]),
				ref = ref_group
			)

			glm_fit <- stats::glm(
				stats::reformulate(trt_var, response = "responder"),
				data = df_subset,
				family = stats::binomial()
			)
			coef_idx <- 2
			or <- exp(stats::coef(glm_fit)[coef_idx])
			or_ci <- exp(stats::confint.default(glm_fit, level = conf_level)[
				coef_idx,
			])
			pval <- summary(glm_fit)$coefficients[coef_idx, "Pr(>|z|)"]

			results[[trt]] <- list(
				estimate = or,
				lcl = or_ci[1],
				ucl = or_ci[2],
				pvalue = pval
			)
		} else if (comparison_type == "RR") {
			# Risk Ratio (approximate)
			# Check for extreme rates that require continuity correction
			extreme_rates <- p_ref == 0 || p_ref == 1 || p_trt == 0 || p_trt == 1
			continuity_applied <- FALSE

			if (extreme_rates) {
				# Apply Haldane continuity correction: add 0.5 to counts
				x_trt_adj <- x_trt + 0.5
				x_ref_adj <- x_ref + 0.5
				n_trt_adj <- n_trt + 1
				n_ref_adj <- n_ref + 1
				p_trt_calc <- x_trt_adj / n_trt_adj
				p_ref_calc <- x_ref_adj / n_ref_adj
				continuity_applied <- TRUE
			} else {
				p_trt_calc <- p_trt
				p_ref_calc <- p_ref
			}

			rr <- p_trt_calc / p_ref_calc
			log_rr <- log(rr)
			se_log_rr <- sqrt(
				(1 - p_trt_calc) /
					(n_trt * p_trt_calc) +
					(1 - p_ref_calc) / (n_ref * p_ref_calc)
			)
			rr_ci <- exp(log_rr + c(-1, 1) * z * se_log_rr)

			# Chi-square test for p-value (use Fisher's exact for extreme cases)
			cont_table <- matrix(
				c(x_trt, n_trt - x_trt, x_ref, n_ref - x_ref),
				nrow = 2
			)
			if (extreme_rates || any(cont_table < 5)) {
				pval <- stats::fisher.test(cont_table)$p.value
			} else {
				pval <- stats::chisq.test(cont_table)$p.value
			}

			results[[trt]] <- list(
				estimate = rr,
				lcl = rr_ci[1],
				ucl = rr_ci[2],
				pvalue = pval,
				continuity_correction = continuity_applied
			)
		} else {
			# Risk Difference
			rd <- p_trt - p_ref
			se_rd <- sqrt(p_trt * (1 - p_trt) / n_trt + p_ref * (1 - p_ref) / n_ref)
			rd_ci <- rd + c(-1, 1) * z * se_rd

			# Z-test for p-value
			z_stat <- rd / se_rd
			pval <- 2 * stats::pnorm(-abs(z_stat))

			results[[trt]] <- list(
				estimate = rd,
				lcl = rd_ci[1],
				ucl = rd_ci[2],
				pvalue = pval
			)
		}
	}

	results
}
