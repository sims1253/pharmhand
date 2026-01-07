#' @title Change from Baseline Tables
#' @name efficacy_cfb
#' @description Functions for change from baseline summaries and
#'   vital signs tables.
NULL

#' Create Change from Baseline Summary Table
#'
#' Generates a summary table showing change from baseline statistics for vital
#' signs or other continuous parameters. The table displays mean change,
#' standard deviation, and other descriptive statistics by treatment group for an
#' analysis visit.
#'
#' @param advs An ADaM ADVS (Analysis Data Vital Signs) data frame. Required
#'   columns include: USUBJID, PARAMCD (parameter code), PARAM (parameter name),
#'   AVISIT (analysis visit), CHG (change from baseline), and the treatment
#'   variable (typically TRT01P).
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
#' @examples
#' # Create change from baseline summary
#' advs <- data.frame(
#'   USUBJID = c("01", "02", "03", "04"),
#'   TRT01P = c("Placebo", "Placebo", "Active", "Active"),
#'   PARAMCD = rep("SYSBP", 4),
#'   PARAM = rep("Systolic Blood Pressure", 4),
#'   AVISIT = rep("End of Treatment", 4),
#'   CHG = c(-2.5, -3.1, -8.2, -7.5)
#' )
#' table <- create_cfb_summary_table(advs, params = "SYSBP")
#' table@type
#'
#' @export
create_cfb_summary_table <- function(
	advs,
	params = c("SYSBP", "DIABP", "PULSE"),
	visit = "End of Treatment",
	trt_var = "TRT01P",
	title = "Change from Baseline Summary",
	autofit = TRUE
) {
	assert_data_frame(advs, "advs")

	required_cols <- c("PARAMCD", "AVISIT", trt_var, "PARAM", "CHG")
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

#' Detect Floor and Ceiling Effects
#'
#' Identifies floor/ceiling effects in PRO data when >15% of responses
#' are at the minimum or maximum possible scale values.
#'
#' @param data Data frame containing PRO scores
#' @param score_var Character. Name of the score variable
#' @param min_score Numeric. Minimum possible score on the scale
#' @param max_score Numeric. Maximum possible score on the scale
#' @param by_var Character vector. Variables to group by (e.g.,
#'   c("VISIT", "TRT01P"))
#' @param threshold Numeric. Threshold for flagging (default: 0.15 = 15%)
#' @param subject_var Character. Subject identifier (default: "USUBJID")
#'
#' @return A data frame with columns:
#'   - Grouping variables
#'   - n: number of subjects with non-missing scores
#'   - n_floor: count of subjects at minimum
#'   - pct_floor: percentage at minimum
#'   - floor_flag: TRUE if pct_floor > threshold
#'   - n_ceiling: count of subjects at maximum
#'   - pct_ceiling: percentage at maximum
#'   - ceiling_flag: TRUE if pct_ceiling > threshold
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Detect floor/ceiling effects in PRO scores
#' result <- detect_floor_ceiling(
#'   data = adqs,
#'   score_var = "AVAL",
#'   min_score = 0,
#'   max_score = 100
#' )
#' print(result)
#' }
detect_floor_ceiling <- function(
	data,
	score_var,
	min_score,
	max_score,
	by_var = NULL,
	threshold = 0.15,
	subject_var = "USUBJID"
) {
	assert_data_frame(data, "data")
	assert_character_scalar(score_var, "score_var")
	assert_numeric_scalar(min_score, "min_score")
	assert_numeric_scalar(max_score, "max_score")
	assert_numeric_scalar(threshold, "threshold")
	assert_character_scalar(subject_var, "subject_var")
	assert_in_range(threshold, 0, 1, "threshold")

	if (min_score >= max_score) {
		ph_abort("'min_score' must be less than 'max_score'")
	}

	assert_column_exists(data, score_var, "data")
	assert_column_exists(data, subject_var, "data")

	group_vars <- NULL
	if (!is.null(by_var)) {
		if (!is.character(by_var) || length(by_var) == 0) {
			ph_abort("'by_var' must be a non-empty character vector or NULL")
		}

		missing_cols <- setdiff(by_var, names(data))
		if (length(missing_cols) > 0) {
			ph_abort(sprintf(
				"Column(s) not found in 'data': %s",
				paste(missing_cols, collapse = ", ")
			))
		}

		group_vars <- by_var
	}

	score_data <- data |>
		dplyr::select(dplyr::all_of(c(group_vars, subject_var, score_var))) |>
		dplyr::filter(!is.na(.data[[score_var]]))

	grouped <- if (is.null(group_vars)) {
		score_data
	} else {
		score_data |>
			dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))
	}

	grouped |>
		dplyr::summarise(
			n = dplyr::n_distinct(.data[[subject_var]]),
			# Count distinct subjects at floor/ceiling (not observations)
			n_floor = dplyr::n_distinct(
				.data[[subject_var]][.data[[score_var]] == min_score]
			),
			n_ceiling = dplyr::n_distinct(
				.data[[subject_var]][.data[[score_var]] == max_score]
			),
			.groups = "drop"
		) |>
		dplyr::mutate(
			pct_floor = ifelse(.data$n > 0, .data$n_floor / .data$n, NA_real_),
			floor_flag = .data$n > 0 & .data$pct_floor > threshold,
			pct_ceiling = ifelse(
				.data$n > 0,
				.data$n_ceiling / .data$n,
				NA_real_
			),
			ceiling_flag = .data$n > 0 & .data$pct_ceiling > threshold
		)
}

#' Create Vital Signs by Visit Table
#'
#' @param advs ADVS data frame
#' @param paramcd Parameter code to analyze
#' @param visits Vector of visits to include
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_vs_by_visit_table <- function(
	advs,
	paramcd = "SYSBP",
	visits = c("Baseline", "Week 2", "Week 4", "Week 8", "End of Treatment"),
	trt_var = "TRT01P",
	title = "Vital Signs by Visit",
	autofit = TRUE
) {
	assert_data_frame(advs, "advs")

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
