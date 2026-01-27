#' @title Change from Baseline Tables
#' @name efficacy_cfb
#' @description Functions for change from baseline summaries and
#'   vital signs tables.
NULL

#' Internal helper function to compute change from baseline summary statistics.
#'
#' @param data Filtered data frame
#' @param trt_var Treatment variable name
#'
#' @return Data frame with CFB summary statistics
#' @noRd
.summarize_cfb <- function(data, trt_var) {
	data |>
		dplyr::summarise(
			n = dplyr::n(),
			Mean_CFB = round(mean(.data$CHG, na.rm = TRUE), 2),
			SD_CFB = round(sd(.data$CHG, na.rm = TRUE), 2),
			.by = dplyr::all_of(c(trt_var, "PARAM"))
		) |>
		dplyr::mutate(
			display = paste0(.data$Mean_CFB, " (", .data$SD_CFB, ")"),
			n = as.character(.data$n)
		)
}

#' Internal helper function to pivot and format CFB summary data for display.
#'
#' @param cfb_data Summary data from .summarize_cfb
#' @param trt_var Treatment variable name
#'
#' @return Formatted data frame ready for flextable
#' @noRd
.format_cfb_table <- function(cfb_data, trt_var) {
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

	cfb_wide
}

#' Create Change from Baseline Summary Table
#'
#' Generates a summary table showing change from baseline statistics for vital
#' signs or other continuous parameters. The table displays mean change,
#' standard deviation, and other descriptive statistics
#'   by treatment group for an analysis visit.
#'
#' @param data An ADaMData object (with domain "ADVS") or an ADVS data frame.
#'   Required columns include: PARAMCD (parameter code), PARAM (parameter name),
#'   AVISIT (analysis visit), CHG (change from baseline), and the treatment
#'   variable (default: "TRT01P" for data frames, or @trt_var for ADaMData).
#' @param params Character vector of PARAMCD values identifying which vital sign
#'   parameters to include in the table (e.g., c("SYSBP", "DIABP", "PULSE")).
#'   Must match values in the PARAMCD column of data.
#' @param visit Character string specifying the analysis visit to summarize.
#'   Must match a value in the AVISIT column of data (e.g., "End of Treatment",
#'   "Week 12").
#' @param trt_var Treatment variable name (default: "TRT01P"). Ignored for
#'   ADaMData objects which use their own trt_var property.
#' @param title Character string for the table title.
#' @param footnotes Character vector of footnotes to append to the table.
#' @param autofit Logical, whether to automatically adjust column widths to fit
#'   content. Default is TRUE.
#' @param ... Additional arguments passed to \code{\link{create_clinical_table}}
#'
#' @return A ClinicalTable S7 object with the formatted change-from-baseline
#'   summary statistics table. The object includes the underlying data frame,
#'   a formatted flextable for rendering, and metadata about the analysis.
#'
#' @examples
#' # Create change from baseline summary
#' advs <- data.frame(
#'   USUBJID = rep(c("01", "02", "03", "04"), each = 1),
#'   TRT01P = rep(c("Placebo", "Placebo", "Active", "Active"), each = 1),
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
	data,
	params = c("SYSBP", "DIABP", "PULSE"),
	visit = "End of Treatment",
	trt_var = "TRT01P",
	title = "Change from Baseline Summary",
	footnotes = character(),
	autofit = TRUE,
	...
) {
	# Ensure ADaMData object with proper domain, passing trt_var for data frames
	data <- .ensure_adam_data(data, "ADVS", trt_var = trt_var)

	# Use trt_var from ADaMData object (set during coercion)
	trt_var <- data@trt_var

	# Use filtered_data (respects population filter)
	df <- data@filtered_data

	# Validate required columns
	required_cols <- c("PARAMCD", "PARAM", "AVISIT", trt_var, "CHG")
	missing_cols <- setdiff(required_cols, names(df))
	if (length(missing_cols) > 0) {
		ph_abort(
			sprintf(
				"'data' is missing required columns: %s. Required: %s",
				paste(missing_cols, collapse = ", "),
				paste(required_cols, collapse = ", ")
			)
		)
	}

	# Filter and summarize CFB data
	cfb_data <- df |>
		dplyr::filter(
			.data$PARAMCD %in% params,
			!is.na(.data$CHG),
			.data$AVISIT == visit
		) |>
		.summarize_cfb(trt_var = trt_var) |>
		dplyr::select("PARAM", dplyr::all_of(trt_var), "n", "display")

	# Warn if no data found
	if (nrow(cfb_data) == 0) {
		ph_warn(
			sprintf(
				"No data found for create_cfb_summary_table: PARAMCD=%s, AVISIT='%s'",
				paste(params, collapse = ", "),
				visit
			)
		)
	}

	# Format for display
	cfb_wide <- .format_cfb_table(cfb_data, trt_var)

	# Create footnotes
	default_footnotes <- c(
		paste(data@population, "Population"),
		"Mean (SD) presented for change from baseline"
	)
	all_footnotes <- c(footnotes, default_footnotes)

	# Use factory function
	create_clinical_table(
		data = cfb_wide,
		type = "cfb",
		title = title,
		footnotes = all_footnotes,
		autofit = autofit,
		...
	)
}

#' Internal helper function to compute vital signs summary by visit.
#'
#' @param data Filtered data frame
#' @param trt_var Treatment variable name
#'
#' @return Data frame with VS summary statistics
#' @noRd
.summarize_vs_by_visit <- function(data, trt_var) {
	data |>
		dplyr::summarise(
			n = sum(!is.na(.data$AVAL)),
			Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
			SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
			.by = dplyr::all_of(c(trt_var, "AVISIT"))
		) |>
		dplyr::mutate(
			display = paste0(.data$n, " / ", .data$Mean, " (", .data$SD, ")")
		)
}

#' Internal helper function to pivot and format VS by visit data for display.
#'
#' @param vs_data Summary data from .summarize_vs_by_visit
#' @param trt_var Treatment variable name
#' @param visits Vector of visits for ordering
#'
#' @return Formatted data frame ready for flextable
#' @noRd
.format_vs_by_visit_table <- function(vs_data, trt_var, visits) {
	vs_wide <- vs_data |>
		dplyr::select("AVISIT", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "--"
		) |>
		dplyr::rename(Visit = "AVISIT")

	# Order visits
	vs_wide |>
		dplyr::mutate(
			Visit = factor(.data$Visit, levels = visits)
		) |>
		dplyr::arrange(.data$Visit) |>
		dplyr::mutate(Visit = as.character(.data$Visit))
}

#' Create Vital Signs by Visit Table
#'
#' Generates a table displaying vital signs measurements across multiple visits,
#' showing sample size, mean, and standard deviation by treatment group.
#'
#' @param data An ADaMData object (with domain "ADVS") or an ADVS data frame.
#'   Required columns include: PARAMCD, AVISIT, AVAL, and the treatment
#'   variable.
#' @param paramcd Parameter code to analyze (e.g., "SYSBP", "DIABP").
#' @param visits Vector of visits to include in the table (e.g., c("Baseline",
#'   "Week 2", "Week 4", "Week 8", "End of Treatment")).
#' @param trt_var Treatment variable name (default: "TRT01P"). Ignored for
#'   ADaMData objects which use their own trt_var property.
#' @param title Table title.
#' @param footnotes Character vector of footnotes to append to the table.
#' @param autofit Logical, whether to autofit column widths (default: TRUE).
#' @param ... Additional arguments passed to \code{\link{create_clinical_table}}
#'
#' @return A ClinicalTable S7 object with the formatted vital signs by visit
#'   table.
#'
#' @examples
#' # Create vital signs by visit table
#' advs <- data.frame(
#'   USUBJID = rep(c("01", "02", "03", "04"), each = 2),
#'   TRT01P = rep(c("Placebo", "Placebo", "Active", "Active"), each = 2),
#'   PARAMCD = rep("SYSBP", 8),
#'   AVISIT = c("Baseline", "Week 2", "Baseline", "Week 2",
#'              "Baseline", "Week 2", "Baseline", "Week 2"),
#'   AVAL = c(120, 118, 125, 122, 118, 112, 122, 115)
#' )
#' table <- create_vs_by_visit_table(advs, paramcd = "SYSBP")
#' table@type
#'
#' @export
create_vs_by_visit_table <- function(
	data,
	paramcd = "SYSBP",
	visits = c("Baseline", "Week 2", "Week 4", "Week 8", "End of Treatment"),
	trt_var = "TRT01P",
	title = "Vital Signs by Visit",
	footnotes = character(),
	autofit = TRUE,
	...
) {
	# Ensure ADaMData object with proper domain, passing trt_var for data frames
	data <- .ensure_adam_data(data, "ADVS", trt_var = trt_var)

	# Use trt_var from ADaMData object (set during coercion)
	trt_var <- data@trt_var

	# Use filtered_data (respects population filter)
	df <- data@filtered_data

	# Validate required columns
	required_cols <- c("PARAMCD", "AVISIT", trt_var, "AVAL")
	missing_cols <- setdiff(required_cols, names(df))
	if (length(missing_cols) > 0) {
		ph_abort(
			sprintf(
				"'data' is missing required columns: %s. Required: %s",
				paste(missing_cols, collapse = ", "),
				paste(required_cols, collapse = ", ")
			)
		)
	}

	# Filter and summarize VS data
	vs_data <- df |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			.data$AVISIT %in% visits
		) |>
		.summarize_vs_by_visit(trt_var = trt_var)

	# Warn if no data found
	if (nrow(vs_data) == 0) {
		ph_warn(
			sprintf(
				"No data found for create_vs_by_visit_table: PARAMCD='%s', visits=%s",
				paramcd,
				paste(visits, collapse = ", ")
			)
		)
	}

	# Format for display
	vs_wide <- .format_vs_by_visit_table(vs_data, trt_var, visits)

	# Create footnotes
	default_footnotes <- c(
		paste(data@population, "Population"),
		"Format: n / Mean (SD)"
	)
	all_footnotes <- c(footnotes, default_footnotes)

	# Use factory function
	create_clinical_table(
		data = vs_wide,
		type = "vs_by_visit",
		title = title,
		footnotes = all_footnotes,
		autofit = autofit,
		...
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
	assert_data_frame(data, arg = "data")
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

	# Summarise with or without grouping
	if (is.null(group_vars)) {
		summary_result <- score_data |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data[[subject_var]]),
				# Count distinct subjects at floor/ceiling (not observations)
				n_floor = dplyr::n_distinct(
					.data[[subject_var]][.data[[score_var]] == min_score]
				),
				n_ceiling = dplyr::n_distinct(
					.data[[subject_var]][.data[[score_var]] == max_score]
				)
			)
	} else {
		summary_result <- score_data |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data[[subject_var]]),
				# Count distinct subjects at floor/ceiling (not observations)
				n_floor = dplyr::n_distinct(
					.data[[subject_var]][.data[[score_var]] == min_score]
				),
				n_ceiling = dplyr::n_distinct(
					.data[[subject_var]][.data[[score_var]] == max_score]
				),
				.by = dplyr::all_of(group_vars)
			)
	}

	summary_result |>
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
