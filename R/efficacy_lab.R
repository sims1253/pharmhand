#' @title Lab Summary Tables
#' @name efficacy_lab
#' @description Functions for laboratory parameter summaries and shift tables.
NULL

#' Create Laboratory Summary Table
#'
#' @param data ADLB data frame or ADaMData object
#' @param params Vector of parameter codes to analyze
#' @param visit Visit to analyze
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @examples
#' # Create lab summary table
#' adlb <- data.frame(
#'   USUBJID = c("01", "02", "03", "04"),
#'   TRT01P = c("Placebo", "Placebo", "Active", "Active"),
#'   PARAMCD = rep("HGB", 4),
#'   PARAM = rep("Hemoglobin", 4),
#'   AVISIT = rep("Week 24", 4),
#'   AVAL = c(14.2, 13.8, 14.5, 14.1)
#' )
#' table <- create_lab_summary_table(adlb, params = "HGB", visit = "Week 24")
#' table@type
#' @export
create_lab_summary_table <- function(
	data,
	params = c("HGB", "WBC", "PLAT", "ALT", "AST", "BILI", "CREAT"),
	visit = "Week 24",
	trt_var = "TRT01P",
	title = "Laboratory Parameters Summary",
	autofit = TRUE
) {
	# Ensure ADaMData object with proper domain, passing trt_var for data frames
	data <- .ensure_adam_data(data, domain = "ADLB", trt_var = trt_var)
	df <- get_filtered_data(data)
	# Use trt_var from ADaMData object (set during coercion)
	trt_var_actual <- data@trt_var

	required_cols <- c("PARAMCD", "AVISIT", trt_var_actual, "PARAM", "AVAL")
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

	lab_data <- df |>
		dplyr::filter(
			.data$PARAMCD %in% params,
			.data$AVISIT == visit
		) |>
		dplyr::summarise(
			n = dplyr::n(),
			Mean = round(mean(.data$AVAL, na.rm = TRUE), 2),
			SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
			.by = c(dplyr::all_of(trt_var_actual), "PARAM")
		) |>
		dplyr::mutate(
			display = paste0(.data$Mean, " (", .data$SD, ")"),
			n = as.character(.data$n)
		) |>
		dplyr::select("PARAM", dplyr::all_of(trt_var_actual), "n", "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var_actual),
			values_from = c("n", "display"),
			names_glue = paste0("{", trt_var_actual, "}_{.value}")
		)

	names(lab_data) <- gsub("_display$", " Mean (SD)", names(lab_data))
	names(lab_data) <- gsub("_n$", " n", names(lab_data))
	names(lab_data) <- gsub("^PARAM$", "Parameter", names(lab_data))

	create_clinical_table(
		data = lab_data,
		type = "lab_summary",
		title = title,
		footnotes = c("Safety Population", "Mean (SD) presented"),
		autofit = autofit
	)
}

#' Create Laboratory Shift Table
#'
#' @param data ADLB data frame or ADaMData object
#' @param paramcd Parameter code to analyze
#' @param visit Visit to analyze
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @examples
#' # Create lab shift table
#' adlb <- data.frame(
#'   USUBJID = c("01", "02", "03", "04"),
#'   TRT01P = c("Placebo", "Placebo", "Active", "Active"),
#'   PARAMCD = rep("ALT", 4),
#'   BNRIND = c("Normal", "Normal", "High", "Normal"),
#'   ANRIND = c("Normal", "High", "High", "Normal"),
#'   AVISIT = rep("Week 24", 4)
#' )
#' table <- create_lab_shift_table(adlb, paramcd = "ALT", visit = "Week 24")
#' table@type
#' @export
create_lab_shift_table <- function(
	data,
	paramcd = "ALT",
	visit = "Week 24",
	trt_var = "TRT01P",
	title = "Laboratory Shift Table",
	autofit = TRUE
) {
	# Ensure ADaMData object with proper domain, passing trt_var for data frames
	data <- .ensure_adam_data(data, domain = "ADLB", trt_var = trt_var)
	df <- get_filtered_data(data)
	# Use trt_var from ADaMData object (set during coercion)
	trt_var_actual <- data@trt_var

	required_cols <- c("PARAMCD", "BNRIND", "ANRIND", "AVISIT", trt_var_actual)
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

	shift_data <- df |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			!is.na(.data$BNRIND),
			!is.na(.data$ANRIND),
			.data$AVISIT == visit
		) |>
		dplyr::summarise(
			n = dplyr::n(),
			.by = c(dplyr::all_of(trt_var_actual), "BNRIND", "ANRIND")
		) |>
		tidyr::pivot_wider(
			names_from = "ANRIND",
			values_from = "n",
			values_fill = 0
		) |>
		dplyr::rename(`Baseline Status` = "BNRIND")

	shift_wide <- shift_data |>
		dplyr::mutate(Treatment = !!rlang::sym(trt_var_actual)) |>
		dplyr::select(
			"Treatment",
			"Baseline Status",
			dplyr::everything(),
			-dplyr::all_of(trt_var_actual)
		)

	create_clinical_table(
		data = shift_wide,
		type = "lab_shift",
		title = title,
		footnotes = c(
			"Safety Population",
			"Shift from baseline normal range indicator"
		),
		autofit = autofit
	)
}
