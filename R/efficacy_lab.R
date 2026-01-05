#' @title Lab Summary Tables
#' @name efficacy_lab
#' @description Functions for laboratory parameter summaries and shift tables.
NULL

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
	assert_data_frame(adlb, "adlb")
	assert_data_frame(trt_n, "trt_n")

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
	assert_data_frame(adlb, "adlb")
	assert_data_frame(trt_n, "trt_n")

	required_cols <- c("PARAMCD", "BNRIND", "ANRIND", "AVISIT", trt_var)
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
