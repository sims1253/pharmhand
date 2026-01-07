#' @title Primary Endpoint Tables
#' @name efficacy_primary
#' @description Functions for creating primary endpoint summary tables.
NULL

#' Create Primary Endpoint Summary Table
#'
#' @param advs ADVS data frame
#' @param paramcd Parameter code to analyze (default: "SYSBP")
#' @param visit Visit to analyze (default: "End of Treatment")
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_primary_endpoint_table <- function(
	advs,
	paramcd = "SYSBP",
	visit = "End of Treatment",
	trt_var = "TRT01P",
	title = "Primary Endpoint Summary",
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

	# Filter and summarize
	primary_data <- advs |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			.data$AVISIT == visit
		) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
		dplyr::summarise(
			n = sum(!is.na(.data$AVAL)),
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
			paste("Parameter:", paramcd, "at", visit)
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
