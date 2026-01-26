#' @title Primary Endpoint Tables
#' @name efficacy_primary
#' @description Functions for creating primary endpoint summary tables.
NULL

#' Create Primary Endpoint Summary Table
#'
#' @param data ADVS data frame
#' @param paramcd Parameter code to analyze (default: "SYSBP")
#' @param visit Visit to analyze (default: "End of Treatment")
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @examples
#' # Create primary endpoint summary
#' advs <- data.frame(
#'   USUBJID = c("01", "02", "03", "04"),
#'   TRT01P = c("Placebo", "Placebo", "Active", "Active"),
#'   PARAMCD = rep("SYSBP", 4),
#'   AVISIT = rep("End of Treatment", 4),
#'   AVAL = c(120, 125, 118, 122)
#' )
#' table <- create_primary_endpoint_table(advs, paramcd = "SYSBP")
#' table@type
#' @export
create_primary_endpoint_table <- function(
	data,
	paramcd = "SYSBP",
	visit = "End of Treatment",
	trt_var = ph_default("trt_var"),
	title = "Primary Endpoint Summary",
	autofit = ph_default("autofit")
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
			"'data' data frame is missing required columns.\n",
			"  Missing: ",
			paste(missing_cols, collapse = ", "),
			"\n",
			"  Required: ",
			paste(required_cols, collapse = ", "),
			"\n",
			"  Available: ",
			paste(names(df), collapse = ", ")
		)
	}

	# Filter and summarize
	primary_data <- df |>
		dplyr::filter(
			.data$PARAMCD == paramcd,
			.data$AVISIT == visit
		) |>
		dplyr::summarise(
			n = sum(!is.na(.data$AVAL)),
			Mean = dplyr::if_else(n == 0, NA_real_, mean(.data$AVAL, na.rm = TRUE)),
			SD = dplyr::if_else(n == 0, NA_real_, sd(.data$AVAL, na.rm = TRUE)),
			Median = dplyr::if_else(
				n == 0,
				NA_real_,
				median(.data$AVAL, na.rm = TRUE)
			),
			Min = dplyr::if_else(n == 0, NA_real_, min(.data$AVAL, na.rm = TRUE)),
			Max = dplyr::if_else(n == 0, NA_real_, max(.data$AVAL, na.rm = TRUE)),
			.by = dplyr::all_of(trt_var)
		) |>
		dplyr::mutate(
			Mean = round(.data$Mean, 1),
			SD = round(.data$SD, 2),
			Median = round(.data$Median, 1),
			Min = round(.data$Min, 1),
			Max = round(.data$Max, 1),
			`Mean (SD)` = dplyr::if_else(
				is.na(.data$Mean) |
					is.nan(.data$Mean) |
					is.infinite(.data$Mean) |
					is.na(.data$SD) |
					is.nan(.data$SD) |
					is.infinite(.data$SD),
				"-",
				paste0(.data$Mean, " (", .data$SD, ")")
			),
			`Min, Max` = dplyr::if_else(
				is.na(.data$Min) |
					is.nan(.data$Min) |
					is.infinite(.data$Min) |
					is.na(.data$Max) |
					is.nan(.data$Max) |
					is.infinite(.data$Max),
				"-",
				paste0(.data$Min, ", ", .data$Max)
			),
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

	# Use factory function
	create_clinical_table(
		data = primary_wide,
		type = "primary_endpoint",
		title = title,
		footnotes = c(
			paste(data@population, "Population"),
			paste("Parameter:", paramcd, "at", visit)
		),
		metadata = list(param = paramcd, visit = visit),
		autofit = autofit
	)
}
