#' Standard Clinical Tables
#'
#' Functions for standard clinical trial tables.
#'
#' @name standard_tables
#' @keywords internal
NULL

#' Create Module 4 Table Template
#'
#' Standardized G-BA Module 4 table with fixed column structure.
#'
#' @param data Data frame to display. If NULL, an empty template is created.
#'   Can also be an ADaMData object.
#' @param title Table title (default: "Module 4 Summary")
#' @param footnotes Character vector of footnotes
#' @param columns Character vector of required column names in display order.
#'   Missing columns are added with `NA`.
#' @param col_widths Named numeric vector of column widths (optional)
#' @param allow_extra Logical, allow extra columns beyond `columns`. When FALSE,
#'   extra columns are dropped; when TRUE, they are appended after `columns`.
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' table <- create_hta_module4_table()
#' }
create_hta_module4_table <- function(
	data = NULL,
	title = "Module 4 Summary",
	footnotes = character(),
	columns = c(
		"Endpoint",
		"Analysis Set",
		"Treatment",
		"Comparator",
		"Effect",
		"95% CI",
		"p-value"
	),
	col_widths = NULL,
	allow_extra = FALSE,
	autofit = TRUE
) {
	if (!is.null(data)) {
		if (S7::S7_inherits(data, ADaMData)) {
			data <- data@data
		}
	} else {
		data <- data.frame(matrix(nrow = 0, ncol = length(columns)))
		names(data) <- columns
	}

	if (length(columns) == 0) {
		ph_abort("'columns' must contain at least one column name")
	}

	missing_cols <- setdiff(columns, names(data))
	if (length(missing_cols) > 0) {
		for (col in missing_cols) {
			data[[col]] <- NA
		}
	}

	extra_cols <- setdiff(names(data), columns)
	if (length(extra_cols) > 0) {
		if (allow_extra) {
			data <- data[, c(columns, extra_cols), drop = FALSE]
		} else {
			ph_warn(paste(
				"Dropping extra columns:",
				paste(extra_cols, collapse = ", ")
			))
			data <- data[, columns, drop = FALSE]
		}
	} else {
		data <- data[, columns, drop = FALSE]
	}

	ct <- create_clinical_table(
		data = data,
		type = "module4",
		title = title,
		footnotes = footnotes,
		theme = "gba",
		autofit = autofit,
		metadata = list(
			columns = columns,
			allow_extra = allow_extra
		)
	)

	# Apply custom column widths if provided (theme_gba doesn't accept col_widths
	# via ...)
	if (!is.null(col_widths)) {
		ft <- ct@flextable
		for (col_name in names(col_widths)) {
			if (col_name %in% names(data)) {
				ft <- ft |>
					flextable::width(
						j = col_name,
						width = col_widths[[col_name]]
					)
			}
		}
		ct@flextable <- ft
	}

	ct
}

#' Create Demographics Table
#'
#' Standard demographics and baseline characteristics table.
#'
#' @param data ADaMData object or data.frame containing ADSL
#' @param title Table title (default: "Demographics and Baseline")
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param age_var Age variable name (default: "AGE")
#' @param age_grp_var Age group variable name (default: "AGEGR1")
#' @param sex_var Sex variable name (default: "SEX")
#' @param race_var Race variable name (default: "RACE")
#' @param ethnic_var Ethnicity variable name (default: "ETHNIC")
#' @param country_var Country variable name (default: "COUNTRY")
#' @param autofit Logical, perform expensive layout calculations (default: TRUE)
#'
#' @return A ClinicalTable object
#' @export
create_demographics_table <- function(
	data,
	title = "Demographics and Baseline Characteristics",
	trt_var = ph_default("trt_var"),
	age_var = "AGE",
	age_grp_var = "AGEGR1",
	sex_var = "SEX",
	race_var = "RACE",
	ethnic_var = "ETHNIC",
	country_var = "COUNTRY",
	autofit = ph_default("autofit")
) {
	# Ensure ADaMData for demographic/population tables, passing trt_var for
	# data frames
	data <- .ensure_adam_data(data, "ADSL", trt_var = trt_var)
	adsl <- data@data

	# Use LayeredTable for comprehensive demographics
	demo_table <- LayeredTable(
		data = adsl,
		trt_var = data@trt_var,
		title = title
	)

	# Helper to safely add layer
	add_if_exists <- function(tbl, var, type = "count", label = var) {
		if (var %in% names(adsl)) {
			if (type == "descriptive") {
				tbl <- tbl |>
					add_layer(DescriptiveLayer(
						target_var = var,
						label = label
					))
			} else {
				tbl <- tbl |>
					add_layer(CountLayer(
						target_var = var,
						label = label
					))
			}
		}
		tbl
	}

	demo_table <- demo_table |>
		add_if_exists(age_var, "descriptive", "Age (years)") |>
		add_if_exists(age_grp_var, "count", "Age Group") |>
		add_if_exists(sex_var, "count", "Sex") |>
		add_if_exists(race_var, "count", "Race") |>
		add_if_exists(ethnic_var, "count", "Ethnicity") |>
		add_if_exists(country_var, "count", "Country")

	# Generate footnotes based on what was included
	footnotes <- c(
		paste(data@population, "Population"),
		"Age summarized as n, Mean (SD), Median, Min-Max",
		"Categorical variables presented as n (%)"
	)

	create_clinical_table(
		data = demo_table,
		type = "demographics",
		title = title,
		footnotes = footnotes,
		metadata = list(
			population = data@population,
			n_subjects = nrow(adsl)
		),
		autofit = autofit
	)
}

#' Create Enrollment by Region Table
#'
#' @param data ADSL data frame or ADaMData object
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param region_var Region variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_region_table <- function(
	data,
	title = "Enrollment by Region",
	trt_var = "TRT01P",
	region_var = "REGION1",
	autofit = TRUE
) {
	# Ensure ADaMData for demographic/population tables, passing trt_var for
	# data frames
	data <- .ensure_adam_data(data, "ADSL", trt_var = trt_var)
	adsl <- data@data

	region_table <- LayeredTable(
		data = adsl,
		trt_var = data@trt_var,
		title = title
	)

	region_table <- region_table |>
		add_layer(CountLayer(
			target_var = region_var,
			label = "Region"
		))

	create_clinical_table(
		data = region_table,
		type = "enrollment",
		title = title,
		footnotes = c(
			paste(data@population, "Population"),
			"n (%) = Number (percentage) of subjects"
		),
		autofit = autofit
	)
}

#' Create Medical History Table
#'
#' @param data ADMH data frame or ADaMData object
#' @param adsl ADSL data frame or ADaMData object for denominators
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param soc_var SOC variable name for MH
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_medical_history_table <- function(
	data,
	adsl,
	title = "Medical History by Body System",
	trt_var = "TRT01P",
	soc_var = "MHBODSYS",
	autofit = TRUE
) {
	# Ensure ADaMData objects, passing trt_var for data frames
	adsl <- .ensure_adam_data(adsl, "ADSL", trt_var = trt_var)

	if (S7::S7_inherits(data, ADaMData)) {
		admh <- data@data
	} else {
		admh <- data
	}

	# Use summary_fn pattern for category summary
	summary_fn <- function(admh_data) {
		trt_n <- adsl@data |>
			dplyr::summarise(N = dplyr::n(), .by = !!rlang::sym(adsl@trt_var))

		admh_data |>
			dplyr::summarise(
				n_subj = dplyr::n_distinct(.data$USUBJID),
				.by = c(!!rlang::sym(adsl@trt_var), !!rlang::sym(soc_var))
			) |>
			dplyr::left_join(trt_n, by = adsl@trt_var) |>
			dplyr::mutate(
				pct = round(.data$n_subj / .data$N * 100, 1),
				display = paste0(.data$n_subj, " (", .data$pct, "%)")
			) |>
			dplyr::select(
				!!rlang::sym(soc_var),
				!!rlang::sym(adsl@trt_var),
				"display"
			) |>
			tidyr::pivot_wider(
				names_from = !!rlang::sym(adsl@trt_var),
				values_from = "display",
				values_fill = "0 (0.0%)"
			) |>
			dplyr::rename(`Body System` = !!rlang::sym(soc_var)) |>
			dplyr::arrange(.data$`Body System`)
	}

	create_clinical_table(
		data = admh,
		type = "medical_history",
		title = title,
		footnotes = c(
			paste(adsl@population, "Population"),
			"n (%) = Number (percentage) of subjects with at least one condition"
		),
		summary_fn = summary_fn,
		autofit = autofit
	)
}

#' Create Concomitant Medications Table
#'
#' @param data ADCM data frame or ADaMData object
#' @param adsl ADSL data frame or ADaMData object for denominators
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param class_var ATC class/category variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_conmeds_table <- function(
	data,
	adsl,
	title = "Prior and Concomitant Medications by Class",
	trt_var = "TRT01P",
	class_var = "CMCLAS",
	autofit = TRUE
) {
	# Ensure ADaMData objects, passing trt_var for data frames
	adsl <- .ensure_adam_data(adsl, "ADSL", trt_var = trt_var)

	if (S7::S7_inherits(data, ADaMData)) {
		adcm <- data@data
	} else {
		adcm <- data
	}

	# Use summary_fn pattern for category summary
	summary_fn <- function(adcm_data) {
		trt_n <- adsl@data |>
			dplyr::summarise(N = dplyr::n(), .by = !!rlang::sym(adsl@trt_var))

		adcm_data |>
			dplyr::summarise(
				n_subj = dplyr::n_distinct(.data$USUBJID),
				.by = c(!!dplyr::sym(adsl@trt_var), !!dplyr::sym(class_var))
			) |>
			dplyr::left_join(trt_n, by = adsl@trt_var) |>
			dplyr::mutate(
				pct = round(.data$n_subj / .data$N * 100, 1),
				display = paste0(.data$n_subj, " (", .data$pct, "%)")
			) |>
			dplyr::select(
				!!rlang::sym(class_var),
				!!rlang::sym(adsl@trt_var),
				"display"
			) |>
			tidyr::pivot_wider(
				names_from = !!rlang::sym(adsl@trt_var),
				values_from = "display",
				values_fill = "0 (0.0%)"
			) |>
			dplyr::rename(`Medication Class` = !!dplyr::sym(class_var)) |>
			dplyr::arrange(.data$`Medication Class`)
	}

	create_clinical_table(
		data = adcm,
		type = "conmeds",
		title = title,
		footnotes = c(
			paste(adsl@population, "Population"),
			"n (%) = Number (percentage) of subjects taking at least one medication"
		),
		summary_fn = summary_fn,
		autofit = autofit
	)
}

#' Create Subject Disposition Table
#'
#' @param data ADSL data frame or ADaMData object
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param status_var End of study status variable (e.g., EOSSTT)
#' @param reason_var Discontinuation reason variable (e.g., DCSREAS)
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_disposition_table <- function(
	data,
	title = "Subject Disposition",
	trt_var = "TRT01P",
	status_var = "EOSSTT",
	reason_var = "DCSREAS",
	autofit = TRUE
) {
	# Ensure ADaMData, passing trt_var for data frames
	data <- .ensure_adam_data(data, "ADSL", trt_var = trt_var)
	adsl <- data@data

	summary_fn <- function(adsl_data) {
		disp_data <- adsl_data |>
			dplyr::summarise(
				n = dplyr::n(),
				.by = c(!!dplyr::sym(data@trt_var), !!dplyr::sym(status_var))
			) |>
			tidyr::pivot_wider(
				names_from = !!dplyr::sym(data@trt_var),
				values_from = "n",
				values_fill = 0
			) |>
			dplyr::rename(`Study Status` = !!dplyr::sym(status_var))

		if (reason_var %in% names(adsl_data)) {
			disc_reasons <- adsl_data |>
				dplyr::filter(toupper(!!rlang::sym(status_var)) == "DISCONTINUED") |>
				dplyr::summarise(
					n = dplyr::n(),
					.by = c(!!dplyr::sym(data@trt_var), !!dplyr::sym(reason_var))
				) |>
				tidyr::pivot_wider(
					names_from = !!dplyr::sym(data@trt_var),
					values_from = "n",
					values_fill = 0
				) |>
				dplyr::rename(`Study Status` = !!dplyr::sym(reason_var)) |>
				dplyr::mutate(`Study Status` = paste0("  ", .data$`Study Status`))

			disp_data <- dplyr::bind_rows(disp_data, disc_reasons)
		}

		disp_data
	}

	create_clinical_table(
		data = adsl,
		type = "disposition",
		title = title,
		footnotes = paste(data@population, "Population"),
		summary_fn = summary_fn,
		autofit = autofit
	)
}

#' Create Analysis Populations Summary Table
#'
#' @param data ADSL data frame or ADaMData object
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param pop_flags Character vector of population flag variables
#' @param pop_labels Character vector of labels for population flags
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_population_summary_table <- function(
	data,
	title = "Analysis Populations",
	trt_var = "TRT01P",
	pop_flags = c("SAFFL"),
	pop_labels = c("Safety"),
	autofit = TRUE
) {
	# Ensure ADaMData, passing trt_var for data frames
	data <- .ensure_adam_data(data, "ADSL", trt_var = trt_var)
	adsl <- data@data

	if (length(pop_flags) != length(pop_labels)) {
		ph_abort("pop_flags and pop_labels must have the same length")
	}

	summary_fn <- function(adsl_data) {
		existing_vars <- pop_flags[pop_flags %in% names(adsl_data)]
		existing_labels <- pop_labels[pop_flags %in% names(adsl_data)]

		pop_summary <- data.frame(
			Population = character()
		)

		for (i in seq_along(existing_vars)) {
			var <- existing_vars[i]
			label <- existing_labels[i]

			counts <- adsl_data |>
				dplyr::filter(.data[[var]] == "Y") |>
				dplyr::summarise(n = dplyr::n(), .by = !!rlang::sym(data@trt_var)) |>
				tidyr::pivot_wider(
					names_from = !!rlang::sym(data@trt_var),
					values_from = "n",
					values_fill = 0
				)

			counts$Population <- label
			pop_summary <- dplyr::bind_rows(pop_summary, counts)
		}

		if (nrow(pop_summary) > 0) {
			pop_summary <- pop_summary |>
				dplyr::select("Population", dplyr::everything())
		}

		pop_summary
	}

	create_clinical_table(
		data = adsl,
		type = "populations",
		title = title,
		footnotes = c(
			"ITT = Intent-To-Treat Population",
			"Safety = Safety Population (subjects who received study drug)"
		),
		summary_fn = summary_fn,
		autofit = autofit
	)
}
