#' Standard Clinical Tables
#'
#' Reusable functions for creating standard clinical trial tables.
#'
#' @name standard_tables
#' @keywords internal
NULL

#' Create Demographics Table
#'
#' Generates a standard demographics and baseline characteristics table.
#'
#' @param adsl_data ADaMData object containing ADSL
#' @param title Table title (default: "Demographics and Baseline Characteristics")
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param age_var Age variable name (default: "AGE")
#' @param age_grp_var Age group variable name (default: "AGEGR1")
#' @param sex_var Sex variable name (default: "SEX")
#' @param race_var Race variable name (default: "RACE")
#' @param ethnic_var Ethnicity variable name (default: "ETHNIC")
#' @param country_var Country variable name (default: "COUNTRY")
#' @param autofit Logical, whether to perform expensive layout calculations (default: TRUE)
#'
#' @return A ClinicalTable object
#' @export
create_demographics_table <- function(
	adsl_data,
	title = "Demographics and Baseline Characteristics",
	trt_var = "TRT01P",
	age_var = "AGE",
	age_grp_var = "AGEGR1",
	sex_var = "SEX",
	race_var = "RACE",
	ethnic_var = "ETHNIC",
	country_var = "COUNTRY",
	autofit = TRUE
) {
	if (!S7::S7_inherits(adsl_data, ADaMData)) {
		cli::cli_abort("{.arg adsl_data} must be an ADaMData object")
	}

	adsl <- adsl_data@data

	# Use LayeredTable for comprehensive demographics
	demo_table <- LayeredTable(
		data = adsl,
		trt_var = trt_var,
		title = title
	)

	# Add layers for each demographic variable if they exist in dataset
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

	# Build and convert to flextable
	demo_data <- build_table(demo_table)

	# Generate footnotes based on what was included
	footnotes <- c(
		paste(adsl_data@population, "Population"),
		"Age summarized as n, Mean (SD), Median, Min-Max",
		"Categorical variables presented as n (%)"
	)

	demo_ft <- create_hta_table(
		demo_data,
		title = title,
		footnotes = footnotes,
		autofit = autofit
	)

	ClinicalTable(
		data = demo_data,
		flextable = demo_ft,
		type = "demographics",
		title = title,
		metadata = list(
			population = adsl_data@population,
			n_subjects = nrow(adsl)
		)
	)
}

#' Create Enrollment by Region Table
#'
#' @param adsl ADSL data frame
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param region_var Region variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_region_table <- function(
	adsl,
	title = "Enrollment by Region",
	trt_var = "TRT01P",
	region_var = "REGION1",
	autofit = TRUE
) {
	region_table <- LayeredTable(
		data = adsl,
		trt_var = trt_var,
		title = title
	)

	region_table <- region_table |>
		add_layer(CountLayer(
			target_var = region_var,
			label = "Region"
		))

	region_data <- build_table(region_table)
	region_ft <- create_hta_table(
		region_data,
		title = title,
		footnotes = c(
			"Safety Population",
			"n (%) = Number (percentage) of subjects"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = region_data,
		flextable = region_ft,
		type = "enrollment",
		title = title
	)
}

# Internal helper for category summary tables (medical history, conmeds, etc.)
# @keywords internal
create_category_summary_table <- function(
	adsl,
	analysis_data,
	category_var,
	category_label,
	title,
	type,
	footnotes,
	trt_var = "TRT01P",
	autofit = TRUE
) {
	trt_n <- adsl |>
		dplyr::group_by(!!rlang::sym(trt_var)) |>
		dplyr::summarise(N = dplyr::n(), .groups = "drop")

	summary_data <- analysis_data |>
		dplyr::group_by(!!rlang::sym(trt_var), !!rlang::sym(category_var)) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = round(.data$n_subj / .data$N * 100, 1),
			display = paste0(.data$n_subj, " (", .data$pct, "%)")
		) |>
		dplyr::select(
			!!rlang::sym(category_var),
			!!rlang::sym(trt_var),
			"display"
		) |>
		tidyr::pivot_wider(
			names_from = !!rlang::sym(trt_var),
			values_from = "display",
			values_fill = "0 (0.0%)"
		)

	# Rename category column to the display label
	names(summary_data)[names(summary_data) == category_var] <- category_label
	summary_data <- summary_data |>
		dplyr::arrange(.data[[category_label]])

	ft <- create_hta_table(
		summary_data,
		title = title,
		footnotes = footnotes,
		autofit = autofit
	)

	ClinicalTable(
		data = summary_data,
		flextable = ft,
		type = type,
		title = title
	)
}

#' Create Medical History Table
#'
#' @param adsl ADSL data frame
#' @param admh ADMH data frame
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param soc_var SOC variable name for MH
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_medical_history_table <- function(
	adsl,
	admh,
	title = "Medical History by Body System",
	trt_var = "TRT01P",
	soc_var = "MHBODSYS",
	autofit = TRUE
) {
	create_category_summary_table(
		adsl = adsl,
		analysis_data = admh,
		category_var = soc_var,
		category_label = "Body System",
		title = title,
		type = "medical_history",
		footnotes = c(
			"ITT Population",
			"n (%) = Number (percentage) of subjects with at least one condition"
		),
		trt_var = trt_var,
		autofit = autofit
	)
}

#' Create Concomitant Medications Table
#'
#' @param adsl ADSL data frame
#' @param adcm ADCM data frame
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param class_var ATC class/category variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_conmeds_table <- function(
	adsl,
	adcm,
	title = "Prior and Concomitant Medications by Class",
	trt_var = "TRT01P",
	class_var = "CMCLAS",
	autofit = TRUE
) {
	create_category_summary_table(
		adsl = adsl,
		analysis_data = adcm,
		category_var = class_var,
		category_label = "Medication Class",
		title = title,
		type = "conmeds",
		footnotes = c(
			"ITT Population",
			"n (%) = Number (percentage) of subjects taking at least one medication"
		),
		trt_var = trt_var,
		autofit = autofit
	)
}

#' Create Subject Disposition Table
#'
#' @param adsl ADSL data frame
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param status_var End of study status variable (e.g., EOSSTT)
#' @param reason_var Discontinuation reason variable (e.g., DCSREAS)
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_disposition_table <- function(
	adsl,
	title = "Subject Disposition",
	trt_var = "TRT01P",
	status_var = "EOSSTT",
	reason_var = "DCSREAS",
	autofit = TRUE
) {
	disp_data <- adsl |>
		dplyr::group_by(!!rlang::sym(trt_var), !!rlang::sym(status_var)) |>
		dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
		tidyr::pivot_wider(
			names_from = !!rlang::sym(trt_var),
			values_from = "n",
			values_fill = 0
		) |>
		dplyr::rename(`Study Status` = !!rlang::sym(status_var))

	if (reason_var %in% names(adsl)) {
		disc_reasons <- adsl |>
			dplyr::filter(!!rlang::sym(status_var) == "DISCONTINUED") |>
			dplyr::group_by(!!rlang::sym(trt_var), !!rlang::sym(reason_var)) |>
			dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
			tidyr::pivot_wider(
				names_from = !!rlang::sym(trt_var),
				values_from = "n",
				values_fill = 0
			) |>
			dplyr::rename(`Study Status` = !!rlang::sym(reason_var)) |>
			dplyr::mutate(`Study Status` = paste0("  ", .data$`Study Status`))

		disp_data <- dplyr::bind_rows(disp_data, disc_reasons)
	}

	ft <- create_hta_table(
		disp_data,
		title = title,
		footnotes = "ITT Population",
		autofit = autofit
	)

	ClinicalTable(
		data = disp_data,
		flextable = ft,
		type = "disposition",
		title = title
	)
}

#' Create Analysis Populations Summary Table
#'
#' @param adsl ADSL data frame
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param pop_flags Character vector of population flag variables
#' @param pop_labels Character vector of labels for population flags
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_population_summary_table <- function(
	adsl,
	title = "Analysis Populations",
	trt_var = "TRT01P",
	pop_flags = c("SAFFL"),
	pop_labels = c("Safety"),
	autofit = TRUE
) {
	existing_vars <- pop_flags[pop_flags %in% names(adsl)]
	existing_labels <- pop_labels[pop_flags %in% names(adsl)]

	pop_summary <- data.frame(
		Population = character(),
		stringsAsFactors = FALSE
	)

	for (i in seq_along(existing_vars)) {
		var <- existing_vars[i]
		label <- existing_labels[i]

		counts <- adsl |>
			dplyr::filter(.data[[var]] == "Y") |>
			dplyr::group_by(!!rlang::sym(trt_var)) |>
			dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
			tidyr::pivot_wider(
				names_from = !!rlang::sym(trt_var),
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

	ft <- create_hta_table(
		pop_summary,
		title = title,
		footnotes = c(
			"ITT = Intent-To-Treat Population",
			"Safety = Safety Population (subjects who received study drug)"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = pop_summary,
		flextable = ft,
		type = "populations",
		title = title
	)
}
