#' Reference Report Generator
#'
#' Generates a comprehensive reference report using pharmaverseadam datasets.
#' This script serves as both documentation and a test fixture for the
#' pharmhand package.
#'
#' @description
#' Creates a clinical study report with the following sections:
#' - Demographics (baseline characteristics)
#' - Patient Disposition
#' - Adverse Events Summary (SOC-PT hierarchy)
#' - Adverse Events by Severity
#' - Serious Adverse Events
#'
#' The output is saved to inst/examples/Reference_Report.docx

# Load required packages
library(pharmhand)
library(dplyr)

#' Generate Reference Report
#'
#' Main function to generate the complete reference report.
#'
#' @param output_path Path for the output .docx file
#' @param include_hta Logical, include HTA-specific sections
#'
#' @return Invisibly returns the ClinicalReport object
#' @export
#'
#' @examples
#' \dontrun{
#' generate_reference_report()
#' }
generate_reference_report <- function(
	output_path = "inst/examples/Reference_Report.docx",
	include_hta = TRUE
) {
	# Load pharmaverseadam data
	if (!requireNamespace("pharmaverseadam", quietly = TRUE)) {
		cli::cli_abort(
			c(
				"Package {.pkg pharmaverseadam} is required",
				"i" = "Install with: install.packages('pharmaverseadam')"
			)
		)
	}

	adsl <- pharmaverseadam::adsl
	adae <- pharmaverseadam::adae

	cli::cli_h1("Generating Reference Report")

	# Create ADaMData wrappers
	cli::cli_progress_step("Wrapping ADaM datasets")
	adsl_data <- ADaMData(
		data = adsl,
		domain = "ADSL",
		population = "ITT",
		trt_var = "TRT01P"
	)

	adae_data <- ADaMData(
		data = adae,
		domain = "ADAE",
		population = "SAF",
		trt_var = "TRT01A"
	)

	# Section 1: Demographics
	cli::cli_progress_step("Building Demographics section")
	demo_section <- build_demographics_section(adsl_data)

	# Section 2: Disposition
	cli::cli_progress_step("Building Disposition section")
	disp_section <- build_disposition_section(adsl_data)

	# Section 3: Adverse Events
	cli::cli_progress_step("Building Adverse Events section")
	ae_section <- build_ae_section(adae_data, adsl_data)

	# Section 4: SAE Summary
	cli::cli_progress_step("Building SAE section")
	sae_section <- build_sae_section(adae_data, adsl_data)

	# Combine sections
	sections <- list(demo_section, disp_section, ae_section, sae_section)

	# Add HTA section if requested
	if (include_hta) {
		cli::cli_progress_step("Building HTA section")
		hta_section <- build_hta_section(adsl_data, adae_data)
		sections <- c(sections, list(hta_section))
	}

	# Create report
	cli::cli_progress_step("Assembling report")
	report <- ClinicalReport(
		study_id = "CDISCPILOT01",
		study_title = "CDISC Pilot Study - Reference Report",
		sections = sections,
		metadata = list(
			generated_at = Sys.time(),
			package_version = as.character(packageVersion("pharmhand")),
			data_source = "pharmaverseadam"
		)
	)

	# Write to file
	cli::cli_progress_step("Writing to {output_path}")
	write_docx(report, output_path)

	cli::cli_alert_success("Reference report generated: {output_path}")

	invisible(report)
}

#' Build Demographics Section
#'
#' @param adsl_data ADaMData object containing ADSL
#' @return ReportSection object
#' @keywords internal
build_demographics_section <- function(adsl_data) {
	adsl <- adsl_data@data

	# Use LayeredTable for demographics
	demo_table <- LayeredTable(
		data = adsl,
		trt_var = "TRT01P",
		title = "Table 1: Demographics and Baseline Characteristics"
	)

	# Add layers
	demo_table <- demo_table |>
		add_layer(DescriptiveLayer(
			target_var = "AGE",
			label = "Age (years)"
		)) |>
		add_layer(CountLayer(
			target_var = "SEX",
			label = "Sex"
		)) |>
		add_layer(CountLayer(
			target_var = "RACE",
			label = "Race"
		)) |>
		add_layer(CountLayer(
			target_var = "ETHNIC",
			label = "Ethnicity"
		))

	# Build and convert to flextable
	demo_data <- build_table(demo_table)
	demo_ft <- create_hta_table(
		demo_data,
		title = "Table 1: Demographics and Baseline Characteristics",
		footnotes = c(
			"ITT Population",
			"Age is summarized as mean (SD), median, min-max"
		)
	)

	demo_content <- ClinicalTable(
		data = demo_data,
		flextable = demo_ft,
		type = "demographics",
		title = "Demographics and Baseline Characteristics",
		metadata = list(
			population = "ITT",
			n_subjects = nrow(adsl)
		)
	)

	ReportSection(
		title = "Demographics and Baseline Characteristics",
		section_type = "baseline",
		content = list(demo_content)
	)
}

#' Build Disposition Section
#'
#' @param adsl_data ADaMData object containing ADSL
#' @return ReportSection object
#' @keywords internal
build_disposition_section <- function(adsl_data) {
	adsl <- adsl_data@data

	# Disposition summary
	disp_data <- adsl |>
		dplyr::group_by(.data$TRT01P, .data$EOSSTT) |>
		dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
		tidyr::pivot_wider(
			names_from = "TRT01P",
			values_from = "n",
			values_fill = 0
		)

	disp_ft <- create_hta_table(
		disp_data,
		title = "Table 2: Patient Disposition"
	)

	disp_content <- ClinicalTable(
		data = disp_data,
		flextable = disp_ft,
		type = "disposition",
		title = "Patient Disposition"
	)

	ReportSection(
		title = "Patient Disposition",
		section_type = "disposition",
		content = list(disp_content)
	)
}

#' Build Adverse Events Section
#'
#' @param adae_data ADaMData object containing ADAE
#' @param adsl_data ADaMData object containing ADSL for denominators
#' @return ReportSection object
#' @keywords internal
build_ae_section <- function(adae_data, adsl_data) {
	adae <- adae_data@data
	adsl <- adsl_data@data

	# Get treatment groups and counts
	trt_n <- adsl |>
		dplyr::group_by(.data$TRT01A) |>
		dplyr::summarise(N = dplyr::n(), .groups = "drop")

	# SOC-level summary
	soc_summary <- adae |>
		dplyr::group_by(.data$TRT01A, .data$AEBODSYS) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			n_events = dplyr::n(),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = "TRT01A") |>
		dplyr::mutate(
			pct = round(.data$n_subj / .data$N * 100, 1),
			display = paste0(.data$n_subj, " (", .data$pct, "%)")
		)

	# Pivot for display
	soc_wide <- soc_summary |>
		dplyr::select("AEBODSYS", "TRT01A", "display") |>
		tidyr::pivot_wider(
			names_from = "TRT01A",
			values_from = "display",
			values_fill = "0 (0.0%)"
		) |>
		dplyr::rename(`System Organ Class` = "AEBODSYS")

	ae_ft <- create_hta_table(
		soc_wide,
		title = "Table 3: Adverse Events by System Organ Class",
		footnotes = c(
			"Safety Population",
			"n (%) = Number (percentage) of subjects with at least one event"
		)
	)

	ae_content <- ClinicalTable(
		data = soc_wide,
		flextable = ae_ft,
		type = "safety_ae",
		title = "Adverse Events by System Organ Class"
	)

	SOCPTSection(
		title = "Adverse Events",
		section_type = "safety",
		soc_var = "AEBODSYS",
		pt_var = "AEDECOD",
		group_var = "TRT01A",
		content = list(ae_content)
	)
}

#' Build SAE Section
#'
#' @param adae_data ADaMData object containing ADAE
#' @param adsl_data ADaMData object containing ADSL
#' @return ReportSection object
#' @keywords internal
build_sae_section <- function(adae_data, adsl_data) {
	adae <- adae_data@data
	adsl <- adsl_data@data

	# Filter to SAEs
	sae <- adae |> dplyr::filter(.data$AESER == "Y")

	if (nrow(sae) == 0) {
		# No SAEs - create empty section
		sae_data <- data.frame(
			Message = "No serious adverse events reported"
		)
	} else {
		# Get treatment counts
		trt_n <- adsl |>
			dplyr::group_by(.data$TRT01A) |>
			dplyr::summarise(N = dplyr::n(), .groups = "drop")

		sae_data <- sae |>
			dplyr::group_by(.data$TRT01A, .data$AEBODSYS, .data$AEDECOD) |>
			dplyr::summarise(
				n_subj = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			) |>
			dplyr::left_join(trt_n, by = "TRT01A") |>
			dplyr::mutate(
				pct = round(.data$n_subj / .data$N * 100, 1),
				display = paste0(.data$n_subj, " (", .data$pct, "%)")
			) |>
			dplyr::select("AEBODSYS", "AEDECOD", "TRT01A", "display") |>
			tidyr::pivot_wider(
				names_from = "TRT01A",
				values_from = "display",
				values_fill = "0 (0.0%)"
			) |>
			dplyr::rename(
				`System Organ Class` = "AEBODSYS",
				`Preferred Term` = "AEDECOD"
			)
	}

	sae_ft <- create_hta_table(
		sae_data,
		title = "Table 4: Serious Adverse Events",
		footnotes = "Safety Population; AESER = 'Y'"
	)

	sae_content <- ClinicalTable(
		data = sae_data,
		flextable = sae_ft,
		type = "safety_sae",
		title = "Serious Adverse Events"
	)

	ReportSection(
		title = "Serious Adverse Events",
		section_type = "safety_sae",
		content = list(sae_content)
	)
}

#' Build HTA Section
#'
#' @param adsl_data ADaMData object containing ADSL
#' @param adae_data ADaMData object containing ADAE
#' @return HTASection object
#' @keywords internal
build_hta_section <- function(adsl_data, adae_data) {
	adsl <- adsl_data@data
	adae <- adae_data@data

	# Create HTA-style summary comparing treatment arms
	# Focus on key safety metrics for G-BA Module 4

	trt_n <- adsl |>
		dplyr::group_by(.data$TRT01A) |>
		dplyr::summarise(N = dplyr::n(), .groups = "drop")

	# Any AE summary
	any_ae <- adae |>
		dplyr::group_by(.data$TRT01A) |>
		dplyr::summarise(
			n_any_ae = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = "TRT01A") |>
		dplyr::mutate(pct_any_ae = round(.data$n_any_ae / .data$N * 100, 1))

	# SAE summary
	sae_sum <- adae |>
		dplyr::filter(.data$AESER == "Y") |>
		dplyr::group_by(.data$TRT01A) |>
		dplyr::summarise(
			n_sae = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		)

	# Combine
	hta_summary <- any_ae |>
		dplyr::left_join(sae_sum, by = "TRT01A") |>
		dplyr::mutate(
			n_sae = tidyr::replace_na(.data$n_sae, 0L),
			pct_sae = round(.data$n_sae / .data$N * 100, 1)
		) |>
		dplyr::select(
			Treatment = "TRT01A",
			N = "N",
			`Any AE n` = "n_any_ae",
			`Any AE %` = "pct_any_ae",
			`SAE n` = "n_sae",
			`SAE %` = "pct_sae"
		)

	hta_ft <- create_hta_table(
		hta_summary,
		title = "Table 5: Safety Overview for HTA Submission",
		footnotes = c(
			"Safety Population",
			"AE = Adverse Event; SAE = Serious Adverse Event",
			"Prepared for G-BA Module 4 submission"
		)
	)

	hta_content <- ClinicalTable(
		data = hta_summary,
		flextable = hta_ft,
		type = "hta_safety",
		title = "Safety Overview for HTA"
	)

	HTASection(
		title = "HTA Safety Summary",
		section_type = "hta",
		comparator = "Placebo",
		population = "Safety",
		content = list(hta_content)
	)
}

# Run if executed directly
if (sys.nframe() == 0) {
	generate_reference_report()
}
