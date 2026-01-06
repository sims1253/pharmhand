#' Efficacy Reference Report
#'
#' Generates a comprehensive efficacy report using pharmaverseadam datasets.
#' Creates a Word document with multiple tables covering primary endpoints,
#' laboratory parameters, vital signs, and subgroup analyses.
#'
#' @description
#' Creates a clinical study report with the following sections:
#' - Table 3.1: Primary Endpoint Summary (Vital Signs - Systolic BP)
#' - Table 3.2: Change from Baseline Summary
#' - Table 3.3: Vital Signs by Visit
#' - Table 3.4: Laboratory Parameters Summary
#' - Table 3.5: Laboratory Shift Table
#' - Table 3.6: Subgroup Analysis by Age Group
#'
#' The output is saved to inst/examples/Efficacy_Report.docx

# Load required packages
library(pharmhand)
library(dplyr)
library(tidyr)

#' Generate Efficacy Report
#'
#' Main function to generate the complete efficacy report.
#'
#' @param output_path Path for the output .docx file
#' @param apply_gba Logical, apply G-BA formatting before export
#'
#' @return Invisibly returns the ClinicalReport object
#'
#' @examples
#' \dontrun{
#' generate_efficacy_report()
#' }
generate_efficacy_report <- function(
	output_path = "inst/examples/Efficacy_Report.docx",
	apply_gba = FALSE
) {
	# Load pharmaverseadam data
	if (!requireNamespace("pharmaverseadam", quietly = TRUE)) {
		stop(
			paste(
				"Package pharmaverseadam is required.",
				"Install with: install.packages('pharmaverseadam')"
			),
			call. = FALSE
		)
	}

	adsl <- pharmaverseadam::adsl
	advs <- pharmaverseadam::advs
	adlb <- pharmaverseadam::adlb

	message("\n=== Generating Efficacy Report ===\n")

	# Section 3.1: Primary Endpoint Summary
	message("Building Primary Endpoint Summary (Table 3.1)")

	# Get treatment counts for denominators (primary endpoint table only)
	trt_n <- adsl |>
		dplyr::filter(.data$SAFFL == "Y") |>
		dplyr::group_by(.data$TRT01P) |>
		dplyr::summarise(N = dplyr::n(), .groups = "drop")

	primary_content <- create_primary_endpoint_table(advs, trt_n)
	primary_section <- ReportSection(
		title = "Primary Endpoint Analysis",
		section_type = "efficacy",
		content = list(primary_content)
	)

	# Section 3.2: Change from Baseline
	message("Building Change from Baseline (Table 3.2)")
	cfb_content <- create_cfb_summary_table(advs)
	cfb_section <- ReportSection(
		title = "Change from Baseline Analysis",
		section_type = "efficacy",
		content = list(cfb_content)
	)

	# Section 3.3: Vital Signs by Visit
	message("Building Vital Signs by Visit (Table 3.3)")
	vs_content <- create_vs_by_visit_table(advs)
	vs_section <- ReportSection(
		title = "Vital Signs by Study Visit",
		section_type = "efficacy",
		content = list(vs_content)
	)

	# Section 3.4: Laboratory Parameters
	message("Building Laboratory Parameters (Table 3.4)")
	lab_content <- create_lab_summary_table(adlb)
	lab_section <- ReportSection(
		title = "Laboratory Parameters",
		section_type = "efficacy",
		content = list(lab_content)
	)

	# Section 3.5: Laboratory Shift Table
	message("Building Laboratory Shift Table (Table 3.5)")
	shift_content <- create_lab_shift_table(adlb)
	shift_section <- ReportSection(
		title = "Laboratory Shift Analysis",
		section_type = "efficacy",
		content = list(shift_content)
	)

	# Section 3.6: Subgroup Analysis
	message("Building Subgroup Analysis (Table 3.6)")
	subgroup_content <- create_subgroup_analysis_table(adsl, advs)
	subgroup_section <- ReportSection(
		title = "Subgroup Analyses",
		section_type = "efficacy",
		content = list(subgroup_content)
	)

	# Combine sections
	sections <- list(
		primary_section,
		cfb_section,
		vs_section,
		lab_section,
		shift_section,
		subgroup_section
	)

	# Create report
	message("Assembling report")
	report <- ClinicalReport(
		study_id = "CDISCPILOT01",
		study_title = "CDISC Pilot Study - Efficacy Report",
		sections = sections,
		metadata = list(
			generated_at = Sys.time(),
			package_version = as.character(packageVersion("pharmhand")),
			data_source = "pharmaverseadam",
			report_type = "efficacy"
		)
	)

	# Write to file
	message(paste0("Writing to ", output_path))
	if (apply_gba) {
		report <- to_gba_template(report)
	}
	generate_word(report, path = output_path)

	message(paste0("Efficacy report generated: ", output_path))

	invisible(report)
}

# Run if executed directly
if (sys.nframe() == 0) {
	generate_efficacy_report()
}
