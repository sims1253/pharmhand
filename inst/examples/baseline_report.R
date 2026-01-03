#' Baseline Characteristics Reference Report
#'
#' Generates a comprehensive baseline characteristics report using
#' pharmaverseadam datasets. Creates a Word document with multiple tables
#' covering demographics, medical history, concomitant medications,
#' disposition, and population summaries.
#'
#' @description
#' Creates a clinical study report with the following sections:
#' - Table 1.1: Demographics and Baseline Characteristics
#' - Table 1.2: Baseline Disease Status
#' - Table 1.3: Medical History by System Organ Class
#' - Table 1.4: Prior/Concomitant Medications
#' - Table 1.5: Subject Disposition
#' - Table 1.6: Analysis Population Summary
#'
#' The output is saved to inst/examples/Baseline_Characteristics_Report.docx

# Load required packages
library(pharmhand)
library(dplyr)
library(tidyr)

#' Generate Baseline Characteristics Report
#'
#' Main function to generate the complete baseline characteristics report.
#'
#' @param output_path Path for the output .docx file
#' @param apply_gba Logical, apply G-BA formatting before export
#'
#' @return Invisibly returns the ClinicalReport object
#' @export
#'
#' @examples
#' \dontrun{
#' generate_baseline_report()
#' }
generate_baseline_report <- function(
	output_path = "inst/examples/Baseline_Characteristics_Report.docx",
	apply_gba = FALSE
) {
	# Load pharmaverseadam data
	if (!requireNamespace("pharmaverseadam", quietly = TRUE)) {
		stop(
			paste(
				"Package 'pharmaverseadam' is required.",
				"Install with: install.packages('pharmaverseadam')"
			),
			call. = FALSE
		)
	}

	adsl <- pharmaverseadam::adsl
	adcm <- pharmaverseadam::adcm
	admh <- pharmaverseadam::admh

	message("\n=== Generating Baseline Characteristics Report ===\n")

	# Create ADaMData wrappers
	message("Wrapping ADaM datasets")
	adsl_data <- ADaMData(
		data = adsl,
		domain = "ADSL",
		population = "ITT",
		trt_var = "TRT01P"
	)

	# Section 1.1: Demographics
	message("Building Demographics section (Table 1.1)")
	demo_content <- create_demographics_table(
		adsl_data = adsl_data,
		title = "Table 1.1: Demographics and Baseline Characteristics",
		autofit = FALSE # For performance test
	)
	demo_section <- ReportSection(
		title = "Demographics and Baseline Characteristics",
		section_type = "baseline",
		content = list(demo_content)
	)

	# Section 1.2: Enrollment by Region
	message("Building Enrollment by Region (Table 1.2)")
	region_content <- create_region_table(
		adsl = adsl,
		title = "Table 1.2: Enrollment by Region"
	)
	region_section <- ReportSection(
		title = "Enrollment by Region",
		section_type = "baseline",
		content = list(region_content)
	)

	# Section 1.3: Medical History
	message("Building Medical History section (Table 1.3)")
	mh_content <- create_medical_history_table(
		adsl = adsl,
		admh = admh,
		title = "Table 1.3: Medical History by Body System"
	)
	mh_section <- ReportSection(
		title = "Medical History",
		section_type = "baseline",
		content = list(mh_content)
	)

	# Section 1.4: Concomitant Medications
	message("Building Concomitant Medications section (Table 1.4)")
	cm_content <- create_conmeds_table(
		adsl = adsl,
		adcm = adcm,
		title = "Table 1.4: Prior and Concomitant Medications by Class"
	)
	cm_section <- ReportSection(
		title = "Prior and Concomitant Medications",
		section_type = "baseline",
		content = list(cm_content)
	)

	# Section 1.5: Disposition
	message("Building Disposition section (Table 1.5)")
	disp_content <- create_disposition_table(
		adsl = adsl,
		title = "Table 1.5: Subject Disposition"
	)
	disp_section <- ReportSection(
		title = "Subject Disposition",
		section_type = "disposition",
		content = list(disp_content)
	)

	# Section 1.6: Analysis Populations
	message("Building Analysis Populations section (Table 1.6)")
	pop_content <- create_population_summary_table(
		adsl = adsl,
		title = "Table 1.6: Analysis Populations"
	)
	pop_section <- ReportSection(
		title = "Analysis Populations",
		section_type = "populations",
		content = list(pop_content)
	)

	# Combine sections
	sections <- list(
		demo_section,
		region_section,
		mh_section,
		cm_section,
		disp_section,
		pop_section
	)

	# Create report
	message("Assembling report")
	report <- ClinicalReport(
		study_id = "CDISCPILOT01",
		study_title = "CDISC Pilot Study - Baseline Characteristics Report",
		sections = sections,
		metadata = list(
			generated_at = Sys.time(),
			package_version = as.character(packageVersion("pharmhand")),
			data_source = "pharmaverseadam",
			report_type = "baseline"
		)
	)

	# Write to file
	message(paste0("Writing to ", output_path))
	if (apply_gba) {
		report <- to_gba_template(report)
	}
	generate_word(report, path = output_path)

	message(paste0("Baseline report generated: ", output_path))

	invisible(report)
}

# Run if executed directly
if (sys.nframe() == 0) {
	generate_baseline_report()
}
