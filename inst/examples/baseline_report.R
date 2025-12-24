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
library(FunctionReport)
library(dplyr)
library(tidyr)

#' Generate Baseline Characteristics Report
#'
#' Main function to generate the complete baseline characteristics report.
#'
#' @param output_path Path for the output .docx file
#'
#' @return Invisibly returns the ClinicalReport object
#' @export
#'
#' @examples
#' \dontrun{
#' generate_baseline_report()
#' }
generate_baseline_report <- function(
    output_path = "inst/examples/Baseline_Characteristics_Report.docx"
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
    adcm <- pharmaverseadam::adcm
    admh <- pharmaverseadam::admh

    cli::cli_h1("Generating Baseline Characteristics Report")

    # Create ADaMData wrappers
    cli::cli_progress_step("Wrapping ADaM datasets")
    adsl_data <- ADaMData(
        data = adsl,
        domain = "ADSL",
        population = "ITT",
        trt_var = "TRT01P"
    )

    # Section 1.1: Demographics
    cli::cli_progress_step("Building Demographics section (Table 1.1)")
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
    cli::cli_progress_step("Building Enrollment by Region (Table 1.2)")
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
    cli::cli_progress_step("Building Medical History section (Table 1.3)")
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
    cli::cli_progress_step(
        "Building Concomitant Medications section (Table 1.4)"
    )
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
    cli::cli_progress_step("Building Disposition section (Table 1.5)")
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
    cli::cli_progress_step("Building Analysis Populations section (Table 1.6)")
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
    cli::cli_progress_step("Assembling report")
    report <- ClinicalReport(
        study_id = "CDISCPILOT01",
        study_title = "CDISC Pilot Study - Baseline Characteristics Report",
        sections = sections,
        metadata = list(
            generated_at = Sys.time(),
            package_version = as.character(packageVersion("FunctionReport")),
            data_source = "pharmaverseadam",
            report_type = "baseline"
        )
    )

    # Write to file
    cli::cli_progress_step("Writing to {output_path}")
    generate_word(report, path = output_path)

    cli::cli_alert_success("Baseline report generated: {output_path}")

    invisible(report)
}

# Run if executed directly
if (sys.nframe() == 0) {
    generate_baseline_report()
}
