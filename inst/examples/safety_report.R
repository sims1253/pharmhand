#' Safety Reference Report
#'
#' Generates a comprehensive safety report using pharmaverseadam datasets.
#' Creates a Word document with multiple tables covering adverse events,
#' serious adverse events, and safety summaries.
#'
#' @description
#' Creates a clinical study report with the following sections:
#' - Table 2.1: Treatment-Emergent Adverse Events Overview
#' - Table 2.2: Adverse Events by System Organ Class
#' - Table 2.3: Most Common Adverse Events by Preferred Term
#' - Table 2.4: Adverse Events by Maximum Severity
#' - Table 2.5: Adverse Events by Relationship to Study Drug
#' - Table 2.6: Serious Adverse Events
#' - Table 2.7: Adverse Events Leading to Discontinuation
#' - Table 2.8: Deaths Summary
#'
#' The output is saved to inst/examples/Safety_Report.docx

# Load required packages
library(FunctionReport)
library(dplyr)
library(tidyr)

#' Generate Safety Report
#'
#' Main function to generate the complete safety report.
#'
#' @param output_path Path for the output .docx file
#'
#' @return Invisibly returns the ClinicalReport object
#' @export
#'
#' @examples
#' \dontrun{
#' generate_safety_report()
#' }
generate_safety_report <- function(
  output_path = "inst/examples/Safety_Report.docx"
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

  cli::cli_h1("Generating Safety Report")

  # Get treatment counts for denominators
  trt_n <- adsl |>
    dplyr::filter(.data$SAFFL == "Y") |>
    dplyr::group_by(.data$TRT01A) |>
    dplyr::summarise(N = dplyr::n(), .groups = "drop")

  # Section 2.1: AE Overview
  cli::cli_progress_step("Building AE Overview (Table 2.1)")
  overview_section <- build_ae_overview(adae, trt_n)

  # Section 2.2: AEs by SOC
  cli::cli_progress_step("Building AEs by SOC (Table 2.2)")
  soc_section <- build_ae_by_soc(adae, trt_n, adsl)

  # Section 2.3: Most Common AEs
  cli::cli_progress_step("Building Most Common AEs (Table 2.3)")
  common_section <- build_common_aes(adae, trt_n)

  # Section 2.4: AEs by Severity
  cli::cli_progress_step("Building AEs by Severity (Table 2.4)")
  severity_section <- build_ae_by_severity(adae, trt_n)

  # Section 2.5: AEs by Relationship
  cli::cli_progress_step("Building AEs by Relationship (Table 2.5)")
  rel_section <- build_ae_by_relationship(adae, trt_n)

  # Section 2.6: SAEs
  cli::cli_progress_step("Building SAE Summary (Table 2.6)")
  sae_section <- build_sae_summary(adae, trt_n)

  # Section 2.7: AEs Leading to Discontinuation
  cli::cli_progress_step(
    "Building AEs Leading to Discontinuation (Table 2.7)"
  )
  disc_section <- build_ae_disc(adae, trt_n)

  # Section 2.8: Deaths
  cli::cli_progress_step("Building Deaths Summary (Table 2.8)")
  death_section <- build_deaths_summary(adsl)

  # Section 2.9: Time to Event
  cli::cli_progress_step("Building Time to Event Analysis (KM Plot)")
  km_section <- build_time_to_event(adsl, adae)

  # Combine sections
  sections <- list(
    overview_section,
    soc_section,
    common_section,
    severity_section,
    rel_section,
    sae_section,
    disc_section,
    death_section,
    km_section
  )

  # Create report
  cli::cli_progress_step("Assembling report")
  report <- ClinicalReport(
    study_id = "CDISCPILOT01",
    study_title = "CDISC Pilot Study - Safety Report",
    sections = sections,
    metadata = list(
      generated_at = Sys.time(),
      package_version = as.character(packageVersion("FunctionReport")),
      data_source = "pharmaverseadam",
      report_type = "safety"
    )
  )

  # Write to file
  cli::cli_progress_step("Writing to {output_path}")
  generate_word(report, path = output_path)

  cli::cli_alert_success("Safety report generated: {output_path}")

  invisible(report)
}

#' Build AE Overview Table (Table 2.1)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return ReportSection object
#' @keywords internal
#' Build AE Overview Table (Table 2.1)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return ReportSection object
#' @keywords internal
build_ae_overview <- function(adae, trt_n) {
  # Use package function
  overview_content <- FunctionReport::create_ae_overview_table(
    adae = adae,
    trt_n = trt_n,
    title = "Table 2.1: Treatment-Emergent Adverse Events Overview"
  )

  if (is.null(overview_content)) {
    return(NULL)
  }

  # Add extra metadata specific to this report if needed
  overview_content@metadata$table_number <- "2.1"

  ReportSection(
    title = "Treatment-Emergent Adverse Events Overview",
    section_type = "safety",
    content = list(overview_content)
  )
}

#' Build Time to Event Analysis
#'
#' @param adsl ADSL data frame
#' @param adae ADAE data frame
#' @return ReportSection object
#' @keywords internal
build_time_to_event <- function(adsl, adae) {
  # Define event of interest: Time to first Dermatologic event
  target_soc <- "Skin and subcutaneous tissue disorders"

  # Filter for first event
  first_event <- adae |>
    dplyr::filter(AEBODSYS == target_soc) |>
    dplyr::group_by(USUBJID) |>
    dplyr::arrange(ASTDY) |>
    dplyr::slice(1) |>
    dplyr::select(USUBJID, ASTDY) |>
    dplyr::mutate(has_event = 1)

  # Merge with ADSL
  # Censor at TRTEDT (Treatment End Date) or last contact
  # Calculating a proxy for study duration for demo purposes
  # Assuming TRTEDT exists. Convert dates to numeric relative to TRTSDT if needed,
  # but here ASTDY is already relative days.
  # We will use TRTDURD if available, else derive it.

  tte_data <- adsl

  if (!"TRTDURD" %in% names(adsl)) {
    # Estimate duration if missing (e.g. from dates or using a fixed end day for pilot)
    # Using a simpler approach for the example if columns missing
    tte_data$TRTDURD <- 100 # Fallback
    if ("TRTEDT" %in% names(adsl) && "TRTSDT" %in% names(adsl)) {
      tte_data$TRTDURD <- as.numeric(adsl$TRTEDT - adsl$TRTSDT) + 1
    }
  }

  tte_data <- tte_data |>
    dplyr::left_join(first_event, by = "USUBJID") |>
    dplyr::mutate(
      event = ifelse(!is.na(has_event), 1, 0),
      time = ifelse(event == 1, ASTDY, TRTDURD),
      time = ifelse(is.na(time), TRTDURD, time) # Ensure no NAs
    ) |>
    dplyr::filter(!is.na(time), !is.na(TRT01A))

  if (nrow(tte_data) == 0) {
    return(NULL)
  }

  km_plot <- FunctionReport::create_km_plot(
    data = tte_data,
    time_var = "time",
    event_var = "event",
    trt_var = "TRT01A",
    title = paste("Figure 2.1: Time to First", target_soc),
    xlab = "Days",
    ylab = "Probability of No Event",
    risk_table = TRUE
  )

  ReportSection(
    title = "Time to Event Analysis",
    section_type = "safety",
    content = list(km_plot)
  )
}

#' Build AEs by SOC Table (Table 2.2)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return SOCPTSection object
#' @keywords internal
build_ae_by_soc <- function(adae, trt_n, adsl) {
  socs <- sort(unique(adae$AEBODSYS[adae$TRTEMFL == "Y"]))

  all_content <- list()

  for (soc in socs) {
    # Table for SOC
    tbl <- create_ae_pt_table_for_soc(
      adae = adae,
      trt_n = trt_n,
      soc = soc
    )
    all_content[[length(all_content) + 1]] <- tbl

    # KM Plot for SOC
    plt <- create_ae_km_plot_for_soc(
      adsl = adsl,
      adae = adae,
      soc = soc
    )
    if (!is.null(plt)) {
      all_content[[length(all_content) + 1]] <- plt
    }
  }

  SOCPTSection(
    title = "Adverse Events by System Organ Class",
    section_type = "safety",
    soc_var = "AEBODSYS",
    pt_var = "AEDECOD",
    group_var = "TRT01A",
    content = all_content
  )
}

#' Build Most Common AEs Table (Table 2.3)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return ReportSection object
#' @keywords internal
build_common_aes <- function(adae, trt_n) {
  common_content <- create_common_ae_table(
    adae = adae,
    trt_n = trt_n,
    title = "Table 2.3: Most Common Adverse Events (>=2% in any treatment group)"
  )

  ReportSection(
    title = "Most Common Adverse Events",
    section_type = "safety",
    content = list(common_content)
  )
}

#' Build AEs by Severity Table (Table 2.4)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return ReportSection object
#' @keywords internal
build_ae_by_severity <- function(adae, trt_n) {
  severity_content <- create_ae_severity_table(
    adae = adae,
    trt_n = trt_n,
    title = "Table 2.4: Subjects by Maximum Adverse Event Severity"
  )

  ReportSection(
    title = "Adverse Events by Maximum Severity",
    section_type = "safety",
    content = list(severity_content)
  )
}

#' Build AEs by Relationship Table (Table 2.5)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return ReportSection object
#' @keywords internal
build_ae_by_relationship <- function(adae, trt_n) {
  rel_content <- create_ae_relationship_table(
    adae = adae,
    trt_n = trt_n,
    title = "Table 2.5: Adverse Events by Relationship to Study Drug"
  )

  ReportSection(
    title = "Adverse Events by Relationship to Study Drug",
    section_type = "safety",
    content = list(rel_content)
  )
}

#' Build SAE Summary Table (Table 2.6)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return ReportSection object
#' @keywords internal
build_sae_summary <- function(adae, trt_n) {
  sae_content <- create_sae_table(
    adae = adae,
    trt_n = trt_n,
    title = "Table 2.6: Serious Adverse Events"
  )

  ReportSection(
    title = "Serious Adverse Events",
    section_type = "safety_sae",
    content = list(sae_content)
  )
}

#' Build AEs Leading to Discontinuation Table (Table 2.7)
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @return ReportSection object
#' @keywords internal
build_ae_disc <- function(adae, trt_n) {
  disc_content <- create_ae_discontinuation_table(
    adae = adae,
    trt_n = trt_n,
    title = "Table 2.7: Adverse Events Leading to Study Drug Discontinuation"
  )

  ReportSection(
    title = "Adverse Events Leading to Study Drug Discontinuation",
    section_type = "safety",
    content = list(disc_content)
  )
}

#' Build Deaths Summary Table (Table 2.8)
#'
#' @param adsl ADSL data frame
#' @return ReportSection object
#' @keywords internal
build_deaths_summary <- function(adsl) {
  death_content <- create_deaths_table(
    adsl = adsl,
    title = "Table 2.8: Deaths Summary"
  )

  ReportSection(
    title = "Deaths",
    section_type = "safety",
    content = list(death_content)
  )
}


# Run if executed directly
if (sys.nframe() == 0) {
  generate_safety_report()
}
