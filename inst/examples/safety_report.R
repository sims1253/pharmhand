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
library(pharmhand)
library(dplyr)
library(tidyr)

#' Generate Safety Report
#'
#' Main function to generate the complete safety report.
#'
#' @param output_path Path for the output .docx file
#' @param apply_gba Logical, apply G-BA formatting before export
#'
#' @return Invisibly returns the ClinicalReport object
#'
#' @examples
#' \dontrun{
#' generate_safety_report()
#' }
generate_safety_report <- function(
	output_path = "inst/examples/Safety_Report.docx",
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
	adae <- pharmaverseadam::adae

	message("\n=== Generating Safety Report ===\n")

	# Section 2.1: AE Overview
	message("Building AE Overview (Table 2.1)")
	overview_section <- build_ae_overview(adae, adsl)

	# Section 2.2: AEs by SOC
	message("Building AEs by SOC (Table 2.2)")
	soc_section <- build_ae_by_soc(adae, adsl)

	# Section 2.3: Most Common AEs
	message("Building Most Common AEs (Table 2.3)")
	common_section <- build_common_aes(adae, adsl)

	# Section 2.4: AEs by Severity
	message("Building AEs by Severity (Table 2.4)")
	severity_section <- build_ae_by_severity(adae, adsl)

	# Section 2.5: AEs by Relationship
	message("Building AEs by Relationship (Table 2.5)")
	rel_section <- build_ae_by_relationship(adae, adsl)

	# Section 2.6: SAEs
	message("Building SAE Summary (Table 2.6)")
	sae_section <- build_sae_summary(adae, adsl)

	# Section 2.7: AEs Leading to Discontinuation
	message("Building AEs Leading to Discontinuation (Table 2.7)")
	disc_section <- build_ae_disc(adae, adsl)

	# Section 2.8: Deaths
	message("Building Deaths Summary (Table 2.8)")
	death_section <- build_deaths_summary(adsl)

	# Section 2.9: Time to Event
	message("Building Time to Event Analysis (KM Plot)")
	km_section <- build_time_to_event(adsl, adae, default_duration = 365)

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
	# Remove NULL sections
	sections <- Filter(Negate(is.null), sections)

	# Create report
	message("Assembling report")
	report <- ClinicalReport(
		study_id = "CDISCPILOT01",
		study_title = "CDISC Pilot Study - Safety Report",
		sections = sections,
		metadata = list(
			generated_at = Sys.time(),
			package_version = as.character(packageVersion("pharmhand")),
			data_source = "pharmaverseadam",
			report_type = "safety"
		)
	)

	# Write to file
	message(paste0("Writing to ", output_path))
	if (apply_gba) {
		report <- to_gba_template(report)
	}
	generate_word(report, path = output_path)

	message(paste0("Safety report generated: ", output_path))

	invisible(report)
}

#' Build AE Overview Table (Table 2.1)
#'
#' @param adae ADAE data frame
#' @param adsl ADSL data frame
#' @return ReportSection object
#' @keywords internal
build_ae_overview <- function(adae, adsl) {
	# Use unified create_ae_summary_table function
	overview_content <- pharmhand::create_ae_summary_table(
		adae = adae,
		adsl = adsl,
		type = "overview",
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
#' @param default_duration Default treatment duration in days when
#'   TRTDURD is not available and cannot be derived from TRTSDT/TRTEDT.
#'   Required in that case.
#' @return ReportSection object
#' @keywords internal
build_time_to_event <- function(adsl, adae, default_duration = NULL) {
	# Input validation for default_duration when supplied
	if (!is.null(default_duration)) {
		if (!is.numeric(default_duration)) {
			stop("default_duration must be numeric", call. = FALSE)
		}
		if (length(default_duration) != 1) {
			stop("default_duration must be a single value (length 1)", call. = FALSE)
		}
		if (is.na(default_duration)) {
			stop("default_duration must not be NA", call. = FALSE)
		}
		if (default_duration <= 0) {
			stop("default_duration must be greater than 0", call. = FALSE)
		}
	}

	# Define event of interest: Time to first Dermatologic event
	target_soc <- "Skin and subcutaneous tissue disorders"

	# Filter for first event
	first_event <- adae |>
		dplyr::filter(.data$AEBODSYS == target_soc) |>
		dplyr::group_by(.data$USUBJID) |>
		dplyr::arrange(.data$ASTDY) |>
		dplyr::slice(1) |>
		dplyr::select("USUBJID", "ASTDY") |>
		dplyr::mutate(has_event = 1)

	# Merge with ADSL
	# Censor at TRTEDT (Treatment End Date) or last contact
	# We will use TRTDURD if available, else derive it from TRTSDT/TRTEDT.
	# If neither is available, default_duration must be supplied.

	tte_data <- adsl

	if (!"TRTDURD" %in% names(adsl)) {
		if ("TRTEDT" %in% names(adsl) && "TRTSDT" %in% names(adsl)) {
			tte_data$TRTDURD <- as.numeric(adsl$TRTEDT - adsl$TRTSDT) + 1
		} else if (!is.null(default_duration)) {
			tte_data$TRTDURD <- default_duration
		} else {
			stop(
				paste(
					"Treatment duration (TRTDURD) is missing and cannot be derived",
					"from TRTSDT/TRTEDT. Provide default_duration explicitly",
					"to build the time-to-event analysis."
				),
				call. = FALSE
			)
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

	km_plot <- pharmhand::create_km_plot(
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
#' @param adsl ADSL data frame
#' @return SOCPTSection object
#' @keywords internal
build_ae_by_soc <- function(adae, adsl) {
	socs <- sort(unique(adae$AEBODSYS[adae$TRTEMFL == "Y"]))

	all_content <- list()

	for (soc in socs) {
		# Table for SOC using unified function with PT type and SOC filter
		tbl <- pharmhand::create_ae_summary_table(
			adae = adae,
			adsl = adsl,
			type = "pt",
			soc = soc,
			title = soc
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
#' @param adsl ADSL data frame
#' @return ReportSection object
#' @keywords internal
build_common_aes <- function(adae, adsl) {
	common_content <- pharmhand::create_ae_summary_table(
		adae = adae,
		adsl = adsl,
		type = "common",
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
#' @param adsl ADSL data frame
#' @return ReportSection object
#' @keywords internal
build_ae_by_severity <- function(adae, adsl) {
	severity_content <- pharmhand::create_ae_summary_table(
		adae = adae,
		adsl = adsl,
		type = "severity",
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
#' @param adsl ADSL data frame
#' @return ReportSection object
#' @keywords internal
build_ae_by_relationship <- function(adae, adsl) {
	rel_content <- pharmhand::create_ae_summary_table(
		adae = adae,
		adsl = adsl,
		type = "relationship",
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
#' @param adsl ADSL data frame
#' @return ReportSection object
#' @keywords internal
build_sae_summary <- function(adae, adsl) {
	sae_content <- pharmhand::create_ae_summary_table(
		adae = adae,
		adsl = adsl,
		type = "sae",
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
#' @param adsl ADSL data frame
#' @return ReportSection object
#' @keywords internal
build_ae_disc <- function(adae, adsl) {
	disc_content <- pharmhand::create_ae_summary_table(
		adae = adae,
		adsl = adsl,
		type = "discontinuation",
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
	death_content <- pharmhand::create_ae_summary_table(
		adae = NULL,
		adsl = adsl,
		type = "deaths",
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
