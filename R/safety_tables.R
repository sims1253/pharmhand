#' Safety Analysis Tables
#'
#' Reusable functions for creating standard safety analysis tables
#' including AE overviews, SOC/PT summaries, severity, relationship,
#' SAEs, and deaths.
#'
#' @name safety_tables
#' @keywords internal
NULL

#' Create Adverse Event Table
#'
#' Unified function for generating standard adverse event tables for
#' HTA dossiers and clinical study reports. Supports multiple table
#' types through a single interface.
#'
#' @param adae ADAE data frame (ADaM Adverse Events dataset)
#' @param adsl ADSL data frame (optional, required for some table types
#'   like "deaths" or for denominator calculation)
#' @param type Character string specifying the table type:
#'   \itemize{
#'     \item "overview" - Summary of TEAEs, related AEs, SAEs, discontinuations
#'     \item "soc" - AEs by System Organ Class
#'     \item "soc_pt" - AEs by SOC and Preferred Term (hierarchical)
#'     \item "pt" - AEs by Preferred Term only
#'     \item "common" - Most frequently reported AEs
#'     \item "severity" - AEs by maximum severity
#'     \item "relationship" - AEs by relationship to study drug
#'     \item "sae" - Serious Adverse Events
#'     \item "discontinuation" - AEs leading to discontinuation
#'     \item "deaths" - Deaths summary (requires adsl)
#'   }
#' @param trt_var Treatment variable name (default: "TRT01A")
#' @param n_top For type="common", number of top PTs to show (default: 15)
#' @param soc For type="pt", filter to specific SOC (optional)
#' @param title Table title (auto-generated if NULL)
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' # AE Overview
#' overview <- create_ae_table(adae, adsl, type = "overview")
#'
#' # SOC table
#' soc_table <- create_ae_table(adae, adsl, type = "soc")
#'
#' # SOC/PT hierarchical table
#' soc_pt <- create_ae_table(adae, adsl, type = "soc_pt")
#'
#' # Most common AEs (top 20)
#' common <- create_ae_table(adae, adsl, type = "common", n_top = 20)
#'
#' # SAE table
#' sae <- create_ae_table(adae, adsl, type = "sae")
#' }
create_ae_table <- function(
	adae,
	adsl = NULL,
	type = c(
		"overview",
		"soc",
		"soc_pt",
		"pt",
		"common",
		"severity",
		"relationship",
		"sae",
		"discontinuation",
		"deaths"
	),
	trt_var = "TRT01A",
	n_top = 15,
	soc = NULL,
	title = NULL,
	autofit = TRUE
) {
	type <- match.arg(type)

	# Input validation
	if (type != "deaths" && !is.data.frame(adae)) {
		cli::cli_abort("{.arg adae} must be a data frame")
	}
	if (type == "deaths" && !is.data.frame(adsl)) {
		cli::cli_abort("{.arg adsl} is required for type = 'deaths'")
	}

	# Get treatment counts - prefer from adsl if available
	trt_n <- if (!is.null(adsl) && is.data.frame(adsl)) {
		get_trt_n(adsl, trt_var = trt_var, population = "SAF")
	} else {
		adae |>
			dplyr::filter(.data$TRTEMFL == "Y") |>
			dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
			dplyr::summarise(N = dplyr::n_distinct(.data$USUBJID), .groups = "drop")
	}

	# Auto-generate title if not provided
	if (is.null(title)) {
		title <- switch(
			type,
			overview = "Overview of Adverse Events",
			soc = "Adverse Events by System Organ Class",
			soc_pt = "Adverse Events by System Organ Class and Preferred Term",
			pt = if (!is.null(soc)) soc else "Adverse Events by Preferred Term",
			common = sprintf("Most Common Adverse Events (Top %d)", n_top),
			severity = "Subjects by Maximum Adverse Event Severity",
			relationship = "Adverse Events by Relationship to Study Drug",
			sae = "Serious Adverse Events",
			discontinuation = "Adverse Events Leading to Study Drug Discontinuation",
			deaths = "Deaths Summary"
		)
	}

	# Dispatch to appropriate handler
	result <- switch(
		type,
		overview = create_ae_table_overview(adae, trt_n, trt_var, title, autofit),
		soc = create_ae_table_soc(adae, trt_n, trt_var, title, autofit),
		soc_pt = create_ae_table_soc_pt(adae, trt_n, trt_var, title, autofit),
		pt = create_ae_table_pt(adae, trt_n, trt_var, soc, title, autofit),
		common = create_ae_table_common(
			adae,
			trt_n,
			trt_var,
			n_top,
			title,
			autofit
		),
		severity = create_ae_table_severity(adae, trt_n, trt_var, title, autofit),
		relationship = create_ae_table_relationship(
			adae,
			trt_n,
			trt_var,
			title,
			autofit
		),
		sae = create_ae_table_sae(adae, trt_n, trt_var, title, autofit),
		discontinuation = create_ae_table_discontinuation(
			adae,
			trt_n,
			trt_var,
			title,
			autofit
		),
		deaths = create_ae_table_deaths(adsl, trt_var, title, autofit)
	)

	result
}

# ===========================================================================
# Internal handler functions for create_ae_table
# ===========================================================================

#' @keywords internal
create_ae_table_overview <- function(adae, trt_n, trt_var, title, autofit) {
	summarize_category <- function(data, category_label) {
		if (nrow(data) == 0) {
			return(NULL)
		}
		data |>
			dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			) |>
			dplyr::left_join(trt_n, by = trt_var) |>
			dplyr::mutate(
				pct = round(.data$n / .data$N * 100, 1),
				Category = category_label
			)
	}

	categories <- list()

	if ("TRTEMFL" %in% names(adae)) {
		teae <- adae |> dplyr::filter(.data$TRTEMFL == "Y")
		categories[[1]] <- summarize_category(
			teae,
			"Subjects with at least one TEAE"
		)
	}

	if ("TRTEMFL" %in% names(adae) && "AEREL" %in% names(adae)) {
		rel_teae <- adae |>
			dplyr::filter(
				.data$TRTEMFL == "Y",
				.data$AEREL %in% c("PROBABLE", "POSSIBLE", "RELATED")
			)
		categories[[2]] <- summarize_category(
			rel_teae,
			"Subjects with at least one related TEAE"
		)
	}

	if ("TRTEMFL" %in% names(adae) && "AESER" %in% names(adae)) {
		sae <- adae |> dplyr::filter(.data$TRTEMFL == "Y", .data$AESER == "Y")
		categories[[3]] <- summarize_category(sae, "Subjects with at least one SAE")
	}

	if ("TRTEMFL" %in% names(adae) && "AEACN" %in% names(adae)) {
		disc <- adae |>
			dplyr::filter(.data$TRTEMFL == "Y", .data$AEACN == "DRUG WITHDRAWN")
		categories[[4]] <- summarize_category(
			disc,
			"Subjects with AE leading to discontinuation"
		)
	}

	if ("TRTEMFL" %in% names(adae) && "AEOUT" %in% names(adae)) {
		fatal <- adae |> dplyr::filter(.data$TRTEMFL == "Y", .data$AEOUT == "FATAL")
		categories[[5]] <- summarize_category(fatal, "Deaths")
	}

	overview_combined <- dplyr::bind_rows(categories)

	if (nrow(overview_combined) == 0) {
		cli::cli_warn("No adverse event data found matching criteria")
		return(NULL)
	}

	overview_formatted <- overview_combined |>
		dplyr::mutate(value = sprintf("%d (%.1f%%)", n, pct)) |>
		dplyr::select("Category", dplyr::all_of(trt_var), "value") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "value",
			values_fill = "0 (0.0%)"
		)

	ft <- create_hta_table(overview_formatted, title = title, autofit = autofit)

	ClinicalTable(
		data = overview_formatted,
		flextable = ft,
		type = "ae_overview",
		title = title
	)
}

#' @keywords internal
create_ae_table_soc <- function(adae, trt_n, trt_var, title, autofit) {
	soc_summary <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y") |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$AEBODSYS) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = round(.data$n_subj / .data$N * 100, 1),
			display = paste0(.data$n_subj, " (", .data$pct, "%)")
		) |>
		dplyr::select("AEBODSYS", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "0 (0.0%)"
		) |>
		dplyr::rename(`System Organ Class` = "AEBODSYS") |>
		dplyr::arrange(.data$`System Organ Class`)

	ft <- create_hta_table(
		soc_summary,
		title = title,
		footnotes = c(
			"Safety Population",
			"n (%) = Number (percentage) of subjects with at least one event"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = soc_summary,
		flextable = ft,
		type = "ae_soc",
		title = title
	)
}

#' @keywords internal
create_ae_table_soc_pt <- function(adae, trt_n, trt_var, title, autofit) {
	soc_pt_summary <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y") |>
		dplyr::group_by(
			dplyr::across(dplyr::all_of(trt_var)),
			.data$AEBODSYS,
			.data$AEDECOD
		) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = round(.data$n_subj / .data$N * 100, 1),
			display = paste0(.data$n_subj, " (", .data$pct, "%)")
		) |>
		dplyr::select("AEBODSYS", "AEDECOD", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "0 (0.0%)"
		) |>
		dplyr::rename(
			`System Organ Class` = "AEBODSYS",
			`Preferred Term` = "AEDECOD"
		) |>
		dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)

	ft <- create_hta_table(
		soc_pt_summary,
		title = title,
		footnotes = c(
			"Safety Population",
			"n (%) = Number (percentage) of subjects with at least one event"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = soc_pt_summary,
		flextable = ft,
		type = "ae_soc_pt",
		title = title
	)
}

#' @keywords internal
create_ae_table_pt <- function(adae, trt_n, trt_var, soc, title, autofit) {
	data <- adae |> dplyr::filter(.data$TRTEMFL == "Y")
	if (!is.null(soc)) {
		data <- data |> dplyr::filter(.data$AEBODSYS == soc)
	}

	pt_summary <- data |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$AEDECOD) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = round(.data$n_subj / .data$N * 100, 1),
			display = paste0(.data$n_subj, " (", .data$pct, "%)")
		) |>
		dplyr::select("AEDECOD", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "0 (0.0%)"
		) |>
		dplyr::rename(`Preferred Term` = "AEDECOD") |>
		dplyr::arrange(.data$`Preferred Term`)

	ft <- create_hta_table(pt_summary, title = title, autofit = autofit)

	ClinicalTable(
		data = pt_summary,
		flextable = ft,
		type = "ae_pt",
		title = title
	)
}

#' @keywords internal
create_ae_table_common <- function(
	adae,
	trt_n,
	trt_var,
	n_top,
	title,
	autofit
) {
	top_pts <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y") |>
		dplyr::group_by(.data$AEDECOD) |>
		dplyr::summarise(n = dplyr::n_distinct(.data$USUBJID), .groups = "drop") |>
		dplyr::arrange(dplyr::desc(.data$n)) |>
		dplyr::slice_head(n = n_top) |>
		dplyr::pull(.data$AEDECOD)

	common_summary <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y", .data$AEDECOD %in% top_pts) |>
		dplyr::group_by(
			dplyr::across(dplyr::all_of(trt_var)),
			.data$AEBODSYS,
			.data$AEDECOD
		) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = round(.data$n_subj / .data$N * 100, 1),
			display = paste0(.data$n_subj, " (", .data$pct, "%)")
		) |>
		dplyr::select("AEBODSYS", "AEDECOD", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "0 (0.0%)"
		) |>
		dplyr::rename(
			`System Organ Class` = "AEBODSYS",
			`Preferred Term` = "AEDECOD"
		) |>
		dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)

	ft <- create_hta_table(
		common_summary,
		title = title,
		footnotes = c(
			"Safety Population",
			sprintf("Showing top %d most frequently reported Preferred Terms", n_top)
		),
		autofit = autofit
	)

	ClinicalTable(
		data = common_summary,
		flextable = ft,
		type = "ae_common",
		title = title
	)
}

#' @keywords internal
create_ae_table_severity <- function(adae, trt_n, trt_var, title, autofit) {
	severity_summary <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y") |>
		dplyr::mutate(
			AESEV_ord = factor(
				.data$AESEV,
				levels = c("MILD", "MODERATE", "SEVERE"),
				ordered = TRUE
			)
		) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$USUBJID) |>
		dplyr::summarise(
			max_sev = as.character(max(.data$AESEV_ord, na.rm = TRUE)),
			.groups = "drop"
		) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$max_sev) |>
		dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = round(.data$n / .data$N * 100, 1),
			display = paste0(.data$n, " (", .data$pct, "%)")
		) |>
		dplyr::select("max_sev", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "0 (0.0%)"
		) |>
		dplyr::rename(`Maximum Severity` = "max_sev")

	ft <- create_hta_table(
		severity_summary,
		title = title,
		footnotes = c(
			"Safety Population",
			"Maximum severity across all TEAEs per subject"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = severity_summary,
		flextable = ft,
		type = "ae_severity",
		title = title
	)
}

#' @keywords internal
create_ae_table_relationship <- function(adae, trt_n, trt_var, title, autofit) {
	rel_summary <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y") |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var)), .data$AEREL) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = round(.data$n_subj / .data$N * 100, 1),
			display = paste0(.data$n_subj, " (", .data$pct, "%)")
		) |>
		dplyr::select("AEREL", dplyr::all_of(trt_var), "display") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "display",
			values_fill = "0 (0.0%)"
		) |>
		dplyr::rename(`Relationship to Study Drug` = "AEREL")

	ft <- create_hta_table(
		rel_summary,
		title = title,
		footnotes = c(
			"Safety Population",
			"Subjects counted once per relationship category"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = rel_summary,
		flextable = ft,
		type = "ae_relationship",
		title = title
	)
}

#' @keywords internal
create_ae_table_sae <- function(adae, trt_n, trt_var, title, autofit) {
	sae <- adae |> dplyr::filter(.data$TRTEMFL == "Y", .data$AESER == "Y")

	if (nrow(sae) == 0) {
		summary_df <- data.frame(
			Message = "No serious adverse events reported during the study"
		)
	} else {
		summary_df <- sae |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(trt_var)),
				.data$AEBODSYS,
				.data$AEDECOD
			) |>
			dplyr::summarise(
				n_subj = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			) |>
			dplyr::left_join(trt_n, by = trt_var) |>
			dplyr::mutate(
				pct = round(.data$n_subj / .data$N * 100, 1),
				display = paste0(.data$n_subj, " (", .data$pct, "%)")
			) |>
			dplyr::select("AEBODSYS", "AEDECOD", dplyr::all_of(trt_var), "display") |>
			tidyr::pivot_wider(
				names_from = dplyr::all_of(trt_var),
				values_from = "display",
				values_fill = "0 (0.0%)"
			) |>
			dplyr::rename(
				`System Organ Class` = "AEBODSYS",
				`Preferred Term` = "AEDECOD"
			) |>
			dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)
	}

	ft <- create_hta_table(
		summary_df,
		title = title,
		footnotes = c(
			"Safety Population",
			"SAE = Serious Adverse Event (AESER = 'Y')"
		),
		autofit = autofit
	)

	ClinicalTable(
		data = summary_df,
		flextable = ft,
		type = "ae_sae",
		title = title
	)
}

#' @keywords internal
create_ae_table_discontinuation <- function(
	adae,
	trt_n,
	trt_var,
	title,
	autofit
) {
	disc <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y", .data$AEACN == "DRUG WITHDRAWN")

	if (nrow(disc) == 0) {
		summary_df <- data.frame(
			Message = "No adverse events leading to study drug discontinuation"
		)
	} else {
		summary_df <- disc |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(trt_var)),
				.data$AEBODSYS,
				.data$AEDECOD
			) |>
			dplyr::summarise(
				n_subj = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			) |>
			dplyr::left_join(trt_n, by = trt_var) |>
			dplyr::mutate(
				pct = round(.data$n_subj / .data$N * 100, 1),
				display = paste0(.data$n_subj, " (", .data$pct, "%)")
			) |>
			dplyr::select("AEBODSYS", "AEDECOD", dplyr::all_of(trt_var), "display") |>
			tidyr::pivot_wider(
				names_from = dplyr::all_of(trt_var),
				values_from = "display",
				values_fill = "0 (0.0%)"
			) |>
			dplyr::rename(
				`System Organ Class` = "AEBODSYS",
				`Preferred Term` = "AEDECOD"
			) |>
			dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)
	}

	ft <- create_hta_table(
		summary_df,
		title = title,
		footnotes = c("Safety Population", "AEACN = 'DRUG WITHDRAWN'"),
		autofit = autofit
	)

	ClinicalTable(
		data = summary_df,
		flextable = ft,
		type = "ae_discontinuation",
		title = title
	)
}

#' @keywords internal
create_ae_table_deaths <- function(adsl, trt_var, title, autofit) {
	death_summary <- adsl |>
		dplyr::filter(.data$SAFFL == "Y") |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
		dplyr::summarise(
			N = dplyr::n(),
			n_deaths = sum(.data$DTHFL == "Y", na.rm = TRUE),
			.groups = "drop"
		) |>
		dplyr::mutate(
			pct = round(.data$n_deaths / .data$N * 100, 1),
			`Deaths n (%)` = paste0(.data$n_deaths, " (", .data$pct, "%)"),
			N = as.character(.data$N)
		) |>
		dplyr::select(dplyr::all_of(trt_var), "N", "Deaths n (%)")

	death_wide <- death_summary |>
		tidyr::pivot_longer(
			cols = c("N", "Deaths n (%)"),
			names_to = "Statistic",
			values_to = "Value"
		) |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = "Value"
		) |>
		dplyr::mutate(
			Statistic = dplyr::case_when(
				.data$Statistic == "N" ~ "Safety Population (N)",
				TRUE ~ .data$Statistic
			)
		)

	ft <- create_hta_table(
		death_wide,
		title = title,
		footnotes = c("Safety Population", "DTHFL = 'Y'"),
		autofit = autofit
	)

	ClinicalTable(
		data = death_wide,
		flextable = ft,
		type = "ae_deaths",
		title = title
	)
}

# ===========================================================================
# Utility Functions for AE Analysis
# ===========================================================================

#' Calculate AE TTE Data for a specific SOC
#'
#' Prepares time-to-event data for Kaplan-Meier analysis of adverse events.
#'
#' @param adsl ADSL data frame
#' @param adae ADAE data frame
#' @param soc SOC value to filter by
#' @param trt_var Treatment variable name
#' @return Data frame formatted for KM plotting
#' @export
calculate_ae_tte_data <- function(
	adsl,
	adae,
	soc,
	trt_var = "TRT01A"
) {
	# Define event of interest: Time to first event in target SOC
	first_event <- adae |>
		dplyr::filter(.data$AEBODSYS == soc, .data$TRTEMFL == "Y") |>
		dplyr::group_by(.data$USUBJID) |>
		dplyr::arrange(.data$ASTDY) |>
		dplyr::slice(1) |>
		dplyr::select("USUBJID", "ASTDY") |>
		dplyr::mutate(event = 1)

	# Merge with ADSL
	tte_data <- adsl |>
		dplyr::filter(.data$SAFFL == "Y") |>
		dplyr::left_join(first_event, by = "USUBJID")

	# Derive time and event
	# If TRTDURD is not available, derive it from TRTEDT and TRTSDT
	if (!"TRTDURD" %in% names(tte_data)) {
		if ("TRTEDT" %in% names(tte_data) && "TRTSDT" %in% names(tte_data)) {
			tte_data$TRTDURD <- as.numeric(tte_data$TRTEDT - tte_data$TRTSDT) +
				1
		} else {
			cli::cli_abort(
				c(
					"Cannot calculate treatment duration for time-to-event analysis",
					"x" = "TRTDURD, TRTEDT, and TRTSDT are all missing from the data"
				)
			)
		}
	}

	tte_data <- tte_data |>
		dplyr::mutate(
			event = ifelse(!is.na(.data$event), 1, 0),
			time = ifelse(.data$event == 1, .data$ASTDY, .data$TRTDURD),
			time = ifelse(is.na(.data$time), .data$TRTDURD, .data$time)
		) |>
		dplyr::filter(!is.na(.data$time), !is.na(!!rlang::sym(trt_var)))

	return(tte_data)
}

#' Create AE KM Plot for a specific SOC
#'
#' @param adsl ADSL data frame
#' @param adae ADAE data frame
#' @param soc SOC value to filter by
#' @param trt_var Treatment variable name
#' @return ClinicalPlot object
#' @export
create_ae_km_plot_for_soc <- function(
	adsl,
	adae,
	soc,
	trt_var = "TRT01A"
) {
	tte_data <- calculate_ae_tte_data(adsl, adae, soc, trt_var)

	if (nrow(tte_data) == 0) {
		return(NULL)
	}

	create_km_plot(
		data = tte_data,
		time_var = "time",
		event_var = "event",
		trt_var = trt_var,
		title = paste("Time to First", soc),
		xlab = "Days",
		ylab = "Probability of No Event",
		risk_table = TRUE
	)
}
