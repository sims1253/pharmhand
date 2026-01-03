#' Safety Analysis Tables
#'
#' Functions for safety tables: AE overviews, SOC/PT summaries, SAEs.
#'
#' @name safety_tables
#' @keywords internal
NULL

# Standard AEACN value for drug discontinuation (CDISC controlled terminology)
AEACN_DRUG_WITHDRAWN <- "DRUG WITHDRAWN"

#' Create Adverse Event Summary Table
#'
#' Generate AE summary tables for clinical study reports.
#'
#' @param adae ADAE data frame (ADaM Adverse Events dataset)
#' @param adsl ADSL data frame (optional, required for some table types
#'   like "deaths", "comparison", or for denominator calculation)
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
#'     \item "comparison" - AE comparison with RD/RR (requires adsl)
#'   }
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param n_top For type="common", number of top PTs to show (default: 15)
#' @param soc For type="pt", filter to specific SOC (optional)
#' @param title Table title (auto-generated if NULL)
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @param ref_group For type="comparison", the reference group for comparisons
#' @param by For type="comparison", grouping level: "soc", "pt", or "overall"
#' @param threshold For type="comparison", minimum incidence pct (default: 0)
#' @param sort_by For type="comparison", sort by "rd", "rr", or "incidence"
#' @param conf_level For type="comparison", confidence level (default: 0.95)
#' @param include_nnh For type="comparison", include NNH column (default: TRUE)
#' @param soc_order For type="soc" or type="soc_pt", custom ordering of SOCs
#'   (character vector). If NULL, SOCs are sorted alphabetically (default: NULL)
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' # AE Overview
#' overview <- create_ae_summary_table(adae, adsl, type = "overview")
#'
#' # SOC table
#' soc_table <- create_ae_summary_table(adae, adsl, type = "soc")
#'
#' # SOC/PT hierarchical table
#' soc_pt <- create_ae_summary_table(adae, adsl, type = "soc_pt")
#'
#' # Most common AEs (top 20)
#' common <- create_ae_summary_table(adae, adsl, type = "common", n_top = 20)
#'
#' # SAE table
#' sae <- create_ae_summary_table(adae, adsl, type = "sae")
#'
#' # AE comparison with risk differences
#' comparison <- create_ae_summary_table(
#'   adae, adsl,
#'   type = "comparison",
#'   ref_group = "Placebo",
#'   by = "pt",
#'   threshold = 5
#' )
#'
#' # SOC table with custom ordering
#' soc_ordered <- create_ae_summary_table(
#'   adae, adsl,
#'   type = "soc",
#'   soc_order = c(
#'     "Infections",
#'     "Nervous system disorders",
#'     "Gastrointestinal disorders"
#'   )
#' )
#' }
create_ae_summary_table <- function(
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
		"deaths",
		"comparison"
	),
	trt_var = "TRT01P",
	n_top = 15,
	soc = NULL,
	title = NULL,
	autofit = TRUE,
	ref_group = NULL,
	by = "pt",
	threshold = 0,
	sort_by = "incidence",
	conf_level = 0.95,
	include_nnh = TRUE,
	soc_order = NULL
) {
	type <- match.arg(type)

	# Input validation
	if (!type %in% c("deaths") && !is.data.frame(adae)) {
		assert_data_frame(adae, "adae")
	}
	if (type == "deaths" && !is.data.frame(adsl)) {
		assert_data_frame(adsl, "adsl")
	}
	if (type == "comparison" && !is.data.frame(adsl)) {
		ph_abort("'adsl' is required for type = 'comparison'", call. = FALSE)
	}
	if (type == "comparison" && is.null(ref_group)) {
		ph_abort("'ref_group' is required for type = 'comparison'", call. = FALSE)
	}

	# Validate required columns when deriving trt_n from adae
	if (is.null(adsl) || !is.data.frame(adsl)) {
		required_cols <- c("TRTEMFL", "USUBJID", trt_var)
		missing_cols <- setdiff(required_cols, names(adae))
		if (length(missing_cols) > 0) {
			ph_abort(
				sprintf(
					"Column(s) %s required in 'adae' when 'adsl' is not provided",
					paste(missing_cols, collapse = ", ")
				)
			)
		}
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

	# Auto-generate title if not provided (for non-comparison types)
	# comparison type handles its own title generation
	if (is.null(title) && type != "comparison") {
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
		soc = create_ae_table_soc(adae, trt_n, trt_var, title, autofit, soc_order),
		soc_pt = create_ae_table_soc_pt(
			adae,
			trt_n,
			trt_var,
			title,
			autofit,
			soc_order
		),
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
		deaths = create_ae_table_deaths(adsl, trt_var, title, autofit),
		comparison = create_ae_comparison_table(
			adae = adae,
			adsl = adsl,
			ref_group = ref_group,
			trt_var = trt_var,
			by = by,
			threshold = threshold,
			sort_by = sort_by,
			conf_level = conf_level,
			include_nnh = include_nnh,
			title = title,
			autofit = autofit
		)
	)

	result
}

# Internal handler functions for create_ae_summary_table ----

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
			dplyr::filter(.data$TRTEMFL == "Y", .data$AEACN == AEACN_DRUG_WITHDRAWN)
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
		ph_warn("No adverse event data found matching criteria", call. = FALSE)
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
create_ae_table_soc <- function(
	adae,
	trt_n,
	trt_var,
	title,
	autofit,
	soc_order = NULL
) {
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
		dplyr::rename(`System Organ Class` = "AEBODSYS")

	# Apply custom SOC ordering if provided
	if (!is.null(soc_order)) {
		soc_summary <- soc_summary |>
			dplyr::mutate(
				`System Organ Class` = factor(
					.data$`System Organ Class`,
					levels = soc_order
				)
			) |>
			dplyr::arrange(.data$`System Organ Class`) |>
			dplyr::mutate(
				`System Organ Class` = as.character(.data$`System Organ Class`)
			)
	} else {
		soc_summary <- soc_summary |>
			dplyr::arrange(.data$`System Organ Class`)
	}

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
create_ae_table_soc_pt <- function(
	adae,
	trt_n,
	trt_var,
	title,
	autofit,
	soc_order = NULL
) {
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
		)

	# Apply custom SOC ordering if provided
	if (!is.null(soc_order)) {
		soc_pt_summary <- soc_pt_summary |>
			dplyr::mutate(
				`System Organ Class` = factor(
					.data$`System Organ Class`,
					levels = soc_order
				)
			) |>
			dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`) |>
			dplyr::mutate(
				`System Organ Class` = as.character(.data$`System Organ Class`)
			)
	} else {
		soc_pt_summary <- soc_pt_summary |>
			dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)
	}

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
	# Shows all AEREL categories including NA/unknown
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
		dplyr::filter(.data$TRTEMFL == "Y", .data$AEACN == AEACN_DRUG_WITHDRAWN)

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

# Utility Functions for AE Analysis ----

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
	trt_var = "TRT01P"
) {
	assert_data_frame(adsl, "adsl")
	assert_data_frame(adae, "adae")

	adae_cols <- c("AEBODSYS", "TRTEMFL", "USUBJID", "ASTDY")
	missing_adae <- setdiff(adae_cols, names(adae))
	if (length(missing_adae) > 0) {
		ph_abort(
			paste0(
				"'adae' is missing required column(s): ",
				paste(missing_adae, collapse = ", ")
			),
			call. = FALSE
		)
	}

	adsl_cols <- c("SAFFL", "USUBJID", trt_var)
	missing_adsl <- setdiff(adsl_cols, names(adsl))
	if (length(missing_adsl) > 0) {
		ph_abort(
			paste0(
				"'adsl' is missing required column(s): ",
				paste(missing_adsl, collapse = ", ")
			),
			call. = FALSE
		)
	}

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
			ph_abort(
				paste0(
					"Cannot calculate treatment duration for time-to-event analysis. ",
					"TRTDURD, TRTEDT, and TRTSDT are all missing from the data"
				),
				call. = FALSE
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
	trt_var = "TRT01P"
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

#' Create Time-to-First AE Analysis
#'
#' Time-to-event analysis for first occurrence of adverse events.
#'
#' @param adae ADAE data frame with time variable (e.g., ASTDY)
#' @param adsl ADSL data frame for denominators and censoring
#' @param ae_filter Expression to filter specific AEs
#'   (e.g., AEBODSYS == "Infections")
#' @param trt_var Treatment variable (default: "TRT01P")
#' @param ref_group Reference group for HR calculation
#' @param time_var Time variable in ADAE (default: "ASTDY")
#' @param censor_var Censoring variable in ADSL (default: "TRTDURD")
#' @param conf_level Confidence level (default: 0.95)
#' @param title Table title
#' @param autofit Logical (default: TRUE)
#'
#' @return List with:
#'   - table: ClinicalTable with KM estimates
#'   - plot: ClinicalPlot with KM curves
#'   - hr: Hazard ratio from Cox model
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- create_time_to_first_ae(
#'   adae = adae,
#'   adsl = adsl,
#'   ae_filter = AEBODSYS == "Infections",
#'   ref_group = "Placebo"
#' )
#' }
create_time_to_first_ae <- function(
	adae,
	adsl,
	ae_filter = NULL,
	trt_var = "TRT01P",
	ref_group = NULL,
	time_var = "ASTDY",
	censor_var = "TRTDURD",
	conf_level = 0.95,
	title = NULL,
	autofit = TRUE
) {
	assert_data_frame(adae, "adae")
	assert_data_frame(adsl, "adsl")
	assert_character_scalar(trt_var, "trt_var")
	assert_character_scalar(time_var, "time_var")
	assert_character_scalar(censor_var, "censor_var")
	assert_numeric_scalar(conf_level, "conf_level")
	assert_in_range(conf_level, 0, 1, "conf_level")

	adae_cols <- c("USUBJID", "TRTEMFL", time_var)
	missing_adae <- setdiff(adae_cols, names(adae))
	if (length(missing_adae) > 0) {
		ph_abort(
			paste0(
				"'adae' is missing required column(s): ",
				paste(missing_adae, collapse = ", ")
			),
			call. = FALSE
		)
	}

	adsl_cols <- c("SAFFL", "USUBJID", trt_var)
	missing_adsl <- setdiff(adsl_cols, names(adsl))
	if (length(missing_adsl) > 0) {
		ph_abort(
			paste0(
				"'adsl' is missing required column(s): ",
				paste(missing_adsl, collapse = ", ")
			),
			call. = FALSE
		)
	}

	ae_filter_quo <- rlang::enquo(ae_filter)
	adae_filtered <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y")

	if (!rlang::quo_is_null(ae_filter_quo)) {
		adae_filtered <- adae_filtered |>
			dplyr::filter(!!ae_filter_quo)
	}

	first_event <- adae_filtered |>
		dplyr::group_by(.data$USUBJID) |>
		dplyr::arrange(.data[[time_var]]) |>
		dplyr::slice(1) |>
		dplyr::transmute(
			USUBJID = .data$USUBJID,
			event_time = .data[[time_var]],
			event = 1
		)

	tte_data <- adsl |>
		dplyr::filter(.data$SAFFL == "Y") |>
		dplyr::left_join(first_event, by = "USUBJID")

	if (!censor_var %in% names(tte_data)) {
		if (
			censor_var == "TRTDURD" &&
				"TRTEDT" %in% names(tte_data) &&
				"TRTSDT" %in% names(tte_data)
		) {
			tte_data[[censor_var]] <- as.numeric(
				tte_data$TRTEDT - tte_data$TRTSDT
			) +
				1
		} else {
			ph_abort(
				paste0(
					"Cannot calculate censoring time for time-to-event analysis. ",
					"Column '",
					censor_var,
					"' is missing from 'adsl'"
				),
				call. = FALSE
			)
		}
	}

	tte_data <- tte_data |>
		dplyr::mutate(
			event = ifelse(!is.na(.data$event), 1, 0),
			time = ifelse(.data$event == 1, .data$event_time, .data[[censor_var]]),
			time = ifelse(is.na(.data$time), .data[[censor_var]], .data$time)
		) |>
		dplyr::filter(!is.na(.data$time), !is.na(.data[[trt_var]]))

	if (nrow(tte_data) == 0) {
		ph_abort(
			"No subjects available for time-to-first AE analysis",
			call. = FALSE
		)
	}

	if (is.null(title)) {
		title <- "Time to First Adverse Event"
	}

	# PH checks are intentionally disabled for time-to-first AE endpoints
	# because AE data often have short follow-up, high rates of tied events,
	# and competing risks that may violate PH assumptions.
	tte_table <- create_tte_summary_table(
		data = tte_data,
		time_var = "time",
		event_var = "event",
		trt_var = trt_var,
		ref_group = ref_group,
		conf_level = conf_level,
		check_ph = FALSE,
		time_unit = "days",
		title = title,
		autofit = autofit
	)

	tte_table@type <- "ae_time_to_first"

	km_plot <- create_km_plot(
		data = tte_data,
		time_var = "time",
		event_var = "event",
		trt_var = trt_var,
		title = title,
		xlab = "Days",
		ylab = "Probability of No Event",
		risk_table = TRUE,
		conf_level = conf_level
	)

	list(
		table = tte_table,
		plot = km_plot,
		hr = tte_table@metadata$cox_fit
	)
}

# Risk Difference/Risk Ratio Functions ----

#' Calculate Risk Difference and Confidence Interval for AE
#'
#' Calculates risk difference, risk ratio, and associated confidence intervals
#' and p-values for comparing adverse event incidence between two groups.
#' Uses Wald method for RD CI and log-transformation for RR CI.
#'
#' @param n1 Number of subjects with event in treatment group
#' @param N1 Total subjects in treatment group
#' @param n2 Number of subjects with event in reference group
#' @param N2 Total subjects in reference group
#' @param conf_level Confidence level (default: 0.95)
#'
#' @return List with rd (risk difference), rd_lower, rd_upper, rr (risk ratio),
#'   rr_lower, rr_upper, p_value
#'
#' @importFrom stats qnorm fisher.test chisq.test
#' @keywords internal
calculate_ae_risk_difference <- function(n1, N1, n2, N2, conf_level = 0.95) {
	p1 <- n1 / N1
	p2 <- n2 / N2

	# Risk Difference
	rd <- p1 - p2

	# Standard error for RD (Wald method)
	se_rd <- sqrt(p1 * (1 - p1) / N1 + p2 * (1 - p2) / N2)

	z <- qnorm((1 + conf_level) / 2)
	rd_lower <- rd - z * se_rd
	rd_upper <- rd + z * se_rd

	# Risk Ratio (with continuity correction for zeros)
	if (p1 == 0 && p2 == 0) {
		# Both proportions are zero - RR is undefined but we can report NA
		rr <- NA_real_
		rr_lower <- NA_real_
		rr_upper <- NA_real_
	} else if (p2 == 0) {
		p2_adj <- 0.5 / N2
		p1_adj <- (n1 + 0.5) / (N1 + 1)
		rr <- p1_adj / p2_adj
		log_rr <- log(rr)
		se_log_rr <- sqrt(
			(1 - p1_adj) / (N1 * p1_adj) + (1 - p2_adj) / (N2 * p2_adj)
		)
		rr_lower <- exp(log_rr - z * se_log_rr)
		rr_upper <- exp(log_rr + z * se_log_rr)
	} else if (p1 == 0) {
		p1_adj <- 0.5 / N1
		p2_adj <- (n2 + 0.5) / (N2 + 1)
		rr <- p1_adj / p2_adj
		log_rr <- log(rr)
		se_log_rr <- sqrt(
			(1 - p1_adj) / (N1 * p1_adj) + (1 - p2_adj) / (N2 * p2_adj)
		)
		rr_lower <- exp(log_rr - z * se_log_rr)
		rr_upper <- exp(log_rr + z * se_log_rr)
	} else {
		p1_adj <- p1
		p2_adj <- p2
		rr <- p1_adj / p2_adj
		log_rr <- log(rr)
		se_log_rr <- sqrt(
			(1 - p1_adj) / (N1 * p1_adj) + (1 - p2_adj) / (N2 * p2_adj)
		)
		rr_lower <- exp(log_rr - z * se_log_rr)
		rr_upper <- exp(log_rr + z * se_log_rr)
	}

	# P-value from chi-square or Fisher's exact (for small counts)
	cont_table <- matrix(c(n1, N1 - n1, n2, N2 - n2), nrow = 2, byrow = TRUE)
	if (any(cont_table < 5)) {
		p_value <- tryCatch(
			fisher.test(cont_table)$p.value,
			error = function(e) NA_real_
		)
	} else {
		p_value <- tryCatch(
			chisq.test(cont_table, correct = FALSE)$p.value,
			error = function(e) NA_real_
		)
	}

	list(
		rd = rd,
		rd_lower = rd_lower,
		rd_upper = rd_upper,
		rr = rr,
		rr_lower = rr_lower,
		rr_upper = rr_upper,
		p_value = p_value
	)
}

#' Create AE Comparison Table with Risk Differences
#'
#' Generate AE table with statistical comparisons (risk difference, risk ratio)
#' between treatment groups. Essential for GBA/AMNOG safety assessments.
#'
#' @param adae ADAE data frame
#' @param adsl ADSL data frame for denominators
#' @param ref_group Character. Reference (control) group for comparison
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param by Character. Grouping level: "soc", "pt", or "overall"
#'   (default: "pt")
#' @param threshold Numeric. Minimum incidence pct in any group (default: 0)
#' @param sort_by Character. Sort by "rd", "rr", or "incidence"
#'   (default: "incidence")
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param include_nnh Logical. Include NNH column (default: TRUE)
#' @param title Table title (auto-generated if NULL)
#' @param autofit Logical (default: TRUE)
#'
#' @return ClinicalTable with columns for each group's n(%), RD, 95% CI, NNH,
#'   RR, p-value
#' @export
#'
#' @examples
#' \dontrun{
#' # AE comparison by PT
#' ae_comp <- create_ae_comparison_table(
#'   adae, adsl,
#'   ref_group = "Placebo",
#'   by = "pt"
#' )
#'
#' # AE comparison by SOC with 5% threshold
#' ae_comp_soc <- create_ae_comparison_table(
#'   adae, adsl,
#'   ref_group = "Placebo",
#'   by = "soc",
#'   threshold = 5
#' )
#' }
create_ae_comparison_table <- function(
	adae,
	adsl,
	ref_group,
	trt_var = "TRT01P",
	by = c("pt", "soc", "overall"),
	threshold = 0,
	sort_by = c("incidence", "rd", "rr"),
	conf_level = 0.95,
	include_nnh = TRUE,
	title = NULL,
	autofit = TRUE
) {
	by <- match.arg(by)
	sort_by <- match.arg(sort_by)

	assert_data_frame(adae, "adae")
	assert_data_frame(adsl, "adsl")

	if (is.null(ref_group)) {
		ph_abort(
			"'ref_group' must be provided for AE comparison tables",
			call. = FALSE
		)
	}

	required_adae_cols <- c("TRTEMFL", "USUBJID", trt_var)
	if (by == "soc") {
		required_adae_cols <- c(required_adae_cols, "AEBODSYS")
	} else if (by == "pt") {
		required_adae_cols <- c(required_adae_cols, "AEDECOD")
	}
	missing_adae <- setdiff(required_adae_cols, names(adae))
	if (length(missing_adae) > 0) {
		ph_abort(
			paste0(
				"'adae' is missing required column(s): ",
				paste(missing_adae, collapse = ", ")
			),
			call. = FALSE
		)
	}

	required_adsl_cols <- c("SAFFL", "USUBJID", trt_var)
	missing_adsl <- setdiff(required_adsl_cols, names(adsl))
	if (length(missing_adsl) > 0) {
		ph_abort(
			paste0(
				"'adsl' is missing required column(s): ",
				paste(missing_adsl, collapse = ", ")
			),
			call. = FALSE
		)
	}
	# Get treatment counts from ADSL
	trt_n <- adsl |>
		dplyr::filter(.data$SAFFL == "Y") |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
		dplyr::summarise(N = dplyr::n_distinct(.data$USUBJID), .groups = "drop")

	# Validate ref_group
	trt_levels <- unique(trt_n[[trt_var]])
	if (!ref_group %in% trt_levels) {
		ph_abort(
			paste0(
				"'ref_group' must be one of the treatment groups. Available groups: ",
				paste(trt_levels, collapse = ", "),
				". Provided: ",
				ref_group
			),
			call. = FALSE
		)
	}

	# Filter to TEAEs
	teae <- adae |> dplyr::filter(.data$TRTEMFL == "Y")

	# Calculate incidence by grouping variable
	if (by == "overall") {
		ae_counts <- teae |>
			dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			) |>
			dplyr::mutate(term = "Any TEAE")
		group_var <- "term"
	} else if (by == "soc") {
		ae_counts <- teae |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(trt_var)),
				.data$AEBODSYS
			) |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			) |>
			dplyr::rename(term = "AEBODSYS")
		group_var <- "term"
	} else {
		ae_counts <- teae |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(trt_var)),
				.data$AEDECOD
			) |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			) |>
			dplyr::rename(term = "AEDECOD")
		group_var <- "term"
	}

	# Join with trt_n to get denominators and calculate percentages
	ae_counts <- ae_counts |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(pct = .data$n / .data$N * 100)

	# Apply threshold filter
	if (threshold > 0) {
		terms_above_threshold <- ae_counts |>
			dplyr::group_by(.data$term) |>
			dplyr::summarise(
				max_pct = max(.data$pct, na.rm = TRUE),
				.groups = "drop"
			) |>
			dplyr::filter(.data$max_pct >= threshold) |>
			dplyr::pull(.data$term)

		ae_counts <- ae_counts |>
			dplyr::filter(.data$term %in% terms_above_threshold)
	}

	if (nrow(ae_counts) == 0) {
		ph_warn(
			"No adverse events meet the specified threshold criteria",
			call. = FALSE
		)
		return(NULL)
	}

	# Pivot to wide format for comparison
	ae_wide <- ae_counts |>
		dplyr::select("term", dplyr::all_of(trt_var), "n", "N", "pct") |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = c("n", "N", "pct"),
			values_fill = list(n = 0, pct = 0)
		)

	# Get treatment groups (excluding reference)
	trt_groups <- setdiff(trt_levels, ref_group)

	# TODO(#196): This stub will be used for the diff feature
	# when that post-1.0 feature is implemented. Currently unused but reserved.
	comparison_results <- list() # nolint: object_usage_linter

	# Calculate risk differences and ratios for each treatment vs reference

	for (trt in trt_groups) {
		n_trt_col <- paste0("n_", trt)
		N_trt_col <- paste0("N_", trt)
		n_ref_col <- paste0("n_", ref_group)
		N_ref_col <- paste0("N_", ref_group)
		pct_trt_col <- paste0("pct_", trt)
		pct_ref_col <- paste0("pct_", ref_group)

		# Ensure columns exist (fill with 0 if missing)
		if (!n_trt_col %in% names(ae_wide)) {
			ae_wide[[n_trt_col]] <- 0
		}
		if (!n_ref_col %in% names(ae_wide)) {
			ae_wide[[n_ref_col]] <- 0
		}

		# Get N values from trt_n
		N_trt <- trt_n |>
			dplyr::filter(.data[[trt_var]] == trt) |>
			dplyr::pull(.data$N)
		N_ref <- trt_n |>
			dplyr::filter(.data[[trt_var]] == ref_group) |>
			dplyr::pull(.data$N)

		# Calculate statistics for each term
		stats_list <- lapply(seq_len(nrow(ae_wide)), function(i) {
			n1 <- ae_wide[[n_trt_col]][i]
			n2 <- ae_wide[[n_ref_col]][i]

			# Handle missing values
			if (is.na(n1)) {
				n1 <- 0
			}
			if (is.na(n2)) {
				n2 <- 0
			}

			calculate_ae_risk_difference(n1, N_trt, n2, N_ref, conf_level)
		})

		# Extract statistics
		ae_wide[[paste0("rd_", trt)]] <- sapply(stats_list, `[[`, "rd")
		ae_wide[[paste0("rd_lower_", trt)]] <- sapply(stats_list, `[[`, "rd_lower")
		ae_wide[[paste0("rd_upper_", trt)]] <- sapply(stats_list, `[[`, "rd_upper")

		if (include_nnh) {
			nnh_list <- lapply(stats_list, function(stats) {
				calculate_nnt(
					rd = stats$rd,
					rd_lower = stats$rd_lower,
					rd_upper = stats$rd_upper,
					event_type = "harm"
				)
			})

			ae_wide[[paste0("nnh_", trt)]] <- vapply(
				nnh_list,
				function(stats) abs(stats$nnt),
				numeric(1)
			)
			ae_wide[[paste0("nnh_lower_", trt)]] <- vapply(
				nnh_list,
				function(stats) abs(stats$nnt_lower),
				numeric(1)
			)
			ae_wide[[paste0("nnh_upper_", trt)]] <- vapply(
				nnh_list,
				function(stats) abs(stats$nnt_upper),
				numeric(1)
			)
			ae_wide[[paste0("nnh_estimable_", trt)]] <- vapply(
				nnh_list,
				function(stats) !isTRUE(stats$ci_crosses_zero),
				logical(1)
			)
		}

		ae_wide[[paste0("rr_", trt)]] <- sapply(stats_list, `[[`, "rr")
		ae_wide[[paste0("rr_lower_", trt)]] <- sapply(stats_list, `[[`, "rr_lower")
		ae_wide[[paste0("rr_upper_", trt)]] <- sapply(stats_list, `[[`, "rr_upper")
		ae_wide[[paste0("pvalue_", trt)]] <- sapply(stats_list, `[[`, "p_value")
	}

	# Sort by specified criterion
	if (sort_by == "rd" && length(trt_groups) > 0) {
		sort_col <- paste0("rd_", trt_groups[1])
		ae_wide <- ae_wide |>
			dplyr::arrange(dplyr::desc(abs(.data[[sort_col]])))
	} else if (sort_by == "rr" && length(trt_groups) > 0) {
		sort_col <- paste0("rr_", trt_groups[1])
		ae_wide <- ae_wide |>
			dplyr::arrange(dplyr::desc(.data[[sort_col]]))
	} else {
		# Sort by maximum incidence across groups
		pct_cols <- names(ae_wide)[grepl("^pct_", names(ae_wide))]
		ae_wide <- ae_wide |>
			dplyr::rowwise() |>
			dplyr::mutate(
				max_incidence = max(
					dplyr::c_across(dplyr::all_of(pct_cols)),
					na.rm = TRUE
				)
			) |>
			dplyr::ungroup() |>
			dplyr::arrange(dplyr::desc(.data$max_incidence)) |>
			dplyr::select(-"max_incidence")
	}

	# Format output table
	ci_level_pct <- round(conf_level * 100)
	output_df <- data.frame(Term = ae_wide$term, stringsAsFactors = FALSE)

	# Add reference group column
	n_ref_col <- paste0("n_", ref_group)
	pct_ref_col <- paste0("pct_", ref_group)
	N_ref <- trt_n |>
		dplyr::filter(.data[[trt_var]] == ref_group) |>
		dplyr::pull(.data$N)

	output_df[[paste0(ref_group, "\nn/N (%)")]] <- sprintf(
		"%d/%d (%.1f%%)",
		ae_wide[[n_ref_col]],
		N_ref,
		ae_wide[[pct_ref_col]]
	)

	# Add treatment group columns with comparisons
	for (trt in trt_groups) {
		n_trt_col <- paste0("n_", trt)
		pct_trt_col <- paste0("pct_", trt)
		N_trt <- trt_n |>
			dplyr::filter(.data[[trt_var]] == trt) |>
			dplyr::pull(.data$N)

		output_df[[paste0(trt, "\nn/N (%)")]] <- sprintf(
			"%d/%d (%.1f%%)",
			ae_wide[[n_trt_col]],
			N_trt,
			ae_wide[[pct_trt_col]]
		)

		# Risk Difference
		rd_col <- paste0("rd_", trt)
		rd_lower_col <- paste0("rd_lower_", trt)
		rd_upper_col <- paste0("rd_upper_", trt)
		output_df[[sprintf(
			"RD %s vs %s\n(%d%% CI)",
			trt,
			ref_group,
			ci_level_pct
		)]] <- sprintf(
			"%.1f%% (%.1f%%, %.1f%%)",
			ae_wide[[rd_col]] * 100,
			ae_wide[[rd_lower_col]] * 100,
			ae_wide[[rd_upper_col]] * 100
		)

		# Number Needed to Harm
		if (include_nnh) {
			nnh_col <- paste0("nnh_", trt)
			nnh_lower_col <- paste0("nnh_lower_", trt)
			nnh_upper_col <- paste0("nnh_upper_", trt)
			nnh_estimable_col <- paste0("nnh_estimable_", trt)
			output_df[[sprintf(
				"NNH %s vs %s\n(%d%% CI)",
				trt,
				ref_group,
				ci_level_pct
			)]] <- vapply(
				seq_len(nrow(ae_wide)),
				function(i) {
					if (!isTRUE(ae_wide[[nnh_estimable_col]][i])) {
						return("NE")
					}

					paste0(
						format_number(ae_wide[[nnh_col]][i], digits = 1),
						" (",
						format_number(ae_wide[[nnh_lower_col]][i], digits = 1),
						", ",
						format_number(ae_wide[[nnh_upper_col]][i], digits = 1),
						")"
					)
				},
				character(1)
			)
		}

		# Risk Ratio
		rr_col <- paste0("rr_", trt)
		rr_lower_col <- paste0("rr_lower_", trt)
		rr_upper_col <- paste0("rr_upper_", trt)
		output_df[[sprintf(
			"RR %s vs %s\n(%d%% CI)",
			trt,
			ref_group,
			ci_level_pct
		)]] <- sprintf(
			"%.2f (%.2f, %.2f)",
			ae_wide[[rr_col]],
			ae_wide[[rr_lower_col]],
			ae_wide[[rr_upper_col]]
		)

		# P-value
		pvalue_col <- paste0("pvalue_", trt)
		output_df[[sprintf(
			"P-value (%s vs %s)",
			trt,
			ref_group
		)]] <- format_pvalue(ae_wide[[pvalue_col]])
	}

	# Auto-generate title if not provided
	if (is.null(title)) {
		title <- switch(
			by,
			overall = "Adverse Event Comparison",
			soc = "Adverse Events by System Organ Class with Risk Comparisons",
			pt = "Adverse Events by Preferred Term with Risk Comparisons"
		)
	}

	# Create footnotes
	definition_line <- if (include_nnh) {
		sprintf(
			paste0(
				"RD = Risk Difference, NNH = Number Needed to Harm, ",
				"RR = Risk Ratio, CI = %d%% Confidence Interval"
			),
			ci_level_pct
		)
	} else {
		sprintf(
			"RD = Risk Difference, RR = Risk Ratio, CI = %d%% Confidence Interval",
			ci_level_pct
		)
	}

	footnotes <- c(
		"Safety Population",
		definition_line,
		paste0("Reference group: ", ref_group),
		"P-values from Chi-square (or Fisher's exact when expected count < 5)"
	)

	if (include_nnh) {
		footnotes <- c(
			footnotes,
			"NNH = 1/|RD|; NE = not estimable when CI crosses zero"
		)
	}

	if (threshold > 0) {
		footnotes <- c(
			footnotes,
			sprintf("Events with incidence >= %.1f%% in any group", threshold)
		)
	}

	ft <- create_hta_table(
		output_df,
		title = title,
		footnotes = footnotes,
		autofit = autofit
	)

	ClinicalTable(
		data = output_df,
		flextable = ft,
		type = "ae_comparison",
		title = title,
		metadata = list(
			ref_group = ref_group,
			by = by,
			threshold = threshold,
			conf_level = conf_level,
			include_nnh = include_nnh
		)
	)
}

# Exposure-Adjusted Incidence Rate Functions ----

#' Calculate Exposure-Adjusted Incidence Rate
#'
#' Calculates incidence rate per X patient-years with Poisson CI.
#'
#' @param n_events Integer. Number of events
#' @param patient_years Numeric. Total patient-years of exposure
#' @param conf_level Numeric. Confidence level (default: 0.95)
#' @param per Numeric. Rate per X patient-years (default: 100)
#'
#' @return A list with:
#'   - rate: Incidence rate per X patient-years
#'   - ci_lower: Lower CI bound
#'   - ci_upper: Upper CI bound
#'   - n_events: Number of events
#'   - patient_years: Total exposure
#'
#' @importFrom stats qchisq
#' @export
#'
#' @examples
#' calculate_exposure_adjusted_rate(n_events = 4, patient_years = 2)
calculate_exposure_adjusted_rate <- function(
	n_events,
	patient_years,
	conf_level = 0.95,
	per = 100
) {
	assert_numeric_scalar(n_events, "n_events")
	if (is.na(n_events) || n_events < 0) {
		ph_abort("'n_events' must be a non-negative number")
	}

	assert_numeric_scalar(patient_years, "patient_years")
	if (is.na(patient_years) || patient_years <= 0) {
		ph_abort("'patient_years' must be a positive number")
	}

	assert_numeric_scalar(conf_level, "conf_level")
	assert_in_range(conf_level, 0, 1, "conf_level")

	assert_numeric_scalar(per, "per")
	if (is.na(per) || per <= 0) {
		ph_abort("'per' must be a positive number")
	}

	alpha <- 1 - conf_level
	rate <- (n_events / patient_years) * per

	lower_count <- if (n_events == 0) {
		0
	} else {
		stats::qchisq(alpha / 2, 2 * n_events) / 2
	}
	upper_count <- stats::qchisq(1 - alpha / 2, 2 * (n_events + 1)) / 2

	ci_lower <- (lower_count / patient_years) * per
	ci_upper <- (upper_count / patient_years) * per

	list(
		rate = rate,
		ci_lower = ci_lower,
		ci_upper = ci_upper,
		n_events = n_events,
		patient_years = patient_years
	)
}

#' Create Exposure-Adjusted AE Table
#'
#' Generate AE table with incidence rates per X patient-years.
#'
#' @param adae ADAE data frame
#' @param adsl ADSL data frame (must contain exposure duration)
#' @param exposure_var Character. Exposure duration variable
#'   (default: "TRTDURD")
#' @param trt_var Character. Treatment variable (default: "TRT01P")
#' @param by Character. "soc", "pt", or "overall" (default: "pt")
#' @param time_unit Character. "days", "weeks", "months" (default: "days")
#' @param per Numeric. Rate per X patient-years (default: 100)
#' @param conf_level Numeric. Confidence level (default: 0.95)
#' @param threshold Numeric. Minimum incidence to include (default: 0)
#' @param title Character. Table title
#' @param autofit Logical. Autofit columns (default: TRUE)
#'
#' @return ClinicalTable with IDR columns
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- create_ae_exposure_table(adae, adsl, by = "pt")
#' }
create_ae_exposure_table <- function(
	adae,
	adsl,
	exposure_var = "TRTDURD",
	trt_var = "TRT01P",
	by = c("pt", "soc", "overall"),
	time_unit = c("days", "weeks", "months"),
	per = 100,
	conf_level = 0.95,
	threshold = 0,
	title = NULL,
	autofit = TRUE
) {
	by <- match.arg(by)
	time_unit <- match.arg(time_unit)

	assert_data_frame(adae, "adae")
	assert_data_frame(adsl, "adsl")
	assert_character_scalar(exposure_var, "exposure_var")
	assert_character_scalar(trt_var, "trt_var")
	assert_numeric_scalar(per, "per")
	assert_numeric_scalar(conf_level, "conf_level")
	assert_in_range(conf_level, 0, 1, "conf_level")
	assert_numeric_scalar(threshold, "threshold")

	if (per <= 0) {
		ph_abort("'per' must be a positive number")
	}
	if (threshold < 0) {
		ph_abort("'threshold' must be a non-negative number")
	}

	required_adae_cols <- c("TRTEMFL", "USUBJID", trt_var)
	if (by == "soc") {
		required_adae_cols <- c(required_adae_cols, "AEBODSYS")
	} else if (by == "pt") {
		required_adae_cols <- c(required_adae_cols, "AEDECOD")
	}
	missing_adae <- setdiff(required_adae_cols, names(adae))
	if (length(missing_adae) > 0) {
		ph_abort(
			paste0(
				"'adae' is missing required column(s): ",
				paste(missing_adae, collapse = ", ")
			),
			call. = FALSE
		)
	}

	required_adsl_cols <- c("USUBJID", trt_var, exposure_var)
	missing_adsl <- setdiff(required_adsl_cols, names(adsl))
	if (length(missing_adsl) > 0) {
		ph_abort(
			paste0(
				"'adsl' is missing required column(s): ",
				paste(missing_adsl, collapse = ", ")
			),
			call. = FALSE
		)
	}

	adsl_exposure <- adsl
	if ("SAFFL" %in% names(adsl_exposure)) {
		adsl_exposure <- adsl_exposure |>
			dplyr::filter(.data$SAFFL == "Y")
	}

	if (nrow(adsl_exposure) == 0) {
		ph_abort("No subjects available after population filter", call. = FALSE)
	}

	exposure_values <- as.numeric(adsl_exposure[[exposure_var]])
	missing_exposure <- is.na(exposure_values)
	if (any(missing_exposure)) {
		ph_warn(
			sprintf(
				"%d subjects have missing %s and were excluded from exposure totals",
				sum(missing_exposure),
				exposure_var
			),
			call. = FALSE
		)
		adsl_exposure <- adsl_exposure[!missing_exposure, , drop = FALSE]
		exposure_values <- exposure_values[!missing_exposure]
	}

	if (nrow(adsl_exposure) == 0) {
		ph_abort("No non-missing exposure values available", call. = FALSE)
	}

	if (any(exposure_values < 0)) {
		ph_abort(
			sprintf("'%s' must be non-negative", exposure_var),
			call. = FALSE
		)
	}

	year_divisor <- switch(
		time_unit,
		days = 365.25,
		weeks = 365.25 / 7,
		months = 12
	)

	adsl_exposure$exposure_years <- exposure_values / year_divisor

	trt_levels <- if (is.factor(adsl_exposure[[trt_var]])) {
		levels(adsl_exposure[[trt_var]])
	} else {
		unique(adsl_exposure[[trt_var]])
	}

	adsl_exposure[[trt_var]] <- factor(
		adsl_exposure[[trt_var]],
		levels = trt_levels
	)

	patient_years_by_trt <- adsl_exposure |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
		dplyr::summarise(
			patient_years = sum(.data$exposure_years, na.rm = TRUE),
			.groups = "drop"
		)

	patient_years_by_trt[[trt_var]] <- factor(
		patient_years_by_trt[[trt_var]],
		levels = trt_levels
	)

	invalid_trt <- patient_years_by_trt |>
		dplyr::filter(is.na(.data$patient_years) | .data$patient_years <= 0) |>
		dplyr::pull(dplyr::all_of(trt_var))

	if (length(invalid_trt) > 0) {
		ph_abort(
			paste0(
				"Exposure totals must be positive for treatment group(s): ",
				paste(invalid_trt, collapse = ", ")
			),
			call. = FALSE
		)
	}

	teae <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y")

	if (nrow(teae) == 0) {
		ph_warn("No treatment-emergent adverse events found", call. = FALSE)
		return(NULL)
	}

	if (by == "overall") {
		event_counts <- teae |>
			dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
			dplyr::summarise(n_events = dplyr::n(), .groups = "drop") |>
			dplyr::mutate(term = "Any TEAE")
	} else if (by == "soc") {
		event_counts <- teae |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(trt_var)),
				.data$AEBODSYS
			) |>
			dplyr::summarise(n_events = dplyr::n(), .groups = "drop") |>
			dplyr::rename(term = "AEBODSYS")
	} else {
		event_counts <- teae |>
			dplyr::group_by(
				dplyr::across(dplyr::all_of(trt_var)),
				.data$AEDECOD
			) |>
			dplyr::summarise(n_events = dplyr::n(), .groups = "drop") |>
			dplyr::rename(term = "AEDECOD")
	}

	event_counts[[trt_var]] <- factor(
		event_counts[[trt_var]],
		levels = trt_levels
	)
	event_counts$term <- as.character(event_counts$term)

	terms <- unique(event_counts$term)

	all_combos <- tidyr::expand_grid(
		term = terms,
		trt_group = trt_levels
	)
	names(all_combos)[names(all_combos) == "trt_group"] <- trt_var
	all_combos[[trt_var]] <- factor(all_combos[[trt_var]], levels = trt_levels)

	event_counts <- all_combos |>
		dplyr::left_join(event_counts, by = c("term", trt_var)) |>
		dplyr::mutate(n_events = tidyr::replace_na(.data$n_events, 0))

	event_counts <- event_counts |>
		dplyr::left_join(patient_years_by_trt, by = trt_var)

	rate_stats <- lapply(seq_len(nrow(event_counts)), function(i) {
		calculate_exposure_adjusted_rate(
			n_events = event_counts$n_events[i],
			patient_years = event_counts$patient_years[i],
			conf_level = conf_level,
			per = per
		)
	})

	event_counts$rate <- vapply(rate_stats, `[[`, numeric(1), "rate")
	event_counts$ci_lower <- vapply(rate_stats, `[[`, numeric(1), "ci_lower")
	event_counts$ci_upper <- vapply(rate_stats, `[[`, numeric(1), "ci_upper")

	if (threshold > 0) {
		terms_above_threshold <- event_counts |>
			dplyr::group_by(.data$term) |>
			dplyr::summarise(
				max_rate = max(.data$rate, na.rm = TRUE),
				.groups = "drop"
			) |>
			dplyr::filter(.data$max_rate >= threshold) |>
			dplyr::pull(.data$term)

		event_counts <- event_counts |>
			dplyr::filter(.data$term %in% terms_above_threshold)
	}

	if (nrow(event_counts) == 0) {
		ph_warn(
			"No adverse events meet the specified threshold criteria",
			call. = FALSE
		)
		return(NULL)
	}

	if (by != "overall") {
		term_levels <- sort(unique(event_counts$term))
	} else {
		term_levels <- terms
	}

	event_counts <- event_counts |>
		dplyr::mutate(term = factor(.data$term, levels = term_levels)) |>
		dplyr::arrange(.data$term, .data[[trt_var]])

	ci_level_pct <- round(conf_level * 100)
	na_string <- get_na_string()
	idr_label <- sprintf("IDR per %s PY (%d%% CI)", per, ci_level_pct)

	event_counts$`Events (n)` <- as.character(event_counts$n_events)
	event_counts$`Patient-Years` <- format_number(
		event_counts$patient_years,
		digits = 2,
		na_string = na_string
	)
	event_counts[[idr_label]] <- ifelse(
		is.na(event_counts$rate) |
			is.na(event_counts$ci_lower) |
			is.na(event_counts$ci_upper),
		na_string,
		paste0(
			format_number(event_counts$rate, digits = 2, na_string = na_string),
			" (",
			format_number(event_counts$ci_lower, digits = 2, na_string = na_string),
			", ",
			format_number(event_counts$ci_upper, digits = 2, na_string = na_string),
			")"
		)
	)

	value_cols <- c("Events (n)", "Patient-Years", idr_label)

	names_glue <- paste0("{", trt_var, "}\n{.value}")

	output_df <- event_counts |>
		dplyr::select("term", dplyr::all_of(trt_var), dplyr::all_of(value_cols)) |>
		tidyr::pivot_wider(
			names_from = dplyr::all_of(trt_var),
			values_from = dplyr::all_of(value_cols),
			names_glue = names_glue
		)

	term_label <- switch(
		by,
		overall = "Term",
		soc = "System Organ Class",
		pt = "Preferred Term"
	)

	names(output_df)[names(output_df) == "term"] <- term_label
	output_df[[term_label]] <- as.character(output_df[[term_label]])

	if (is.null(title)) {
		title <- switch(
			by,
			overall = "Exposure-Adjusted Adverse Event Rates",
			soc = "Exposure-Adjusted Adverse Events by System Organ Class",
			pt = "Exposure-Adjusted Adverse Events by Preferred Term"
		)
	}

	population_label <- if ("SAFFL" %in% names(adsl)) {
		"Safety Population (SAFFL = 'Y')"
	} else {
		"Safety Population"
	}

	footnotes <- c(
		population_label,
		sprintf(
			"IDR per %s patient-years with %d%% Poisson CI",
			per,
			ci_level_pct
		),
		sprintf(
			"Patient-years derived from %s (%s)",
			exposure_var,
			time_unit
		),
		"Events counted from TEAEs (TRTEMFL = 'Y')"
	)

	if (threshold > 0) {
		footnotes <- c(
			footnotes,
			sprintf(
				"Terms with IDR >= %s per %s patient-years in any group",
				format_number(threshold, digits = 2, na_string = na_string),
				per
			)
		)
	}

	ft <- create_hta_table(
		output_df,
		title = title,
		footnotes = footnotes,
		autofit = autofit
	)

	ClinicalTable(
		data = output_df,
		flextable = ft,
		type = "ae_exposure",
		title = title,
		metadata = list(
			exposure_var = exposure_var,
			trt_var = trt_var,
			by = by,
			time_unit = time_unit,
			per = per,
			conf_level = conf_level,
			threshold = threshold
		)
	)
}
