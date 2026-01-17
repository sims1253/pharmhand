#' @title Adverse Event Summary Tables
#' @name safety_summary
#' @description Functions for creating AE summary tables in various formats.
NULL

# Standard AEACN value for drug discontinuation (CDISC controlled terminology)
AEACN_DRUG_WITHDRAWN <- "DRUG WITHDRAWN"

# Standard AEREL values indicating relationship to study drug
AEREL_RELATED_VALUES <- c(
	"PROBABLE",
	"POSSIBLE",
	"RELATED",
	"DEFINITELY RELATED"
)

#' Safe percentage calculation
#' @keywords internal
safe_pct <- function(n, N, digits = 1) {
	dplyr::if_else(
		N == 0 | is.na(N),
		NA_real_,
		round(n / N * 100, digits)
	)
}

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
#' @param severity_levels For type="severity", severity levels ordering
#'   (character vector). Default: c("MILD", "MODERATE", "SEVERE")
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
	trt_var = ph_default("trt_var"),
	n_top = ph_default("n_top"),
	soc = NULL,
	title = NULL,
	autofit = ph_default("autofit"),
	ref_group = NULL,
	by = "pt",
	threshold = 0,
	sort_by = "incidence",
	conf_level = ph_default("conf_level"),
	include_nnh = TRUE,
	soc_order = NULL,
	severity_levels = c("MILD", "MODERATE", "SEVERE")
) {
	type <- match.arg(type)

	# Input validation with improved messages
	if (type != "deaths" && !is.data.frame(adae)) {
		ph_abort(
			"'adae' must be a data frame for type = '",
			type,
			"'. ",
			"Got: ",
			class(adae)[1]
		)
	}
	if (type == "deaths" && !is.data.frame(adsl)) {
		ph_abort(
			"'adsl' data frame is required for type = 'deaths'. ",
			"Got: ",
			if (is.null(adsl)) "NULL" else class(adsl)[1]
		)
	}
	if (type == "comparison" && !is.data.frame(adsl)) {
		ph_abort(
			"'adsl' data frame is required for type = 'comparison'. ",
			"Cannot calculate risk differences. ",
			"Got: ",
			if (is.null(adsl)) "NULL" else class(adsl)[1]
		)
	}
	if (type == "comparison" && is.null(ref_group)) {
		trt_display <- if (
			is.data.frame(adae) &&
				trt_var %in% names(adae)
		) {
			paste(unique(adae[[trt_var]]), collapse = ", ")
		} else {
			""
		}
		trt_values <- if (nchar(trt_display) > 0) {
			paste0("Available in data: ", trt_display)
		} else {
			""
		}
		ph_abort(
			"'ref_group' is required for type = 'comparison'. ",
			"Specify which treatment to use as reference. ",
			trt_values
		)
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
		ph_warn(
			"adsl not provided; deriving trt_n from subjects with TEAEs in adae. ",
			"This may underestimate the safety population ",
			"(excludes subjects without TEAEs). ",
			"Provide adsl for accurate safety population counts.",
			call. = FALSE
		)
		adae |>
			dplyr::filter(.data$TRTEMFL == "Y") |>
			dplyr::summarise(
				N = dplyr::n_distinct(.data$USUBJID),
				.by = dplyr::all_of(trt_var)
			)
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
		severity = create_ae_table_severity(
			adae,
			trt_n,
			trt_var,
			title,
			autofit,
			severity_levels
		),
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
		result <- data |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.by = dplyr::all_of(trt_var)
			) |>
			dplyr::left_join(trt_n, by = trt_var) |>
			dplyr::mutate(
				pct = dplyr::if_else(
					.data$N == 0,
					NA_real_,
					round(.data$n / .data$N * 100, 1)
				)
			)

		result |>
			dplyr::mutate(Category = category_label)
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
				.data$AEREL %in% AEREL_RELATED_VALUES
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
		dplyr::mutate(
			value = dplyr::case_when(
				is.na(.data$pct) ~ "--",
				TRUE ~ sprintf("%d (%.1f%%)", .data$n, .data$pct)
			)
		) |>
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
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.by = c(dplyr::all_of(trt_var), "AEBODSYS")
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = safe_pct(.data$n_subj, .data$N),
			display = dplyr::if_else(
				is.na(.data$pct),
				paste0(.data$n_subj, " (--)"),
				paste0(.data$n_subj, " (", .data$pct, "%)")
			)
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
		observed_socs <- unique(soc_summary$`System Organ Class`)
		soc_levels <- c(
			soc_order[soc_order %in% observed_socs],
			setdiff(observed_socs, soc_order)
		)
		soc_summary <- soc_summary |>
			dplyr::mutate(
				`System Organ Class` = factor(
					.data$`System Organ Class`,
					levels = soc_levels
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
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.by = c(dplyr::all_of(trt_var), "AEBODSYS", "AEDECOD")
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = safe_pct(.data$n_subj, .data$N),
			display = dplyr::if_else(
				is.na(.data$pct),
				paste0(.data$n_subj, " (--)"),
				paste0(.data$n_subj, " (", .data$pct, "%)")
			)
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
		observed_socs <- unique(soc_pt_summary$`System Organ Class`)
		soc_levels <- c(
			soc_order[soc_order %in% observed_socs],
			setdiff(observed_socs, soc_order)
		)
		soc_pt_summary <- soc_pt_summary |>
			dplyr::mutate(
				`System Organ Class` = factor(
					.data$`System Organ Class`,
					levels = soc_levels
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
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.by = c(dplyr::all_of(trt_var), "AEDECOD")
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = safe_pct(.data$n_subj, .data$N),
			display = dplyr::if_else(
				is.na(.data$pct),
				paste0(.data$n_subj, " (--)"),
				paste0(.data$n_subj, " (", .data$pct, "%)")
			)
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
		dplyr::summarise(
			n = dplyr::n_distinct(.data$USUBJID),
			.by = "AEDECOD"
		) |>
		dplyr::arrange(dplyr::desc(.data$n)) |>
		dplyr::slice_head(n = n_top) |>
		dplyr::pull(.data$AEDECOD)

	common_summary <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y", .data$AEDECOD %in% top_pts) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.by = c(dplyr::all_of(trt_var), "AEBODSYS", "AEDECOD")
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = safe_pct(.data$n_subj, .data$N),
			display = dplyr::if_else(
				is.na(.data$pct),
				paste0(.data$n_subj, " (--)"),
				paste0(.data$n_subj, " (", .data$pct, "%)")
			)
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
create_ae_table_severity <- function(
	adae,
	trt_n,
	trt_var,
	title,
	autofit,
	severity_levels = c("MILD", "MODERATE", "SEVERE")
) {
	# Validate AESEV values match expected levels
	observed_sev <- unique(adae$AESEV[adae$TRTEMFL == "Y"])
	unmatched <- setdiff(observed_sev, c(severity_levels, NA))
	if (length(unmatched) > 0) {
		ph_warn(
			sprintf(
				"AESEV values %s not in severity_levels and will be treated as NA",
				paste(unmatched, collapse = ", ")
			)
		)
	}

	severity_summary <- adae |>
		dplyr::filter(.data$TRTEMFL == "Y") |>
		dplyr::mutate(
			AESEV_ord = factor(
				.data$AESEV,
				levels = severity_levels,
				ordered = TRUE
			)
		) |>
		dplyr::summarise(
			max_sev = as.character(max(.data$AESEV_ord, na.rm = TRUE)),
			.by = c(dplyr::all_of(trt_var), "USUBJID")
		) |>
		dplyr::mutate(
			max_sev = dplyr::if_else(
				.data$max_sev == "-Inf",
				NA_character_,
				.data$max_sev
			)
		) |>
		dplyr::filter(!is.na(.data$max_sev)) |>
		dplyr::summarise(
			n = dplyr::n(),
			.by = c(dplyr::all_of(trt_var), "max_sev")
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = safe_pct(.data$n, .data$N),
			display = dplyr::if_else(
				is.na(.data$pct),
				paste0(.data$n, " (--)"),
				paste0(.data$n, " (", .data$pct, "%)")
			)
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
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.by = c(dplyr::all_of(trt_var), "AEREL")
		) |>
		dplyr::left_join(trt_n, by = trt_var) |>
		dplyr::mutate(
			pct = safe_pct(.data$n_subj, .data$N),
			display = dplyr::if_else(
				is.na(.data$pct),
				paste0(.data$n_subj, " (--)"),
				paste0(.data$n_subj, " (", .data$pct, "%)")
			)
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
			dplyr::summarise(
				n_subj = dplyr::n_distinct(.data$USUBJID),
				.by = c(dplyr::all_of(trt_var), "AEBODSYS", "AEDECOD")
			) |>
			dplyr::left_join(trt_n, by = trt_var) |>
			dplyr::mutate(
				pct = safe_pct(.data$n_subj, .data$N),
				display = dplyr::if_else(
					is.na(.data$pct),
					paste0(.data$n_subj, " (--)"),
					paste0(.data$n_subj, " (", .data$pct, "%)")
				)
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
			dplyr::summarise(
				n_subj = dplyr::n_distinct(.data$USUBJID),
				.by = c(dplyr::all_of(trt_var), "AEBODSYS", "AEDECOD")
			) |>
			dplyr::left_join(trt_n, by = trt_var) |>
			dplyr::mutate(
				pct = safe_pct(.data$n_subj, .data$N),
				display = dplyr::if_else(
					is.na(.data$pct),
					paste0(.data$n_subj, " (--)"),
					paste0(.data$n_subj, " (", .data$pct, "%)")
				)
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
		dplyr::summarise(
			N = dplyr::n(),
			n_deaths = sum(.data$DTHFL == "Y", na.rm = TRUE),
			.by = dplyr::all_of(trt_var)
		) |>
		dplyr::mutate(
			pct = safe_pct(.data$n_deaths, .data$N),
			`Deaths n (%)` = dplyr::if_else(
				is.na(.data$pct),
				paste0(.data$n_deaths, " (--)"),
				paste0(.data$n_deaths, " (", .data$pct, "%)")
			),
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
