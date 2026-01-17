#' @title Adverse Event Comparison Tables
#' @name safety_comparison
#' @description Functions for comparing AE rates between treatment groups.
NULL

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
		p2_adj <- 0.5 / (N2 + 1)
		p1_adj <- (n1 + 0.5) / (N1 + 1)
		rr <- p1_adj / p2_adj
		log_rr <- log(rr)
		se_log_rr <- sqrt(
			(1 - p1_adj) / (N1 * p1_adj) + (1 - p2_adj) / (N2 * p2_adj)
		)
		rr_lower <- exp(log_rr - z * se_log_rr)
		rr_upper <- exp(log_rr + z * se_log_rr)
	} else if (p1 == 0) {
		p1_adj <- 0.5 / (N1 + 1)
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
		dplyr::summarise(
			N = dplyr::n_distinct(.data$USUBJID),
			.by = dplyr::all_of(trt_var)
		)

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
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.by = dplyr::all_of(trt_var)
			) |>
			dplyr::mutate(term = "Any TEAE")
		group_var <- "term"
	} else if (by == "soc") {
		ae_counts <- teae |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.by = c(dplyr::all_of(trt_var), "AEBODSYS")
			) |>
			dplyr::rename(term = "AEBODSYS")
		group_var <- "term"
	} else {
		ae_counts <- teae |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.by = c(dplyr::all_of(trt_var), "AEDECOD")
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
			dplyr::summarise(
				max_pct = max(.data$pct, na.rm = TRUE),
				.by = "term"
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

			# NNH reported as positive (absolute value of NNT for harm)
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
