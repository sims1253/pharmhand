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

#' Calculate AE Comparisons for All Treatment Groups vs Reference
#'
#' Internal helper function that computes risk differences, risk ratios,
#' and associated statistics for all treatment groups compared to a reference.
#'
#' @param ae_wide Wide-format AE counts with columns for each treatment's
#'   n and N values
#' @param trt_n Treatment counts data frame
#' @param trt_groups Treatment groups to compare (excluding reference)
#' @param ref_group Reference group name
#' @param trt_var Treatment variable name
#' @param conf_level Confidence level for intervals
#' @param include_nnh Logical, include NNH calculations
#'
#' @return Modified ae_wide data frame with additional columns for statistics
#' @keywords internal
.calculate_ae_comparisons <- function(
	ae_wide,
	trt_n,
	trt_groups,
	ref_group,
	trt_var,
	conf_level,
	include_nnh
) {
	for (trt in trt_groups) {
		n_trt_col <- paste0("n_", trt)
		N_trt_col <- paste0("N_", trt)
		n_ref_col <- paste0("n_", ref_group)
		N_ref_col <- paste0("N_", ref_group)

		# Ensure columns exist (fill with 0 if missing)
		if (!n_trt_col %in% names(ae_wide)) {
			ae_wide[[n_trt_col]] <- 0
		}
		if (!n_ref_col %in% names(ae_wide)) {
			ae_wide[[n_ref_col]] <- 0
		}

		# Get N values from trt_n (with guard for empty results)
		N_trt <- trt_n |>
			dplyr::filter(.data[[trt_var]] == trt) |>
			dplyr::pull(.data$N)
		if (length(N_trt) == 0) {
			N_trt <- 0
		}

		N_ref <- trt_n |>
			dplyr::filter(.data[[trt_var]] == ref_group) |>
			dplyr::pull(.data$N)
		if (length(N_ref) == 0) {
			N_ref <- 0
		}

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

		# Extract risk difference statistics
		ae_wide[[paste0("rd_", trt)]] <- vapply(stats_list, `[[`, numeric(1), "rd")
		ae_wide[[paste0("rd_lower_", trt)]] <- vapply(
			stats_list,
			`[[`,
			numeric(1),
			"rd_lower"
		)
		ae_wide[[paste0("rd_upper_", trt)]] <- vapply(
			stats_list,
			`[[`,
			numeric(1),
			"rd_upper"
		)

		# Calculate NNH if requested
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
				function(stats) abs(stats$nnt), # nnh (main) as abs(nnt)
				numeric(1)
			)
			ae_wide[[paste0("nnh_lower_", trt)]] <- vapply(
				nnh_list,
				function(stats) {
					# Ensure nnh_lower <= nnh_upper by using pmin of absolute values
					pmin(abs(stats$nnt_lower), abs(stats$nnt_upper))
				},
				numeric(1)
			)
			ae_wide[[paste0("nnh_upper_", trt)]] <- vapply(
				nnh_list,
				function(stats) {
					# Ensure nnh_lower <= nnh_upper by using pmax of absolute values
					pmax(abs(stats$nnt_lower), abs(stats$nnt_upper))
				},
				numeric(1)
			)
			ae_wide[[paste0("nnh_estimable_", trt)]] <- vapply(
				nnh_list,
				function(stats) !isTRUE(stats$ci_crosses_zero),
				logical(1)
			)
		}

		# Extract risk ratio statistics
		ae_wide[[paste0("rr_", trt)]] <- vapply(stats_list, `[[`, numeric(1), "rr")
		ae_wide[[paste0("rr_lower_", trt)]] <- vapply(
			stats_list,
			`[[`,
			numeric(1),
			"rr_lower"
		)
		ae_wide[[paste0("rr_upper_", trt)]] <- vapply(
			stats_list,
			`[[`,
			numeric(1),
			"rr_upper"
		)
		ae_wide[[paste0("pvalue_", trt)]] <- vapply(
			stats_list,
			`[[`,
			numeric(1),
			"p_value"
		)
	}

	ae_wide
}

#' Create AE Comparison Table with Risk Differences
#'
#' Generate AE table with statistical comparisons (risk difference, risk ratio)
#' between treatment groups. Essential for GBA/AMNOG safety assessments.
#'
#' @param data ADAE data frame or ADaMData object containing ADaM Adverse Events
#'   dataset
#' @param adsl ADSL data frame or ADaMData object for denominators
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
#' @param footnotes Character vector of footnotes (optional)
#' @param theme Theme for table styling (default: "hta")
#' @param ... Additional arguments passed to [create_clinical_table()]
#'
#' @return ClinicalTable with columns for each group's n(%), RD, 95% CI, NNH,
#'   RR, p-value
#' @export
#'
#' @examples
#' \dontrun{
#' # AE comparison by PT
#' ae_comp <- create_ae_comparison_table(
#'   data = adae,
#'   adsl = adsl,
#'   ref_group = "Placebo",
#'   by = "pt"
#' )
#'
#' # AE comparison by SOC with 5% threshold
#' ae_comp_soc <- create_ae_comparison_table(
#'   data = adae,
#'   adsl = adsl,
#'   ref_group = "Placebo",
#'   by = "soc",
#'   threshold = 5
#' )
#' }
create_ae_comparison_table <- function(
	data,
	adsl,
	ref_group,
	trt_var = "TRT01P",
	by = c("pt", "soc", "overall"),
	threshold = 0,
	sort_by = c("incidence", "rd", "rr"),
	conf_level = ph_default("conf_level"),
	include_nnh = TRUE,
	title = NULL,
	footnotes = character(),
	theme = "hta",
	...
) {
	by <- match.arg(by)
	sort_by <- match.arg(sort_by)

	# Explicit check for ref_group
	if (length(ref_group) == 0 || is.null(ref_group)) {
		ph_abort("ref_group must be provided for AE comparison tables")
	}

	# Ensure ADaMData objects
	adae <- .ensure_adam_data(data, "ADAE", trt_var = trt_var)
	adsl <- .ensure_adam_data(adsl, "ADSL", trt_var = trt_var)

	# Validate ref_group
	trt_levels <- adsl@trt_levels
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

	# Get treatment counts from ADSL using computed property
	trt_n <- adsl@trt_n

	# Filter to TEAEs
	teae <- adae@filtered_data |>
		dplyr::filter(.data$TRTEMFL == "Y")

	# Get treatment groups that actually have TEAE data
	trt_levels_with_data <- teae |>
		dplyr::distinct(.data[[trt_var]]) |>
		dplyr::pull() |>
		as.character()

	# Filter trt_n to only include groups with TEAE data
	trt_n <- trt_n |>
		dplyr::filter(.data[[trt_var]] %in% trt_levels_with_data)

	# Update trt_levels
	trt_levels <- trt_levels_with_data

	# Ensure ref_group is in trt_levels_with_data
	if (!ref_group %in% trt_levels) {
		ph_abort(
			paste0(
				"Reference group '",
				ref_group,
				"' has no TEAE data. ",
				"Available groups with TEAE data: ",
				paste(trt_levels, collapse = ", ")
			),
			call. = FALSE
		)
	}

	# Calculate incidence by grouping variable
	if (by == "overall") {
		ae_counts <- teae |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.by = dplyr::all_of(trt_var)
			) |>
			dplyr::mutate(term = "Any TEAE")
	} else if (by == "soc") {
		ae_counts <- teae |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.by = c(dplyr::all_of(trt_var), "AEBODSYS")
			) |>
			dplyr::rename(term = "AEBODSYS")
	} else {
		ae_counts <- teae |>
			dplyr::summarise(
				n = dplyr::n_distinct(.data$USUBJID),
				.by = c(dplyr::all_of(trt_var), "AEDECOD")
			) |>
			dplyr::rename(term = "AEDECOD")
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

	# Calculate comparisons using internal helper
	ae_wide <- .calculate_ae_comparisons(
		ae_wide = ae_wide,
		trt_n = trt_n,
		trt_groups = trt_groups,
		ref_group = ref_group,
		trt_var = trt_var,
		conf_level = conf_level,
		include_nnh = include_nnh
	)

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

	# Calculate CI level percentage for use in formatting
	ci_level_pct <- round(conf_level * 100)

	# Create summary function for formatting output
	summary_fn <- function(ae_wide_df) {
		output_df <- data.frame(Term = ae_wide_df$term, stringsAsFactors = FALSE)

		# Validate that required columns exist
		n_ref_col <- paste0("n_", ref_group)
		pct_ref_col <- paste0("pct_", ref_group)

		# Check reference group columns exist
		missing_cols <- character()
		if (!n_ref_col %in% names(ae_wide_df)) {
			missing_cols <- c(missing_cols, n_ref_col)
		}
		if (!pct_ref_col %in% names(ae_wide_df)) {
			missing_cols <- c(missing_cols, pct_ref_col)
		}

		# Check treatment group columns exist
		for (trt in trt_groups) {
			n_trt_col <- paste0("n_", trt)
			pct_trt_col <- paste0("pct_", trt)
			rd_col <- paste0("rd_", trt)
			rd_lower_col <- paste0("rd_lower_", trt)
			rd_upper_col <- paste0("rd_upper_", trt)
			rr_col <- paste0("rr_", trt)
			rr_lower_col <- paste0("rr_lower_", trt)
			rr_upper_col <- paste0("rr_upper_", trt)
			pvalue_col <- paste0("pvalue_", trt)

			trt_cols <- c(
				n_trt_col,
				pct_trt_col,
				rd_col,
				rd_lower_col,
				rd_upper_col,
				rr_col,
				rr_lower_col,
				rr_upper_col,
				pvalue_col
			)
			if (include_nnh) {
				trt_cols <- c(
					trt_cols,
					paste0("nnh_", trt),
					paste0("nnh_lower_", trt),
					paste0("nnh_upper_", trt),
					paste0("nnh_estimable_", trt)
				)
			}

			for (col in trt_cols) {
				if (!col %in% names(ae_wide_df)) {
					missing_cols <- c(missing_cols, col)
				}
			}
		}

		if (length(missing_cols) > 0) {
			ph_abort(
				paste0(
					"Missing required columns in AE comparison data. ",
					"This may be due to a mismatch between treatment groups ",
					"in ADAE and ADSL datasets. Missing columns: ",
					paste(missing_cols, collapse = ", "),
					". ",
					"Available columns: ",
					paste(names(ae_wide_df), collapse = ", ")
				),
				call. = FALSE
			)
		}

		# Add reference group column
		N_ref <- trt_n |>
			dplyr::filter(.data[[trt_var]] == ref_group) |>
			dplyr::pull(.data$N)

		# Handle empty N_ref
		if (length(N_ref) == 0) {
			N_ref <- 0
		}

		# Format reference group column
		ref_col_name <- paste0(ref_group, "\nn/N (%)")
		output_df[[ref_col_name]] <- .format_n_over_n(
			ae_wide_df[[n_ref_col]],
			N_ref,
			ae_wide_df[[pct_ref_col]],
			digits = 1
		)

		# Add treatment group columns with comparisons
		for (trt in trt_groups) {
			n_trt_col <- paste0("n_", trt)
			pct_trt_col <- paste0("pct_", trt)
			N_trt <- trt_n |>
				dplyr::filter(.data[[trt_var]] == trt) |>
				dplyr::pull(.data$N)

			# Handle empty N_trt
			if (length(N_trt) == 0) {
				N_trt <- 0
			}

			# Format treatment group column
			trt_col_name <- paste0(trt, "\nn/N (%)")
			output_df[[trt_col_name]] <- .format_n_over_n(
				ae_wide_df[[n_trt_col]],
				N_trt,
				ae_wide_df[[pct_trt_col]],
				digits = 1
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
				ae_wide_df[[rd_col]] * 100,
				ae_wide_df[[rd_lower_col]] * 100,
				ae_wide_df[[rd_upper_col]] * 100
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
					seq_len(nrow(ae_wide_df)),
					function(i) {
						if (!isTRUE(ae_wide_df[[nnh_estimable_col]][i])) {
							return("NE")
						}
						paste0(
							format_number(ae_wide_df[[nnh_col]][i], digits = 1),
							" (",
							format_number(ae_wide_df[[nnh_lower_col]][i], digits = 1),
							", ",
							format_number(ae_wide_df[[nnh_upper_col]][i], digits = 1),
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
				ae_wide_df[[rr_col]],
				ae_wide_df[[rr_lower_col]],
				ae_wide_df[[rr_upper_col]]
			)

			# P-value
			pvalue_col <- paste0("pvalue_", trt)
			output_df[[sprintf(
				"P-value (%s vs %s)",
				trt,
				ref_group
			)]] <- format_pvalue(ae_wide_df[[pvalue_col]])
		}

		output_df
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

	# Build footnotes
	default_footnotes <- c(
		"Safety Population",
		sprintf(
			paste0(
				"RD = Risk Difference",
				if (include_nnh) ", NNH = Number Needed to Harm" else "",
				", RR = Risk Ratio, CI = %d%% Confidence Interval"
			),
			ci_level_pct
		),
		paste0("Reference group: ", ref_group),
		"P-values from Chi-square (or Fisher's exact when expected count < 5)"
	)

	if (include_nnh) {
		default_footnotes <- c(
			default_footnotes,
			"NNH = 1/|RD|; NE = not estimable when CI crosses zero"
		)
	}

	if (threshold > 0) {
		default_footnotes <- c(
			default_footnotes,
			sprintf("Events with incidence >= %.1f%% in any group", threshold)
		)
	}

	all_footnotes <- c(footnotes, default_footnotes)

	# Build custom metadata for AE comparison tables
	ae_metadata <- list(
		ref_group = ref_group,
		by = by,
		threshold = threshold,
		conf_level = conf_level,
		include_nnh = include_nnh
	)

	# Create ClinicalTable using create_clinical_table
	create_clinical_table(
		data = ae_wide,
		type = "ae_comparison",
		title = title,
		footnotes = all_footnotes,
		theme = theme,
		summary_fn = summary_fn,
		metadata = ae_metadata,
		...
	)
}
