#' @title Adverse Event Hierarchy Tables
#' @name safety_hierarchy
#' @description Functions for creating MedDRA hierarchy tables.
NULL

#' Create AE Table with Full MedDRA Hierarchy
#'
#' Creates an adverse event summary table using the full MedDRA hierarchy:
#' System Organ Class (SOC) → High Level Group Term (HLGT) →
#' High Level Term (HLT) → Preferred Term (PT).
#'
#' @param data ADAE dataset (data frame or ADaMData object)
#' @param adsl ADSL dataset for denominators (data frame or ADaMData object)
#' @param trt_var Treatment variable. Default: "TRT01P"
#' @param soc_var SOC variable. Default: "AEBODSYS"
#' @param hlgt_var HLGT variable. Default: "AEHLTGT"
#' @param hlt_var HLT variable. Default: "AEHLT"
#' @param pt_var PT variable. Default: "AEDECOD"
#' @param levels Character vector of hierarchy levels to include.
#'   Options: "soc", "hlgt", "hlt", "pt". Default: c("soc", "pt")
#' @param min_pct Numeric. Minimum percentage to display. Default: 0
#' @param sort_by Character. Sort by "alphabetical" or
#'   "frequency". Default: "frequency"
#' @param title Character. Table title. Default: NULL (auto-generated)
#' @param ... Additional arguments passed to [create_clinical_table()]
#'
#' @return ClinicalTable with hierarchical AE summary
#'
#' @examples
#' \dontrun{
#' # Create AE hierarchy table
#' table <- create_ae_hierarchy_table(
#'   data = adae,
#'   adsl = adsl,
#'   levels = c("soc", "pt"),
#'   min_pct = 5
#' )
#' table@flextable
#' }
#'
#' @export
create_ae_hierarchy_table <- function(
	data,
	adsl,
	trt_var = "TRT01P",
	soc_var = "AEBODSYS",
	hlgt_var = "AEHLTGT",
	hlt_var = "AEHLT",
	pt_var = "AEDECOD",
	levels = c("soc", "pt"),
	min_pct = 0,
	sort_by = c("frequency", "alphabetical"),
	title = NULL,
	...
) {
	sort_by <- match.arg(sort_by)

	# Ensure ADaMData objects, passing trt_var for data frames
	data <- .ensure_adam_data(data, domain = "ADAE", trt_var = trt_var)
	adsl <- .ensure_adam_data(adsl, domain = "ADSL", trt_var = trt_var)

	# Use trt_var from ADaMData object (set during coercion)
	trt_var <- data@trt_var

	# Get treatment counts from ADaMData property
	trt_n <- adsl@trt_n
	trt_levels <- adsl@trt_levels

	# Map level names to variables
	level_vars <- list(
		soc = soc_var,
		hlgt = hlgt_var,
		hlt = hlt_var,
		pt = pt_var
	)

	# Check which levels are requested and available
	df <- data@filtered_data
	available_levels <- character()
	for (lvl in levels) {
		var <- level_vars[[lvl]]
		if (!is.null(var) && var %in% names(df)) {
			available_levels <- c(available_levels, lvl)
		} else if (lvl %in% c("hlgt", "hlt")) {
			ph_inform(sprintf(
				"Variable for '%s' level (%s) not found, skipping",
				lvl,
				var
			))
		} else {
			ph_warn(sprintf(
				"Required variable for '%s' level (%s) not found in data",
				lvl,
				var
			))
		}
	}

	if (length(available_levels) == 0) {
		ph_abort("No valid MedDRA hierarchy levels found in data")
	}

	# Summarize AE hierarchy data
	summary_df <- .summarize_ae_hierarchy(
		adae = data,
		trt_n = trt_n,
		trt_var = trt_var,
		level_vars = level_vars,
		available_levels = available_levels,
		min_pct = min_pct,
		sort_by = sort_by,
		trt_levels = trt_levels
	)

	# Auto-generate title if not provided
	if (is.null(title)) {
		title <- "Adverse Events by MedDRA Hierarchy"
	}

	# Create footnotes
	footnotes <- c(
		sprintf("%s (N=%d)", adsl@population, adsl@subject_n),
		sprintf("Minimum threshold: %.1f%%", min_pct)
	)

	# Create ClinicalTable using unified engine
	create_clinical_table(
		data = summary_df,
		type = "ae_hierarchy",
		title = title,
		footnotes = footnotes,
		theme = "hta",
		...
	)
}

#' Summarize AE Hierarchy Data
#'
#' Internal function to summarize AE data by MedDRA hierarchy levels.
#'
#' @param adae ADaMData object with ADAE data
#' @param trt_n Treatment counts from ADaMData property
#' @param trt_var Treatment variable name
#' @param level_vars Named list mapping level names to variable names
#' @param available_levels Character vector of available hierarchy levels
#' @param min_pct Minimum percentage threshold
#' @param sort_by Sort method ("frequency" or "alphabetical")
#'
#' @return Data frame with summarized AE hierarchy data
#' @keywords internal
.summarize_ae_hierarchy <- function(
	adae,
	trt_n,
	trt_var,
	level_vars,
	available_levels,
	min_pct,
	sort_by,
	trt_levels
) {
	df <- adae@filtered_data
	subject_var <- adae@subject_var
	treatments <- trt_levels
	df[[trt_var]] <- factor(df[[trt_var]], levels = treatments)

	# Helper function to count subjects at each level
	count_at_level <- function(data, group_vars) {
		by_vars <- c(trt_var, group_vars)
		split_data <- split(data, interaction(data[by_vars], drop = TRUE))
		result_list <- lapply(split_data, function(x) {
			val <- unique(x[, by_vars, drop = FALSE])
			val$n <- length(unique(x[[subject_var]]))
			val
		})
		do.call(rbind, result_list)
	}

	# Build hierarchical results
	results_list <- list()

	for (lvl in available_levels) {
		var <- level_vars[[lvl]]

		# Determine grouping: for nested levels, include parent levels
		idx <- which(available_levels == lvl)
		if (idx == 1) {
			group_vars <- var
		} else {
			parent_levels <- available_levels[1:(idx - 1)]
			group_vars <- c(sapply(parent_levels, function(l) level_vars[[l]]), var)
		}

		counts <- count_at_level(df, group_vars)

		# Pivot wider for treatments
		wide_counts <- counts |>
			tidyr::pivot_wider(
				names_from = dplyr::all_of(trt_var),
				values_from = "n",
				values_fill = 0,
				names_expand = TRUE
			)

		# Add level indicator
		wide_counts$level <- lvl
		wide_counts$term_var <- var
		wide_counts$term <- wide_counts[[var]]

		results_list[[lvl]] <- wide_counts
	}

	# Combine results
	combined <- dplyr::bind_rows(results_list)

	# Format n (%) for each treatment using trt_n property
	for (trt in treatments) {
		n_col <- as.character(trt)
		N_total <- trt_n$N[trt_n[[trt_var]] == trt]

		# Guard against division by zero
		if (length(N_total) == 0 || N_total == 0) {
			ph_warn(sprintf(
				"No subjects found for treatment '%s', skipping percentage calculation",
				trt
			))
			next
		}

		if (n_col %in% names(combined)) {
			combined[[paste0(n_col, "_fmt")]] <- sprintf(
				"%d (%.1f%%)",
				combined[[n_col]],
				100 * combined[[n_col]] / N_total
			)
			combined[[paste0(n_col, "_pct")]] <- 100 * combined[[n_col]] / N_total
		}
	}

	# Filter by minimum percentage
	if (min_pct > 0) {
		pct_cols <- grep("_pct$", names(combined), value = TRUE)
		if (length(pct_cols) > 0) {
			combined$max_pct <- do.call(pmax, c(combined[pct_cols], na.rm = TRUE))
			combined <- combined[combined$max_pct >= min_pct, ]
		}
	}

	# Sort
	if (sort_by == "frequency") {
		pct_cols <- grep("_pct$", names(combined), value = TRUE)
		if (length(pct_cols) > 0) {
			combined$sort_val <- rowMeans(combined[pct_cols], na.rm = TRUE)
			combined <- combined[order(-combined$sort_val), ]
		}
	} else {
		combined <- combined[order(combined$term), ]
	}

	# Create display table
	display_cols <- c("level", "term")
	fmt_cols <- grep("_fmt$", names(combined), value = TRUE)
	display_cols <- c(display_cols, fmt_cols)

	display_df <- combined[, display_cols, drop = FALSE]

	# Rename columns
	names(display_df) <- gsub("_fmt$", "", names(display_df))
	names(display_df)[names(display_df) == "level"] <- "Level"
	names(display_df)[names(display_df) == "term"] <- "Term"

	# Add indentation based on level
	level_indent <- c(soc = "", hlgt = "  ", hlt = "    ", pt = "      ")
	display_df$Term <- paste0(
		level_indent[combined$level],
		display_df$Term
	)

	display_df
}
