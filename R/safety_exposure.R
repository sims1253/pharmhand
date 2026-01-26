#' @title Exposure-Adjusted AE Analysis
#' @name safety_exposure
#' @description Functions for exposure-adjusted adverse event analysis.
NULL

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

	if (patient_years < 0.01) {
		ph_warn(
			sprintf(
				"Very small patient_years value (%g) may produce unstable rate estimates",
				patient_years
			),
			call. = FALSE
		)
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
#' @param data ADAE data frame or ADaMData object
#' @param adsl ADSL data frame or ADaMData object (must contain exposure
#'   duration)
#' @param exposure_var Character. Exposure duration variable
#'   (default: "TRTDURD")
#' @param trt_var Character. Treatment variable (default: "TRT01P")
#' @param by Character. "soc", "pt", or "overall" (default: "pt")
#' @param time_unit Character. Unit of the exposure_var values:
#'   "days" (default for TRTDURD), "weeks", or "months".
#'   Used to convert exposure to patient-years.
#' @param per Numeric. Rate per X patient-years (default: 100)
#' @param conf_level Numeric. Confidence level (default: 0.95)
#' @param threshold Numeric. Minimum incidence to include (default: 0)
#' @param title Character. Table title
#' @param ... Additional arguments passed to [create_clinical_table()]
#'
#' @return ClinicalTable with IDR columns
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- create_ae_exposure_table(data, adsl, by = "pt")
#' }
create_ae_exposure_table <- function(
	data,
	adsl,
	exposure_var = "TRTDURD",
	trt_var = "TRT01P",
	by = c("pt", "soc", "overall"),
	time_unit = c("days", "weeks", "months"),
	per = 100,
	conf_level = 0.95,
	threshold = 0,
	title = NULL,
	...
) {
	by <- match.arg(by)
	time_unit <- match.arg(time_unit)

	# Ensure ADaMData objects, passing trt_var for data frames
	data <- .ensure_adam_data(data, domain = "ADAE", trt_var = trt_var)
	adsl <- .ensure_adam_data(adsl, domain = "ADSL", trt_var = trt_var)

	# Use trt_var from ADaMData object (set during coercion)
	trt_var <- data@trt_var

	assert_character_scalar(exposure_var, "exposure_var")
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

	# Use ADaMData properties
	na_string <- get_na_string()

	# Summarize exposure data
	summary_df <- .summarize_ae_exposure(
		adae = data,
		adsl = adsl,
		exposure_var = exposure_var,
		trt_var = trt_var,
		by = by,
		time_unit = time_unit,
		per = per,
		conf_level = conf_level,
		threshold = threshold,
		na_string = na_string
	)

	# Generate title if not provided
	if (is.null(title)) {
		title <- switch(
			by,
			overall = "Exposure-Adjusted Adverse Event Rates",
			soc = "Exposure-Adjusted Adverse Events by System Organ Class",
			pt = "Exposure-Adjusted Adverse Events by Preferred Term"
		)
	}

	# Create footnotes
	ci_level_pct <- round(conf_level * 100)
	population_label <- if (adsl@population != "ALL") {
		sprintf("%s (%s = 'Y')", adsl@population, paste0(adsl@population, "FL"))
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

	# Create ClinicalTable using unified engine
	create_clinical_table(
		data = summary_df,
		type = "ae_exposure",
		title = title,
		footnotes = footnotes,
		theme = "hta",
		...
	)
}

#' Summarize AE Exposure Data
#'
#' Internal function to summarize AE data for exposure-adjusted analysis.
#'
#' @param adae ADaMData object with ADAE data
#' @param adsl ADaMData object with ADSL data
#' @param exposure_var Exposure duration variable name
#' @param trt_var Treatment variable name
#' @param by Grouping level ("pt", "soc", or "overall")
#' @param time_unit Unit of exposure time
#' @param per Rate per X patient-years
#' @param conf_level Confidence level
#' @param threshold Minimum incidence threshold
#' @param na_string NA string for formatting
#'
#' @return Data frame with summarized AE exposure data
#' @keywords internal
.summarize_ae_exposure <- function(
	adae,
	adsl,
	exposure_var,
	trt_var,
	by,
	time_unit,
	per,
	conf_level,
	threshold,
	na_string
) {
	df <- adae@filtered_data
	adsl_df <- adsl@filtered_data
	subject_var <- adae@subject_var

	# Check for empty data
	if (nrow(adsl_df) == 0) {
		ph_abort("No subjects available after population filter", call. = FALSE)
	}

	# Check required columns
	required_cols <- c("TRTEMFL", subject_var, trt_var)
	if (by == "soc") {
		required_cols <- c(required_cols, "AEBODSYS")
	} else if (by == "pt") {
		required_cols <- c(required_cols, "AEDECOD")
	}
	missing_cols <- setdiff(required_cols, names(df))
	if (length(missing_cols) > 0) {
		ph_abort(
			paste0(
				"'data' is missing required column(s): ",
				paste(missing_cols, collapse = ", ")
			),
			call. = FALSE
		)
	}

	required_adsl_cols <- c(subject_var, trt_var, exposure_var)
	missing_adsl <- setdiff(required_adsl_cols, names(adsl_df))
	if (length(missing_adsl) > 0) {
		ph_abort(
			paste0(
				"'adsl' is missing required column(s): ",
				paste(missing_adsl, collapse = ", ")
			),
			call. = FALSE
		)
	}

	# Calculate patient-years from adsl
	exposure_values <- as.numeric(adsl_df[[exposure_var]])
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
		adsl_df <- adsl_df[!missing_exposure, , drop = FALSE]
		exposure_values <- exposure_values[!missing_exposure]
	}

	if (nrow(adsl_df) == 0) {
		ph_abort("No non-missing exposure values available", call. = FALSE)
	}

	if (any(exposure_values < 0)) {
		ph_abort(
			sprintf("'%s' must be non-negative", exposure_var),
			call. = FALSE
		)
	}

	# Convert exposure from time_unit to patient-years
	year_divisor <- switch(
		time_unit,
		days = 365.25,
		weeks = 365.25 / 7,
		months = 12
	)

	adsl_df$exposure_years <- exposure_values / year_divisor

	# Use ADaMData trt_levels property
	trt_levels <- adsl@trt_levels

	# Set factors
	adsl_df[[trt_var]] <- factor(
		adsl_df[[trt_var]],
		levels = trt_levels
	)

	# Calculate patient-years by treatment
	patient_years_by_trt <- adsl_df |>
		dplyr::summarise(
			patient_years = sum(.data$exposure_years, na.rm = TRUE),
			.by = dplyr::all_of(trt_var)
		)

	patient_years_by_trt[[trt_var]] <- factor(
		patient_years_by_trt[[trt_var]],
		levels = trt_levels
	)

	# Validate patient-years
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

	# Filter for treatment-emergent AEs
	teae <- df |>
		dplyr::filter(.data$TRTEMFL == "Y")

	# Return empty data frame if no TEAEs
	if (nrow(teae) == 0) {
		ph_warn("No treatment-emergent adverse events found", call. = FALSE)
		return(.empty_ae_exposure_df(trt_levels, by, conf_level, per, na_string))
	}

	# Count events by term and treatment
	if (by == "overall") {
		event_counts <- teae |>
			dplyr::summarise(n_events = dplyr::n(), .by = dplyr::all_of(trt_var)) |>
			dplyr::mutate(term = "Any TEAE")
	} else if (by == "soc") {
		event_counts <- teae |>
			dplyr::summarise(
				n_events = dplyr::n(),
				.by = c(dplyr::all_of(trt_var), "AEBODSYS")
			) |>
			dplyr::rename(term = "AEBODSYS")
	} else {
		event_counts <- teae |>
			dplyr::summarise(
				n_events = dplyr::n(),
				.by = c(dplyr::all_of(trt_var), "AEDECOD")
			) |>
			dplyr::rename(term = "AEDECOD")
	}

	event_counts[[trt_var]] <- factor(
		event_counts[[trt_var]],
		levels = trt_levels
	)
	event_counts$term <- as.character(event_counts$term)

	terms <- unique(event_counts$term)

	# Create all combinations of terms and treatments
	all_combos <- tidyr::expand_grid(
		term = terms,
		trt_group = trt_levels
	)
	names(all_combos)[names(all_combos) == "trt_group"] <- trt_var
	all_combos[[trt_var]] <- factor(all_combos[[trt_var]], levels = trt_levels)

	event_counts <- all_combos |>
		dplyr::left_join(event_counts, by = c("term", trt_var)) |>
		dplyr::mutate(n_events = tidyr::replace_na(.data$n_events, 0))

	# Join with patient-years
	event_counts <- event_counts |>
		dplyr::left_join(patient_years_by_trt, by = trt_var)

	# Calculate incidence rates
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

	# Apply threshold filter
	if (threshold > 0) {
		terms_above_threshold <- event_counts |>
			dplyr::summarise(
				max_rate = max(.data$rate, na.rm = TRUE),
				.by = "term"
			) |>
			dplyr::filter(.data$max_rate >= threshold) |>
			dplyr::pull(.data$term)

		event_counts <- event_counts |>
			dplyr::filter(.data$term %in% terms_above_threshold)
	}

	# Return empty if no terms meet threshold
	if (nrow(event_counts) == 0) {
		ph_warn(
			"No adverse events meet the specified threshold criteria",
			call. = FALSE
		)
		return(.empty_ae_exposure_df(trt_levels, by, conf_level, per, na_string))
	}

	# Sort and format
	if (by != "overall") {
		term_levels <- sort(unique(event_counts$term))
	} else {
		term_levels <- terms
	}

	event_counts <- event_counts |>
		dplyr::mutate(term = factor(.data$term, levels = term_levels)) |>
		dplyr::arrange(.data$term, .data[[trt_var]])

	# Format output columns
	ci_level_pct <- round(conf_level * 100)
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

	output_df
}

#' Create Empty AE Exposure Data Frame
#'
#' Helper function to construct an empty data frame for AE exposure analysis.
#'
#' @param trt_levels Character vector of treatment levels
#' @param by Grouping level
#' @param conf_level Confidence level
#' @param per Rate per X patient-years
#' @param na_string NA string
#'
#' @return Empty data frame with correct column structure
#' @keywords internal
.empty_ae_exposure_df <- function(trt_levels, by, conf_level, per, na_string) {
	term_label <- switch(
		by,
		overall = "Term",
		soc = "System Organ Class",
		pt = "Preferred Term"
	)

	ci_level_pct <- round(conf_level * 100)
	idr_label <- sprintf("IDR per %s PY (%d%% CI)", per, ci_level_pct)
	value_cols <- c("Events (n)", "Patient-Years", idr_label)

	col_names <- c(term_label)
	for (trt in trt_levels) {
		for (vc in value_cols) {
			col_names <- c(col_names, paste0(trt, "\n", vc))
		}
	}

	output_df <- as.data.frame(
		lapply(col_names, function(x) character(0)),
		stringsAsFactors = FALSE
	)
	names(output_df) <- col_names

	output_df
}
