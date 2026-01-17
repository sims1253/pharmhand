#' @title Adverse Event Time-to-Event Analysis
#' @name safety_tte
#' @description Functions for time-to-event analysis of adverse events.
NULL


#' Calculate AE TTE Data for a specific SOC
#'
#' Prepares time-to-event data for Kaplan-Meier analysis of adverse events.
#'
#' @param adsl ADSL data frame
#' @param adae ADAE data frame
#' @param soc SOC value to filter by
#' @param trt_var Treatment variable name
#' @return Data frame formatted for KM plotting
#' @examples
#' \dontrun{
#' # Calculate TTE data for a specific SOC
#' tte_data <- calculate_ae_tte_data(
#'   adsl = adsl,
#'   adae = adae,
#'   soc = "Gastrointestinal disorders"
#' )
#' head(tte_data)
#' }
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
		dplyr::ungroup() |>
		dplyr::select("USUBJID", "ASTDY") |>
		dplyr::mutate(event = 1)

	# Merge with ADSL
	tte_data <- adsl |>
		dplyr::filter(.data$SAFFL == "Y") |>
		dplyr::left_join(first_event, by = dplyr::join_by(USUBJID))

	# Derive time and event
	# If TRTDURD is not available, derive it from TRTEDT and TRTSDT
	if (!"TRTDURD" %in% names(tte_data)) {
		if ("TRTEDT" %in% names(tte_data) && "TRTSDT" %in% names(tte_data)) {
			tte_data <- tte_data |>
				dplyr::mutate(
					TRTDURD = as.numeric(.data$TRTEDT - .data$TRTSDT) + 1
				)
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
#' @return ClinicalPlot object, or NULL if no data available for the
#'   specified SOC
#' @examples
#' \dontrun{
#' # Create KM plot for AE in a specific SOC
#' plot <- create_ae_km_plot_for_soc(
#'   adsl = adsl,
#'   adae = adae,
#'   soc = "Gastrointestinal disorders"
#' )
#' plot@plot
#' }
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

	# Validate ref_group if provided
	if (!is.null(ref_group)) {
		if (!is.character(ref_group) || length(ref_group) != 1) {
			ph_abort("'ref_group' must be a single character string")
		}
		trt_levels <- unique(c(adsl[[trt_var]], adae[[trt_var]]))
		if (!(ref_group %in% trt_levels)) {
			ph_abort(sprintf(
				"'ref_group' value '%s' not found in treatment levels. Valid values: %s",
				ref_group,
				paste(trt_levels, collapse = ", ")
			))
		}
	}

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
		dplyr::ungroup() |>
		dplyr::transmute(
			USUBJID = .data$USUBJID,
			event_time = .data[[time_var]],
			event = 1
		)

	tte_data <- adsl |>
		dplyr::filter(.data$SAFFL == "Y") |>
		dplyr::left_join(first_event, by = dplyr::join_by(USUBJID))

	if (!censor_var %in% names(tte_data)) {
		if (
			censor_var == "TRTDURD" &&
				"TRTEDT" %in% names(tte_data) &&
				"TRTSDT" %in% names(tte_data)
		) {
			tte_data <- tte_data |>
				dplyr::mutate(
					!!rlang::sym(censor_var) := as.numeric(.data$TRTEDT - .data$TRTSDT) +
						1
				)
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

	n_before <- nrow(tte_data)
	tte_data <- tte_data |>
		dplyr::mutate(
			event = dplyr::if_else(!is.na(.data$event), 1L, 0L),
			time = dplyr::case_when(
				.data$event == 1 & !is.na(.data$event_time) ~ .data$event_time,
				TRUE ~ .data[[censor_var]]
			)
		) |>
		dplyr::filter(
			!is.na(.data$time),
			.data$time > 0,
			!is.na(.data[[trt_var]])
		)

	n_dropped <- n_before - nrow(tte_data)
	if (n_dropped > 0) {
		ph_warn(
			sprintf(
				"%d subjects excluded: missing or non-positive time, or no treatment",
				n_dropped
			),
			call. = FALSE
		)
	}

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
