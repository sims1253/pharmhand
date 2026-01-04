#' PRO Analysis Functions
#'
#' Functions for Patient-Reported Outcome analysis including
#'   MCID calculation and time-to-deterioration.
#' @name pro_analysis
#' @keywords internal
NULL

# =============================================================================
# MCID Calculation Functions
# =============================================================================

#' Calculate MCID using Anchor-Based Method
#'
#' Calculates the Minimal Clinically Important Difference using an anchor
#' variable (e.g., Patient Global Impression of Change).
#'
#' @param data Data frame with PRO scores and anchor variable
#' @param score_var Character. Name of the PRO score change variable
#' @param anchor_var Character. Name of the anchor variable
#' @param anchor_positive Character vector. Values indicating positive/improved
#'   response on anchor (default: c("Minimally Improved", "Much Improved"))
#' @param anchor_minimal Character vector. Values indicating minimal improvement
#'   used for MCID (default: "Minimally Improved")
#' @param conf_level Numeric. Confidence level for CI (default: 0.95)
#'
#' @return List with mcid estimate, ci, n, and method
#' @export
calculate_mcid_anchor <- function(
	data,
	score_var,
	anchor_var,
	anchor_positive = c("Minimally Improved", "Much Improved"),
	anchor_minimal = "Minimally Improved",
	conf_level = 0.95
) {
	admiraldev::assert_data_frame(data)
	admiraldev::assert_character_scalar(score_var)
	admiraldev::assert_character_scalar(anchor_var)

	if (!score_var %in% names(data)) {
		ph_abort(sprintf("Variable '%s' not found in data", score_var))
	}
	if (!anchor_var %in% names(data)) {
		ph_abort(sprintf("Variable '%s' not found in data", anchor_var))
	}

	# Filter to minimal improvement group
	minimal_data <- data[data[[anchor_var]] %in% anchor_minimal, , drop = FALSE]

	if (nrow(minimal_data) == 0) {
		ph_warn("No observations in minimal improvement group")
		return(list(
			mcid = NA_real_,
			ci = c(NA_real_, NA_real_),
			n = 0L,
			method = "anchor-based"
		))
	}

	scores <- minimal_data[[score_var]]
	scores <- scores[!is.na(scores)]
	n <- length(scores)

	if (n < 2) {
		return(list(
			mcid = if (n == 1) scores[1] else NA_real_,
			ci = c(NA_real_, NA_real_),
			n = n,
			method = "anchor-based"
		))
	}

	mcid <- mean(scores)
	se <- stats::sd(scores) / sqrt(n)
	alpha <- 1 - conf_level
	t_crit <- stats::qt(1 - alpha / 2, df = n - 1)
	ci <- c(mcid - t_crit * se, mcid + t_crit * se)

	list(
		mcid = mcid,
		ci = ci,
		n = n,
		se = se,
		method = "anchor-based"
	)
}

#' Calculate MCID using Distribution-Based Methods
#'
#' Calculates MCID estimates using distribution-based approaches.
#'
#' @param data Data frame with baseline PRO scores
#' @param score_var Character. Name of the baseline score variable
#' @param reliability Numeric. Test-retest reliability for SEM method
#'   (default: NULL, SEM not calculated)
#' @param methods Character vector. Methods to use: "half_sd", "one_sem",
#'   "third_sd", "fifth_sd" (default: all)
#'
#' @return A list with MCID estimates:
#' \describe{
#' \item{half_sd}{0.5 * SD of baseline scores}
#' \item{third_sd}{0.33 * SD of baseline scores}
#' \item{fifth_sd}{0.2 * SD of baseline scores}
#' \item{one_sem}{1 * SEM (requires reliability)}
#' \item{sd}{Standard deviation of baseline scores}
#' \item{n}{Number of observations}
#' \item{reliability}{Test-retest reliability if provided}
#' }
#' @examples
#' \dontrun{
#' # Distribution-based MCID for a PRO measure
#' result <- calculate_mcid_distribution(
#'   data = pro_data,
#'   score_var = "AVAL",
#'   reliability = 0.85
#' )
#'
#' # Access 0.5 SD MCID
#' result$half_sd
#' }
#' @export
calculate_mcid_distribution <- function(
	data,
	score_var,
	reliability = NULL,
	methods = c("half_sd", "one_sem", "third_sd", "fifth_sd")
) {
	admiraldev::assert_data_frame(data)
	admiraldev::assert_character_scalar(score_var)

	if (!score_var %in% names(data)) {
		ph_abort(sprintf("Variable '%s' not found in data", score_var))
	}

	scores <- data[[score_var]]
	scores <- scores[!is.na(scores)]
	n <- length(scores)

	if (n < 2) {
		ph_warn("Insufficient data for distribution-based MCID")
		return(list(
			half_sd = NA_real_,
			one_sem = NA_real_,
			third_sd = NA_real_,
			fifth_sd = NA_real_,
			sd = NA_real_,
			n = n,
			method = "distribution-based"
		))
	}

	sd_score <- stats::sd(scores)

	results <- list(
		sd = sd_score,
		n = n,
		method = "distribution-based"
	)

	if ("half_sd" %in% methods) {
		results$half_sd <- 0.5 * sd_score
	}
	if ("third_sd" %in% methods) {
		results$third_sd <- (1 / 3) * sd_score
	}
	if ("fifth_sd" %in% methods) {
		results$fifth_sd <- 0.2 * sd_score
	}
	if ("one_sem" %in% methods && !is.null(reliability)) {
		# SEM = SD * sqrt(1 - reliability)
		results$one_sem <- sd_score * sqrt(1 - reliability)
		results$reliability <- reliability
	}

	results
}

#' Calculate MCID (Combined Approach)
#'
#' Wrapper function to calculate MCID using anchor-based, distribution-based,
#' or both approaches.
#'
#' @param data Data frame with PRO data
#' @param score_var Character. Name of the score/change variable
#' @param anchor_var Character. Name of anchor variable (required
#'   for anchor method)
#' @param baseline_var Character. Name of baseline score variable
#'   (for distribution method)
#' @param method Character. "anchor", "distribution", or "both"
#'   (default: "both")
#' @param reliability Numeric. Test-retest reliability for SEM calculation
#' @param anchor_minimal Character vector. Anchor values for minimal improvement
#' @param conf_level Numeric. Confidence level (default: 0.95)
#'
#' @return List containing MCID results. Structure depends on method:
#'   \itemize{
#'     \item If method = "anchor": anchor-based results with mcid, ci, n, method
#'     \item If method = "distribution": distribution-based results
#'       (half_sd, one_sem, etc.)
#'     \item If method = "both": both anchor and distribution results
#'   }
#'
#' @examples
#' \dontrun{
#' # Anchor-based MCID
#' anchor_result <- calculate_mcid(
#'   data = pro_data,
#'   score_var = "AVAL",
#'   anchor_var = "PGIC",
#'   responder_value = "Much improved",
#'   method = "anchor"
#' )
#'
#' # Distribution-based MCID
#' dist_result <- calculate_mcid(
#'   data = pro_data,
#'   score_var = "AVAL",
#'   method = "distribution",
#'   reliability = 0.85
#' )
#' }
#' @export
calculate_mcid <- function(
	data,
	score_var,
	anchor_var = NULL,
	baseline_var = NULL,
	method = c("both", "anchor", "distribution"),
	reliability = NULL,
	anchor_minimal = "Minimally Improved",
	conf_level = 0.95
) {
	method <- match.arg(method)

	results <- list()

	if (method %in% c("anchor", "both")) {
		if (is.null(anchor_var)) {
			if (method == "anchor") {
				ph_abort("anchor_var required for anchor-based method")
			}
		} else {
			results$anchor <- calculate_mcid_anchor(
				data = data,
				score_var = score_var,
				anchor_var = anchor_var,
				anchor_minimal = anchor_minimal,
				conf_level = conf_level
			)
		}
	}

	if (method %in% c("distribution", "both")) {
		var_for_dist <- if (!is.null(baseline_var)) baseline_var else score_var
		results$distribution <- calculate_mcid_distribution(
			data = data,
			score_var = var_for_dist,
			reliability = reliability
		)
	}

	results$method <- method
	class(results) <- c("mcid_result", class(results))
	results
}

# =============================================================================
# Time-to-Deterioration Analysis Functions
# =============================================================================

#' Create Time-to-Deterioration Analysis
#'
#' Performs time-to-deterioration (TTD) analysis for PRO endpoints.
#' Deterioration is defined as a decrease from baseline exceeding the MCID.
#'
#' @param data Data frame with longitudinal PRO data
#' @param subject_var Character. Subject ID variable. Default: "USUBJID"
#' @param trt_var Character. Treatment variable. Default: "TRT01P"
#' @param param_var Character. Parameter variable. Default: "PARAMCD"
#' @param paramcd Character. Parameter code to analyze.
#'   Default: NULL (uses first)
#' @param value_var Character. Value variable. Default: "AVAL"
#' @param base_var Character. Baseline value variable. Default: "BASE"
#' @param chg_var Character. Change from baseline variable.
#'   Default: "CHG"
#' @param time_var Character. Time/day variable. Default: "ADY"
#' @param visit_var Character. Visit number variable.
#'   Default: "AVISITN"
#' @param threshold Numeric. MCID threshold for deterioration
#'   (positive = improvement)
#' @param direction Character. "decrease" if lower is worse,
#'   "increase" if higher is worse
#' @param definition Character. "first" or "confirmed"
#'   deterioration. Default: "first"
#' @param confirmation_visits Integer. Visits to confirm
#'   (for definition="confirmed")
#' @param censor_at Numeric. Time to censor if no event.
#'   Default: max time in data
#'
#' @return A list with components:
#' \describe{
#' \item{ttd_data}{Prepared time-to-deterioration data frame}
#' \item{km_fit}{Kaplan-Meier fit object from survfit}
#' \item{summary_table}{Summary statistics by treatment group}
#' \item{threshold}{MCID threshold used}
#' \item{direction}{Direction of deterioration}
#' \item{definition}{Deterioration definition used}
#' \item{n_subjects}{Total number of subjects}
#' \item{n_events}{Total number of deterioration events}
#' }
#' @export
create_ttd_analysis <- function(
	data,
	subject_var = "USUBJID",
	trt_var = "TRT01P",
	param_var = "PARAMCD",
	paramcd = NULL,
	value_var = "AVAL",
	base_var = "BASE",
	chg_var = "CHG",
	time_var = "ADY",
	visit_var = "AVISITN",
	threshold,
	direction = c("decrease", "increase"),
	definition = c("first", "confirmed"),
	confirmation_visits = 1,
	censor_at = NULL
) {
	direction <- match.arg(direction)
	definition <- match.arg(definition)

	# Validate inputs
	admiraldev::assert_data_frame(data)
	admiraldev::assert_character_scalar(subject_var)
	admiraldev::assert_character_scalar(trt_var)
	admiraldev::assert_numeric_vector(threshold, len = 1)

	# Filter to parameter if specified
	if (!is.null(paramcd) && param_var %in% names(data)) {
		data <- data[data[[param_var]] == paramcd, , drop = FALSE]
	}

	# Check required columns
	required_vars <- c(subject_var, trt_var, time_var)
	if (chg_var %in% names(data)) {
		change_col <- chg_var
	} else if (base_var %in% names(data) && value_var %in% names(data)) {
		data$CHG_CALC <- data[[value_var]] - data[[base_var]]
		change_col <- "CHG_CALC"
	} else {
		ph_abort("Either CHG variable or both AVAL and BASE required")
	}

	# Define deterioration based on direction
	if (direction == "decrease") {
		# Lower is worse, so deterioration is negative change exceeding threshold
		data$deteriorated <- data[[change_col]] <= -abs(threshold)
	} else {
		# Higher is worse, so deterioration is positive change exceeding threshold
		data$deteriorated <- data[[change_col]] >= abs(threshold)
	}

	# Sort by subject and time
	data <- data[order(data[[subject_var]], data[[time_var]]), ]

	# Find first deterioration for each subject
	if (definition == "first") {
		ttd_events <- data |>
			dplyr::filter(.data$deteriorated) |>
			dplyr::group_by(dplyr::across(dplyr::all_of(c(subject_var, trt_var)))) |>
			dplyr::summarise(
				event_time = min(.data[[time_var]], na.rm = TRUE),
				event = 1L,
				.groups = "drop"
			)
	} else {
		# Confirmed deterioration: need consecutive visits
		ttd_events <- data |>
			dplyr::group_by(dplyr::across(dplyr::all_of(subject_var))) |>
			dplyr::mutate(
				run_length = sequence(rle(.data$deteriorated)$lengths)
			) |>
			dplyr::filter(
				.data$deteriorated & .data$run_length >= confirmation_visits
			) |>
			dplyr::group_by(dplyr::across(dplyr::all_of(c(subject_var, trt_var)))) |>
			dplyr::summarise(
				event_time = min(.data[[time_var]], na.rm = TRUE),
				event = 1L,
				.groups = "drop"
			)
	}

	# Get all subjects with their treatment
	all_subjects <- data |>
		dplyr::group_by(dplyr::across(dplyr::all_of(c(subject_var, trt_var)))) |>
		dplyr::summarise(
			max_time = max(.data[[time_var]], na.rm = TRUE),
			.groups = "drop"
		)

	# Merge events with all subjects
	ttd_data <- dplyr::left_join(
		all_subjects,
		ttd_events,
		by = c(subject_var, trt_var)
	)

	# Censored subjects (no event)
	ttd_data$event <- ifelse(is.na(ttd_data$event), 0L, ttd_data$event)

	# Set time: event time if event, otherwise max follow-up or censor_at
	if (is.null(censor_at)) {
		censor_at <- max(data[[time_var]], na.rm = TRUE)
	}
	ttd_data$time <- ifelse(
		ttd_data$event == 1,
		ttd_data$event_time,
		pmin(ttd_data$max_time, censor_at)
	)

	# Ensure positive times
	ttd_data$time <- pmax(ttd_data$time, 1)

	# Create survival object and fit KM
	surv_formula <- stats::as.formula(
		sprintf("survival::Surv(time, event) ~ %s", trt_var)
	)

	km_fit <- survival::survfit(surv_formula, data = ttd_data)

	# Create summary table
	summary_table <- ttd_data |>
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
		dplyr::summarise(
			N = dplyr::n(),
			Events = sum(.data$event),
			`Event Rate (%)` = sprintf("%.1f", 100 * mean(.data$event)),
			`Median TTD` = {
				km_sub <- survival::survfit(
					survival::Surv(time, event) ~ 1,
					data = dplyr::pick(dplyr::everything())
				)
				med <- summary(km_sub)$table["median"]
				if (is.na(med)) "NR" else sprintf("%.0f", med)
			},
			.groups = "drop"
		)

	list(
		ttd_data = ttd_data,
		km_fit = km_fit,
		summary_table = summary_table,
		threshold = threshold,
		direction = direction,
		definition = definition,
		n_subjects = nrow(ttd_data),
		n_events = sum(ttd_data$event)
	)
}
