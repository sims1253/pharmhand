#' @title Competing Risk Analysis
#' @name competing_risks
#' @description Functions for competing risk analysis using the Fine-Gray model
#'   and cumulative incidence function estimation.
NULL

# =============================================================================
# CompetingRiskResult S7 Class
# =============================================================================

#' CompetingRiskResult Class
#'
#' An S7 class for storing competing risk analysis results.
#'
#' @export
#'
#' @param model The cmprsk model object
#' @param cif_main Data frame with cumulative incidence function for main event
#' @param cif_competing Data frame with cumulative incidence function
#'   for competing events
#' @param cif_by_treatment List of CIFs by treatment group
#' @param treatment_comparison Data frame comparing treatments
#' @param subhazard_ratio Data frame with subhazard ratios
#' @param main_event Integer code for main event of interest
#' @param competing_events Integer vector of competing event codes
#' @param time_points Numeric vector of time points used
#' @param n_obs Number of observations
#' @param n_events Vector of event counts by type
#' @param metadata List of additional metadata
#'
#' @return A CompetingRiskResult object
CompetingRiskResult <- S7::new_class(
	"CompetingRiskResult",
	package = "pharmhand",
	properties = list(
		model = S7::new_property(S7::class_any),
		cif_main = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("cif_main must be a data frame")
				}
				NULL
			}
		),
		cif_competing = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("cif_competing must be a data frame")
				}
				NULL
			}
		),
		cif_by_treatment = S7::new_property(S7::class_list, default = list()),
		treatment_comparison = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("treatment_comparison must be a data frame")
				}
				NULL
			}
		),
		subhazard_ratio = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("subhazard_ratio must be a data frame")
				}
				NULL
			}
		),
		main_event = S7::new_property(S7::class_integer),
		competing_events = S7::new_property(S7::class_any),
		time_points = S7::new_property(S7::class_numeric),
		n_obs = S7::new_property(S7::class_integer),
		n_events = S7::new_property(S7::class_numeric),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

# =============================================================================
# Competing Risk Analysis Functions
# =============================================================================

#' Competing Risk Analysis
#'
#' Performs competing risk analysis using the Fine-Gray model to estimate
#' cumulative incidence functions and subhazard ratios.
#'
#' @param data A data frame containing the survival data
#' @param time_var Character. Name of the time variable
#' @param event_var Character. Name of the event indicator variable
#' @param trt_var Character. Name of the treatment variable
#' @param main_event Integer. Code for the main event of interest
#' @param competing_events Integer vector. Codes for competing events
#' @param covariates Character vector of covariate names
#' @param conf_level Numeric. Confidence level for intervals (default: 0.95)
#' @param time_points Numeric vector of specific time points to estimate CIF.
#'   If NULL, uses automatic grid based on data.
#' @param reference_group Character. Reference group for treatment comparison.
#'   If NULL, uses first level of treatment variable.
#'
#' @return A CompetingRiskResult object
#'
#' @details
#' The Fine-Gray model estimates the cumulative incidence function (CIF):
#' F_k(t) = P(T <= t, epsilon = k)
#'
#' where T is the event time and epsilon is the event type.
#'
#' The model uses a proportional subhazards approach:
#' lambda_k(t|X) = lambda_\{0k\}(t) * exp(beta_k'X)
#'
#' where lambda_k is the subhazard for event type k.
#'
#' @references
#' Fine, J.P. and Gray, R.J. (1999). A proportional hazards model for the
#' subdistribution of a competing risk. Journal of the American Statistical
#' Association, 94(446), 496-509.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic competing risk analysis
#' result <- competing_risk_analysis(
#'   data = survival_data,
#'   time_var = "time",
#'   event_var = "event",
#'   trt_var = "TRT01P",
#'   main_event = 1,
#'   competing_events = c(2, 3)
#' )
#'
#' # With covariates and custom time points
#' result <- competing_risk_analysis(
#'   data = survival_data,
#'   time_var = "time",
#'   event_var = "event",
#'   trt_var = "TRT01P",
#'   main_event = 1,
#'   competing_events = c(2),
#'   covariates = c("age", "sex"),
#'   time_points = c(6, 12, 24)
#' )
#' }
competing_risk_analysis <- function(
	data,
	time_var,
	event_var,
	trt_var,
	main_event,
	competing_events,
	covariates = NULL,
	conf_level = 0.95,
	time_points = NULL,
	reference_group = NULL
) {
	# Validate inputs
	if (!is.data.frame(data)) {
		ph_abort("'data' must be a data frame")
	}

	# Check cmprsk package availability
	if (!requireNamespace("cmprsk", quietly = TRUE)) {
		ph_abort(
			"Package 'cmprsk' is required for competing risk analysis. ",
			"Install with: install.packages('cmprsk')"
		)
	}

	# Validate confidence level
	if (
		!is.numeric(conf_level) ||
			length(conf_level) != 1 ||
			conf_level <= 0 ||
			conf_level >= 1
	) {
		ph_abort("'conf_level' must be a single value between 0 and 1")
	}

	# Compute z critical value for confidence intervals
	z <- qnorm(1 - (1 - conf_level) / 2)

	# Check required variables exist
	required_vars <- c(time_var, event_var, trt_var)
	if (!all(required_vars %in% names(data))) {
		missing <- setdiff(required_vars, names(data))
		ph_abort(sprintf(
			"Required variables not found: %s",
			paste(missing, collapse = ", ")
		))
	}

	# Check covariates exist
	if (!is.null(covariates)) {
		if (!all(covariates %in% names(data))) {
			missing <- setdiff(covariates, names(data))
			ph_abort(sprintf(
				"Covariate variables not found: %s",
				paste(missing, collapse = ", ")
			))
		}
	}

	# Prepare data
	analysis_data <- data[, c(time_var, event_var, trt_var, covariates)]
	colnames(analysis_data) <- c("time", "event", "treatment", covariates)

	# Remove rows with NA values
	analysis_data <- na.omit(analysis_data)

	# Check data quality
	if (any(analysis_data$time <= 0, na.rm = TRUE)) {
		ph_warn(
			"Some time values are <= 0. These will be handled appropriately by cmprsk."
		)
	}

	# Set reference group
	trt_levels <- unique(analysis_data$treatment)
	trt_levels <- trt_levels[!is.na(trt_levels)]
	if (length(trt_levels) < 2) {
		ph_warn(
			"Only one treatment level found. ",
			"Treatment comparisons may not be meaningful."
		)
	}

	if (is.null(reference_group)) {
		reference_group <- trt_levels[1]
	}

	# Validate reference_group is in treatment levels
	if (!reference_group %in% trt_levels) {
		ph_abort(sprintf(
			"reference_group '%s' not found in treatment levels: %s",
			reference_group,
			paste(trt_levels, collapse = ", ")
		))
	}

	# Relevel treatment factor with reference_group first
	analysis_data$treatment <- factor(
		analysis_data$treatment,
		levels = c(reference_group, setdiff(trt_levels, reference_group))
	)

	# Build model formula for model.matrix (RHS only - LHS not needed by crr)
	formula_rhs <- c("treatment")
	if (!is.null(covariates)) {
		formula_rhs <- c(formula_rhs, covariates)
	}

	formula_str <- sprintf(
		"~ %s",
		paste(formula_rhs, collapse = " + ")
	)
	formula <- stats::as.formula(formula_str)

	# Validate main_event is not in competing_events
	if (main_event %in% competing_events) {
		ph_abort(sprintf(
			"main_event (%s) cannot also be in competing_events (%s)",
			main_event,
			paste(competing_events, collapse = ", ")
		))
	}

	# Validate all event values are in allowed set
	allowed_events <- c(0, main_event, competing_events)
	actual_events <- unique(na.omit(analysis_data$event))
	unexpected <- setdiff(actual_events, allowed_events)
	if (length(unexpected) > 0) {
		ph_abort(sprintf(
			"Unexpected event values found: %s. Allowed values are: %s",
			paste(unexpected, collapse = ", "),
			paste(allowed_events, collapse = ", ")
		))
	}

	# Create event indicator matrix for main event
	status_main <- as.numeric(ifelse(analysis_data$event == main_event, 1, 0))
	status_main[analysis_data$event %in% competing_events] <- 2 # Competing

	# Fit Fine-Gray model for main event
	tryCatch(
		{
			fit_main <- cmprsk::crr(
				ftime = analysis_data$time,
				fstatus = status_main,
				cov1 = model.matrix(formula, analysis_data)[, -1], # Remove intercept
				failcode = 1,
				cencode = 0
			)
		},
		error = function(e) {
			ph_abort(sprintf(
				"Fine-Gray model fitting failed: %s",
				conditionMessage(e)
			))
		}
	)

	# Calculate CIF for main event
	if (is.null(time_points)) {
		# Use automatic time grid
		time_grid <- seq(
			min(analysis_data$time, na.rm = TRUE),
			max(analysis_data$time, na.rm = TRUE),
			length.out = 50
		)
	} else {
		time_grid <- time_points
	}

	# Estimate CIF for main event by treatment
	cif_by_treatment <- list()
	for (trt in trt_levels) {
		trt_data <- analysis_data[analysis_data$treatment == trt, ]

		# Create covariate matrix for predictions
		pred_data <- analysis_data[1, ] # Template row
		pred_data$treatment <- factor(trt, levels = levels(analysis_data$treatment))
		pred_matrix <- model.matrix(formula, pred_data)[, -1]

		# Calculate CIF
		# Note: predict.crr returns a matrix with times in col 1 and CIF in col 2+
		cif_result <- cmprsk::predict.crr(
			fit_main,
			cov1 = pred_matrix
		)

		# Extract times and CIF values from matrix output
		pred_times <- cif_result[, 1]
		cif_values <- cif_result[, 2]

		cif_by_treatment[[trt]] <- data.frame(
			time = pred_times,
			cif = cif_values,
			treatment = trt,
			stringsAsFactors = FALSE
		)
	}

	# Overall CIF
	cif_main <- do.call(rbind, cif_by_treatment)

	# Calculate treatment comparison
	if (length(trt_levels) >= 2) {
		# Extract coefficient for treatment
		coef_name <- grep(
			"treatment",
			names(fit_main$coef),
			value = TRUE,
			fixed = TRUE
		)
		if (length(coef_name) > 0) {
			# Extract SE from variance diagonal to handle multiple coefficients
			se_vec <- sqrt(diag(fit_main$var))[coef_name]
			coef_vec <- fit_main$coef[coef_name]

			subhazard_ratio <- data.frame(
				effect_measure = "Subhazard Ratio",
				estimate = exp(coef_vec),
				se = se_vec,
				ci_lower = exp(coef_vec - z * se_vec),
				ci_upper = exp(coef_vec + z * se_vec),
				p_value = 2 * (1 - pnorm(abs(coef_vec / se_vec))),
				stringsAsFactors = FALSE
			)
		} else {
			subhazard_ratio <- data.frame(
				effect_measure = "Subhazard Ratio",
				estimate = NA_real_,
				se = NA_real_,
				ci_lower = NA_real_,
				ci_upper = NA_real_,
				p_value = NA_real_,
				stringsAsFactors = FALSE
			)
		}

		# Treatment comparison
		treatment_comparison <- data.frame(
			comparison = sprintf("%s vs %s", trt_levels[-1], reference_group),
			effect_measure = "Subhazard Ratio",
			stringsAsFactors = FALSE
		)
	} else {
		subhazard_ratio <- data.frame()
		treatment_comparison <- data.frame()
	}

	# Count events
	event_counts <- table(analysis_data$event, useNA = "no")
	n_events <- c(
		main = ifelse(
			main_event %in% names(event_counts),
			event_counts[as.character(main_event)],
			0
		),
		competing = sum(event_counts[as.character(competing_events)], na.rm = TRUE),
		censored = ifelse("0" %in% names(event_counts), event_counts["0"], 0)
	)

	CompetingRiskResult(
		model = fit_main,
		cif_main = cif_main,
		cif_competing = data.frame(), # Placeholder - could be expanded
		cif_by_treatment = cif_by_treatment,
		treatment_comparison = treatment_comparison,
		subhazard_ratio = subhazard_ratio,
		main_event = as.integer(main_event),
		competing_events = as.integer(competing_events),
		time_points = time_grid,
		n_obs = nrow(analysis_data),
		n_events = n_events,
		metadata = list(
			time_var = time_var,
			event_var = event_var,
			trt_var = trt_var,
			covariates = covariates,
			conf_level = conf_level,
			reference_group = reference_group
		)
	)
}

#' Create Competing Risk Table
#'
#' Creates a clinical table from competing risk analysis results.
#'
#' @param result A CompetingRiskResult object
#' @param title Table title
#' @param subtitle Optional subtitle
#' @param footnotes Optional footnotes
#' @param autofit Logical. Whether to autofit column widths
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' table <- create_competing_risk_table(
#'   result, title = "Competing Risk Analysis"
#' )
#' }
create_competing_risk_table <- function(
	result,
	title = "Competing Risk Analysis",
	subtitle = NULL,
	footnotes = NULL,
	autofit = TRUE
) {
	if (!S7::S7_inherits(result, CompetingRiskResult)) {
		ph_abort("'result' must be a CompetingRiskResult object")
	}

	# Get subhazard ratio results
	subhazard_df <- result@subhazard_ratio

	if (nrow(subhazard_df) == 0) {
		ph_warn("No treatment comparison available for table creation")
		subhazard_df <- data.frame(
			effect_measure = "Subhazard Ratio",
			estimate = NA_real_,
			se = NA_real_,
			ci_lower = NA_real_,
			ci_upper = NA_real_,
			p_value = NA_real_,
			stringsAsFactors = FALSE
		)
	}

	# Format p-values and CI with dynamic label based on conf_level
	conf_level <- result@metadata$conf_level
	ci_label <- paste0(round(conf_level * 100, 0), "% CI")

	subhazard_df$`P-value` <- format_pvalue(subhazard_df$p_value)
	subhazard_df[[ci_label]] <- sprintf(
		"%.3f (%.3f, %.3f)",
		subhazard_df$estimate,
		subhazard_df$ci_lower,
		subhazard_df$ci_upper
	)

	# Create summary
	summary_df <- data.frame(
		`Effect Measure` = subhazard_df$effect_measure,
		`Subhazard Ratio` = subhazard_df$estimate,
		CI = subhazard_df[[ci_label]],
		`P-value` = subhazard_df$`P-value`,
		stringsAsFactors = FALSE,
		check.names = FALSE
	)
	names(summary_df)[3] <- ci_label

	# Add metadata
	meta_footnotes <- c(
		paste("Main event:", result@main_event),
		paste("Competing events:", paste(result@competing_events, collapse = ", ")),
		paste("Observations:", result@n_obs),
		paste(
			"Events:",
			paste(names(result@n_events), result@n_events, collapse = ", ")
		),
		if (!is.null(footnotes)) footnotes
	)

	create_clinical_table(
		data = summary_df,
		title = title,
		footnotes = meta_footnotes,
		autofit = autofit
	)
}

#' Plot Cumulative Incidence Function
#'
#' Creates a plot of the cumulative incidence function from competing
#' risk analysis.
#'
#' @param result A CompetingRiskResult object
#' @param title Plot title
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_cif(result, title = "Cumulative Incidence Function")
#' }
plot_cif <- function(
	result,
	title = "Cumulative Incidence Function"
) {
	if (!S7::S7_inherits(result, CompetingRiskResult)) {
		ph_abort("'result' must be a CompetingRiskResult object")
	}

	cif_data <- result@cif_main
	if (nrow(cif_data) == 0) {
		ph_warn("No CIF data available for plotting")
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No data available")

		return(ClinicalPlot(
			plot = p,
			type = "cif_plot",
			title = title
		))
	}

	p <- ggplot2::ggplot(
		cif_data,
		ggplot2::aes(x = .data$time, y = .data$cif, color = .data$treatment)
	) +
		ggplot2::geom_line() +
		ggplot2::geom_ribbon(
			ggplot2::aes(
				ymin = .data$ci_lower,
				ymax = .data$ci_upper,
				fill = .data$treatment
			),
			alpha = 0.2,
			color = NA
		) +
		ggplot2::labs(
			title = title,
			x = "Time",
			y = "Cumulative Incidence",
			color = "Treatment",
			fill = "Treatment"
		) +
		ggplot2::theme_minimal()

	ClinicalPlot(
		plot = p,
		type = "cif_plot",
		title = title,
		data = cif_data,
		metadata = list(
			main_event = result@main_event,
			competing_events = result@competing_events
		)
	)
}
