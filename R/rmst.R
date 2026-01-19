#' @title Restricted Mean Survival Time (RMST)
#' @name rmst
#' @description Functions for restricted mean survival time analysis using
#'   Kaplan-Meier estimation with treatment group comparisons.
#' @references IQWiG General Methods 10.3.8 p.227-229
NULL

# =============================================================================
# RMSTResult S7 Class
# =============================================================================

#' RMSTResult Class
#'
#' An S7 class for storing RMST analysis results.
#'
#' @export
#'
#' @param rmst_by_group Data frame with RMST estimates by treatment group
#' @param rmst_difference Numeric. Difference in RMST between groups
#' @param se_difference Standard error of the difference
#' @param ci Numeric vector c(lower, upper) confidence interval for difference
#' @param p_value P-value for treatment difference test
#' @param tau Numeric. Time restriction used for RMST calculation
#' @param treatment_comparison Data frame with treatment comparison results
#' @param n_obs Number of observations
#' @param n_events Vector of event counts by group
#' @param metadata List of additional metadata
#'
#' @return An RMSTResult object
RMSTResult <- S7::new_class(
	"RMSTResult",
	package = "pharmhand",
	properties = list(
		rmst_by_group = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame()),
			validator = function(value) {
				if (!is.data.frame(value)) {
					return("rmst_by_group must be a data frame")
				}
				NULL
			}
		),
		rmst_difference = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (length(value) != 1) {
					return("rmst_difference must be a single numeric value")
				}
				NULL
			}
		),
		se_difference = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (length(value) != 1 || value < 0) {
					return("se_difference must be a single positive value")
				}
				NULL
			}
		),
		ci = S7::new_property(
			S7::class_any,
			validator = function(value) {
				if (length(value) != 2) {
					return("ci must be a numeric vector of length 2")
				}
				if (!is.numeric(value)) {
					return("ci must be numeric")
				}
				NULL
			}
		),
		p_value = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (length(value) != 1 || value < 0 || value > 1) {
					return("p_value must be a single value between 0 and 1")
				}
				NULL
			}
		),
		tau = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (length(value) != 1 || value <= 0) {
					return("tau must be a single positive value")
				}
				NULL
			}
		),
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
		n_obs = S7::new_property(S7::class_integer),
		n_events = S7::new_property(S7::class_numeric),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

# =============================================================================
# RMST Analysis Functions
# =============================================================================

#' RMST Analysis
#'
#' Calculates restricted mean survival time with treatment group comparisons.
#'
#' @param data A data frame containing survival data
#' @param time_var Character. Name of the time variable
#' @param event_var Character. Name of the event indicator variable
#'   (1=event, 0=censored)
#' @param trt_var Character. Name of the treatment variable
#' @param tau Numeric. Time restriction for RMST calculation
#' @param conf_level Numeric. Confidence level for intervals (default: 0.95)
#' @param reference_group Character. Reference group for treatment comparison.
#'   If NULL, uses first level of treatment variable.
#'
#' @return An RMSTResult object
#'
#' @details
#' The restricted mean survival time (RMST) is defined as:
#' \code{RMST(tau) = E[min(T, tau)] = integral from 0 to tau of S(t) dt}
#'
#' where T is the survival time and S(t) is the survival function.
#'
#' RMST is estimated using the Kaplan-Meier estimator:
#' \code{RMST(tau) = tau - integral from 0 to tau of F(t) dt}
#' \code{= sum_i (t_i - t_i-1) * S(t_i-1)}
#'
#' where the integral is restricted to \code{[0, tau]}.
#'
#' @references
#' Uno, H. et al. (2014). Moving beyond the hazard ratio in quantifying the
#' between-group difference in survival analysis. Journal of Clinical Oncology,
#' 32(22), 2380-2385.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic RMST analysis
#' result <- rmst_analysis(
#'   data = survival_data,
#'   time_var = "time",
#'   event_var = "status",
#'   trt_var = "TRT01P",
#'   tau = 12
#' )
#'
#' # With custom confidence level
#' result <- rmst_analysis(
#'   data = survival_data,
#'   time_var = "time",
#'   event_var = "status",
#'   trt_var = "TRT01P",
#'   tau = 24,
#'   conf_level = 0.90
#' )
#' }
rmst_analysis <- function(
	data,
	time_var,
	event_var,
	trt_var,
	tau,
	conf_level = 0.95,
	reference_group = NULL
) {
	# Validate inputs
	if (!is.data.frame(data)) {
		ph_abort("'data' must be a data frame")
	}

	# Check survRM2 package availability
	if (!requireNamespace("survRM2", quietly = TRUE)) {
		ph_abort(
			"Package 'survRM2' is required for RMST analysis. ",
			"Install with: install.packages('survRM2')"
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

	# Validate tau
	if (!is.numeric(tau) || length(tau) != 1 || tau <= 0) {
		ph_abort("'tau' must be a single positive numeric value")
	}

	# Check required variables exist
	required_vars <- c(time_var, event_var, trt_var)
	if (!all(required_vars %in% names(data))) {
		missing <- setdiff(required_vars, names(data))
		ph_abort(sprintf(
			"Required variables not found: %s",
			paste(missing, collapse = ", ")
		))
	}

	# Prepare data
	analysis_data <- data[, c(time_var, event_var, trt_var)]
	colnames(analysis_data) <- c("time", "status", "treatment")

	# Remove rows with missing values
	complete_data <- analysis_data[complete.cases(analysis_data), ]
	n_removed <- nrow(analysis_data) - nrow(complete_data)
	if (n_removed > 0) {
		ph_inform(sprintf("Removed %d rows with missing values", n_removed))
	}

	# Check data quality
	if (any(complete_data$time <= 0)) {
		ph_warn(
			"Some time values are <= 0. survRM2 will handle these appropriately."
		)
	}

	# Set reference group
	trt_levels <- unique(complete_data$treatment)
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

	# Set factor levels with reference_group first
	complete_data$treatment <- factor(
		complete_data$treatment,
		levels = c(reference_group, setdiff(trt_levels, reference_group))
	)

	# Convert treatment factor to 0/1 numeric for survRM2
	# 0 = reference/control, 1 = active/treatment
	if (length(levels(complete_data$treatment)) != 2) {
		ph_abort(sprintf(
			"RMST analysis requires exactly 2 treatment groups, found %d: %s",
			length(levels(complete_data$treatment)),
			paste(levels(complete_data$treatment), collapse = ", ")
		))
	}
	treatment_numeric <- as.integer(complete_data$treatment) - 1L

	# Perform RMST analysis using survRM2
	rmst_results <- tryCatch(
		{
			survRM2::rmst2(
				time = complete_data$time,
				status = complete_data$status,
				group = treatment_numeric,
				tau = tau
			)
		},
		error = function(e) {
			ph_abort(sprintf("RMST analysis failed: %s", conditionMessage(e)))
		}
	)

	# Extract results
	rmst_by_group <- rmst_results$RMST_summary
	rmst_diff <- rmst_results$unadjusted$result

	# Extract difference results using named column access
	# Row 1 is the RMST difference: "RMST (arm=1)-(arm=0)"
	diff_estimate <- rmst_diff["RMST (arm=1)-(arm=0)", "Est."]
	diff_se <- rmst_diff["RMST (arm=1)-(arm=0)", "se"]

	# Calculate confidence interval for difference
	alpha <- 1 - conf_level
	z_crit <- qnorm(1 - alpha / 2)
	diff_ci <- c(
		diff_estimate - z_crit * diff_se,
		diff_estimate + z_crit * diff_se
	)

	# Create treatment comparison
	treatment_levels <- unique(complete_data$treatment)
	if (length(treatment_levels) >= 2) {
		treatment_comp <- data.frame(
			group = treatment_levels,
			rmst = rmst_by_group[, "RMST"],
			se = rmst_by_group[, "SE"],
			ci_lower = rmst_by_group[, "lower.CL"],
			ci_upper = rmst_by_group[, "upper.CL"],
			stringsAsFactors = FALSE
		)
	} else {
		treatment_comp <- data.frame()
	}

	# Calculate p-value for difference
	p_value <- rmst_diff["RMST (arm=1)-(arm=0)", "p"]

	# Count events by group
	event_counts <- table(complete_data$treatment, complete_data$status)
	if ("1" %in% colnames(event_counts)) {
		n_events <- as.numeric(event_counts[, "1"])
	} else {
		# No events occurred - create zero vector
		n_events <- rep(0, nrow(event_counts))
	}
	names(n_events) <- rownames(event_counts)

	RMSTResult(
		rmst_by_group = rmst_by_group,
		rmst_difference = diff_estimate,
		se_difference = diff_se,
		ci = diff_ci,
		p_value = p_value,
		tau = tau,
		treatment_comparison = treatment_comp,
		n_obs = nrow(complete_data),
		n_events = n_events,
		metadata = list(
			time_var = time_var,
			event_var = event_var,
			trt_var = trt_var,
			conf_level = conf_level,
			reference_group = reference_group,
			n_missing = n_removed
		)
	)
}

#' Create RMST Table
#'
#' Creates a clinical table from RMST analysis results.
#'
#' @param result An RMSTResult object
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
#' table <- create_rmst_table(result, title = "RMST Analysis")
#' }
create_rmst_table <- function(
	result,
	title = "Restricted Mean Survival Time Analysis",
	subtitle = NULL,
	footnotes = NULL,
	autofit = TRUE
) {
	if (!S7::S7_inherits(result, RMSTResult)) {
		ph_abort("'result' must be an RMSTResult object")
	}

	# Get treatment comparison data
	comp_data <- result@treatment_comparison

	# Get confidence level for dynamic labeling (shared across branches)
	conf_level <- result@metadata$conf_level
	ci_label <- paste0(round(conf_level * 100, 0), "% CI")

	if (nrow(comp_data) == 0) {
		ph_warn("No treatment comparison data available for table creation")
		comp_data <- data.frame(
			Group = "No comparison",
			RMST = NA_real_,
			SE = NA_real_,
			CI = "Not available",
			stringsAsFactors = FALSE
		)
	} else {
		# Format confidence intervals
		comp_data$CI <- sprintf(
			"%.2f (%.2f, %.2f)",
			comp_data$rmst,
			comp_data$ci_lower,
			comp_data$ci_upper
		)

		comp_data <- comp_data[, c("group", "rmst", "se", "CI")]
		colnames(comp_data) <- c("Group", "RMST", "SE", ci_label)
	}

	# Add difference information if available
	if (!is.na(result@rmst_difference)) {
		diff_row <- data.frame(
			Group = "Difference",
			RMST = round(result@rmst_difference, 3),
			SE = round(result@se_difference, 3),
			CI = sprintf(
				"%.3f (%.3f, %.3f)",
				result@rmst_difference,
				result@ci[1],
				result@ci[2]
			),
			check.names = FALSE
		)
		colnames(diff_row)[4] <- ci_label

		comp_data <- rbind(comp_data, diff_row)
	}

	# Add metadata
	meta_footnotes <- c(
		paste("Time restriction (tau):", result@tau),
		paste("Observations:", result@n_obs),
		paste("Total events:", sum(result@n_events)),
		paste("P-value:", format_pvalue(result@p_value)),
		if (!is.null(footnotes)) footnotes
	)

	create_clinical_table(
		data = comp_data,
		title = title,
		footnotes = meta_footnotes,
		autofit = autofit
	)
}

#' Plot RMST Results
#'
#' Creates a plot of RMST results by treatment group.
#'
#' @param result An RMSTResult object
#' @param title Plot title
#'
#' @return A ClinicalPlot object
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_rmst(result, title = "RMST by Treatment Group")
#' }
plot_rmst <- function(
	result,
	title = "Restricted Mean Survival Time by Treatment Group"
) {
	if (!S7::S7_inherits(result, RMSTResult)) {
		ph_abort("'result' must be an RMSTResult object")
	}

	comp_data <- result@treatment_comparison
	if (nrow(comp_data) == 0) {
		ph_warn("No treatment comparison data available for plotting")
		p <- ggplot2::ggplot() +
			ggplot2::theme_void() +
			ggplot2::labs(title = title, subtitle = "No data available")

		return(ClinicalPlot(
			plot = p,
			type = "rmst_plot",
			title = title
		))
	}

	p <- ggplot2::ggplot(
		comp_data,
		ggplot2::aes(x = .data$group, y = .data$rmst)
	) +
		ggplot2::geom_point(size = 3) +
		ggplot2::geom_errorbar(
			ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
			width = 0.2
		) +
		ggplot2::labs(
			title = title,
			x = "Treatment Group",
			y = sprintf("RMST (tau = %.1f)", result@tau)
		) +
		ggplot2::theme_minimal()

	ClinicalPlot(
		plot = p,
		type = "rmst_plot",
		title = title,
		data = comp_data,
		metadata = list(
			tau = result@tau,
			difference = result@rmst_difference,
			p_value = result@p_value
		)
	)
}
