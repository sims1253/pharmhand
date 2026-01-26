#' @title Time-to-Event Tables
#' @name efficacy_tte
#' @description Functions for time-to-event analysis tables including
#'   Kaplan-Meier summaries.
NULL

#' Internal helper function to compute TTE summary statistics.
#'
#' @param df Filtered data frame with TTE data
#' @param time_var Time variable name
#' @param event_var Event variable name (will be inverted if "CNSR")
#' @param trt_var Treatment variable name
#' @param ref_group Reference group for HR calculation
#' @param conf_level Confidence level for intervals
#' @param check_ph Logical. Whether to test proportional hazards assumption
#' @param landmarks Numeric vector of timepoints for landmark survival estimates
#' @param time_unit Character string for time unit display
#'
#' @return List with:
#'   - results: Data frame with TTE summary statistics
#'   - km_fit: Kaplan-Meier survfit object
#'   - cox_fit: Cox coxph object (NULL if single arm)
#'   - ph_test: PH test results (NULL if check_ph=FALSE)
#'   - ref_group: Reference group used
#' @noRd
.summarize_tte <- function(
	df,
	time_var,
	event_var,
	trt_var,
	ref_group,
	conf_level,
	check_ph,
	landmarks,
	time_unit
) {
	ci_pct <- round(conf_level * 100)

	# Handle CNSR inversion (ADaM: 0=event, 1=censor -> survival: 1=event)
	if (event_var == "CNSR") {
		df$event <- 1 - df[[event_var]]
	} else {
		df$event <- df[[event_var]]
	}

	# Ensure treatment is factor for consistent ordering
	df[[trt_var]] <- as.factor(df[[trt_var]])
	trt_levels <- levels(df[[trt_var]])

	if (is.null(ref_group)) {
		ref_group <- trt_levels[1]
	}

	if (!is.logical(check_ph) || length(check_ph) != 1 || is.na(check_ph)) {
		ph_abort("'check_ph' must be TRUE or FALSE")
	}

	cox_fit <- NULL
	ph_test <- NULL

	# Build survival formula
	surv_formula <- stats::as.formula(
		paste0("survival::Surv(", time_var, ", event) ~ ", trt_var)
	)

	# Fit Kaplan-Meier
	km_fit <- survival::survfit(surv_formula, data = df, conf.int = conf_level)

	# Extract median and CI from survfit
	km_summary <- summary(km_fit)$table
	if (is.null(dim(km_summary))) {
		# Single treatment arm - convert to matrix and preserve column names
		col_names <- names(km_summary)
		km_summary <- matrix(km_summary, nrow = 1)
		colnames(km_summary) <- col_names
		rownames(km_summary) <- trt_levels[1]
	}

	# Build results data frame
	results <- data.frame(
		Treatment = sub(paste0("^", trt_var, "="), "", rownames(km_summary)),
		stringsAsFactors = FALSE
	)

	# N and events
	results$N <- as.integer(km_summary[, "records"])
	results$Events <- as.integer(km_summary[, "events"])
	events_pct <- ifelse(
		!is.na(results$N) & results$N > 0,
		results$Events / results$N * 100,
		0
	)
	results$Events_pct <- sprintf("%.1f", events_pct)
	results$`Events n (%)` <- paste0(
		results$Events,
		" (",
		results$Events_pct,
		")"
	)

	# Median survival with CI
	median_col <- if ("median" %in% colnames(km_summary)) {
		"median"
	} else {
		"*rmean"
	}
	time_summary_label <- if (median_col == "median") "Median" else "RMST"
	# Find CI columns by pattern - more robust across survival versions
	ci_lower_name <- grep("LCL$", colnames(km_summary), value = TRUE)[1]
	ci_upper_name <- grep("UCL$", colnames(km_summary), value = TRUE)[1]
	if (is.na(ci_lower_name) || is.na(ci_upper_name)) {
		ph_abort("Could not find confidence interval columns in survfit output")
	}

	time_summary_col_name <- paste0(time_summary_label, " (", ci_pct, "% CI)")

	fmt_time <- function(x) {
		ifelse(is.na(x) | !is.finite(x), "NE", sprintf("%.1f", x))
	}
	time_est <- km_summary[, median_col]
	time_lcl <- km_summary[, ci_lower_name]
	time_ucl <- km_summary[, ci_upper_name]
	time_est_str <- fmt_time(time_est)
	time_lcl_str <- fmt_time(time_lcl)
	time_ucl_str <- fmt_time(time_ucl)

	results[[time_summary_col_name]] <- ifelse(
		time_est_str == "NE",
		"NE",
		paste0(time_est_str, " (", time_lcl_str, ", ", time_ucl_str, ")")
	)

	# Landmark estimates if requested
	if (!is.null(landmarks) && length(landmarks) > 0) {
		landmark_summary <- summary(km_fit, times = landmarks, extend = TRUE)

		# Expand strata counts into per-row labels
		strata_labels <- rep(
			names(landmark_summary$strata),
			times = as.integer(landmark_summary$strata)
		)

		for (i in seq_along(landmarks)) {
			time_pt <- landmarks[i]
			col_name <- paste0(time_pt, "-", time_unit, " Rate (", ci_pct, "% CI)")

			# Extract survival estimates for this timepoint
			if (length(trt_levels) > 1) {
				# For each treatment level, find the matching stratum
				surv_vals <- numeric(length(trt_levels))
				lower_vals <- numeric(length(trt_levels))
				upper_vals <- numeric(length(trt_levels))

				for (k in seq_along(trt_levels)) {
					trt <- trt_levels[k]
					# Stratum format is "TRT01P=Active", "TRT01P=Placebo", etc.
					stratum_name <- paste0(trt_var, "=", trt)

					# Find rows matching both time point and stratum
					time_idx <- which(landmark_summary$time == time_pt)
					if (length(time_idx) == 0) {
						# No exact match - find nearest time point
						ph_warn(sprintf(
							"No exact match for landmark timepoint %.1f, finding nearest",
							time_pt
						))
						time_idx <- which.min(abs(landmark_summary$time - time_pt))
					}

					# Find row index that matches both time point and stratum
					# Use expanded strata_labels to identify which rows belong to which stratum
					row_idx <- which(
						landmark_summary$time == time_pt & strata_labels == stratum_name
					)

					if (length(row_idx) == 0) {
						# Fallback: use first time-matched row if no exact stratum found
						if (length(time_idx) > 0) {
							row_idx <- time_idx[1]
						} else {
							ph_warn(sprintf(
								"No stratum match for treatment %s at time %.1f",
								trt,
								time_pt
							))
							next
						}
					}

					surv_vals[k] <- landmark_summary$surv[row_idx]
					lower_vals[k] <- landmark_summary$lower[row_idx]
					upper_vals[k] <- landmark_summary$upper[row_idx]
				}
			} else {
				surv_vals <- landmark_summary$surv[i]
				lower_vals <- landmark_summary$lower[i]
				upper_vals <- landmark_summary$upper[i]
			}

			results[[col_name]] <- sprintf(
				"%.1f (%.1f, %.1f)",
				surv_vals * 100,
				lower_vals * 100,
				upper_vals * 100
			)
		}
	}

	# Hazard Ratio from Cox model (only if >1 treatment)
	hr_col_name <- paste0("HR (", ci_pct, "% CI)")
	if (length(trt_levels) > 1) {
		# Relevel to set reference group
		df[[trt_var]] <- stats::relevel(
			df[[trt_var]],
			ref = ref_group
		)

		cox_fit <- survival::coxph(surv_formula, data = df)
		cox_summary <- summary(cox_fit, conf.int = conf_level)

		if (isTRUE(check_ph)) {
			ph_test <- tryCatch(
				test_ph_assumption(cox_fit, alpha = 0.05, plot = FALSE),
				error = function(e) {
					ph_warn(sprintf(
						"PH assumption test could not be performed: %s",
						conditionMessage(e)
					))
					NULL
				}
			)
			if (!is.null(ph_test) && isTRUE(ph_test$violation)) {
				violations <- ph_test$results[
					ph_test$results$violation,
					,
					drop = FALSE
				]
				violated_vars <- violations$variable
				pvals <- violations$p_value
				ph_warn(sprintf(
					paste(
						"Proportional hazards assumption may be violated for: %s.",
						"P-value(s): %s.",
						"Consider stratified analysis, time-varying coefficients,",
						"or restricted mean survival time (RMST) analysis."
					),
					paste(violated_vars, collapse = ", "),
					paste(format_pvalue(pvals), collapse = ", ")
				))
			}
		}

		# Extract HR and CI
		hr_table <- cox_summary$conf.int
		hr_coefs <- cox_summary$coefficients

		results[[hr_col_name]] <- NA_character_
		results$`p-value` <- NA_character_

		# Reference group gets "Reference"
		ref_idx <- which(results$Treatment == ref_group)
		results[[hr_col_name]][ref_idx] <- "Reference"
		results$`p-value`[ref_idx] <- "-"

		# Build dynamic CI column names based on conf_level
		# Format: "lower .95", "upper .95" for 95% CI
		dec <- sub(
			"^0\\.",
			"",
			format(conf_level, scientific = FALSE, trim = TRUE)
		)
		if (nchar(dec) == 1) {
			dec <- paste0(dec, "0")
		}
		lower_col <- paste0("lower .", dec)
		upper_col <- paste0("upper .", dec)

		# Other groups get HR values
		for (j in seq_len(nrow(hr_table))) {
			trt_name <- sub(paste0("^", trt_var), "", rownames(hr_table)[j])
			trt_name <- sub("^[,\\s]+", "", trt_name)
			trt_idx <- which(results$Treatment == trt_name)

			if (length(trt_idx) > 0) {
				results[[hr_col_name]][trt_idx] <- sprintf(
					"%.2f (%.2f, %.2f)",
					hr_table[j, "exp(coef)"],
					hr_table[j, lower_col],
					hr_table[j, upper_col]
				)
				results$`p-value`[trt_idx] <- format_pvalue(
					hr_coefs[j, "Pr(>|z|)"]
				)
			}
		}
	}

	list(
		results = results,
		km_fit = km_fit,
		cox_fit = cox_fit,
		ph_test = ph_test,
		ref_group = ref_group,
		ci_pct = ci_pct,
		time_summary_label = time_summary_label
	)
}

#' Internal helper function to pivot and format TTE summary data for display.
#'
#' @param tte_data Summary data from .summarize_tte
#' @param landmarks Numeric vector of landmark timepoints
#' @param time_unit Character string for time unit display
#'
#' @return Formatted data frame ready for flextable
#' @noRd
.format_tte_table <- function(tte_data, landmarks, time_unit) {
	results <- tte_data$results
	ci_pct <- tte_data$ci_pct
	time_summary_label <- tte_data$time_summary_label
	hr_col_name <- paste0("HR (", ci_pct, "% CI)")

	# Select columns for display
	display_cols <- c(
		"Treatment",
		"N",
		"Events n (%)",
		paste0(time_summary_label, " (", ci_pct, "% CI)")
	)

	# Add landmark columns if present
	if (!is.null(landmarks)) {
		landmark_cols <- paste0(
			landmarks,
			"-",
			time_unit,
			" Rate (",
			ci_pct,
			"% CI)"
		)
		display_cols <- c(display_cols, landmark_cols)
	}

	# Add HR columns if present
	if (hr_col_name %in% names(results)) {
		display_cols <- c(display_cols, hr_col_name, "p-value")
	}

	display_df <- results[, display_cols, drop = FALSE]

	# Convert N to character for consistent pivoting
	display_df$N <- as.character(display_df$N)

	# Transpose for display (rows = statistics, columns = treatments)
	display_transposed <- display_df |>
		tidyr::pivot_longer(
			cols = -"Treatment",
			names_to = "Statistic",
			values_to = "Value"
		) |>
		tidyr::pivot_wider(
			names_from = "Treatment",
			values_from = "Value"
		)

	display_transposed
}

#' Create Time-to-Event Summary Table
#'
#' Generates a standard TTE summary table with median survival, confidence
#' intervals, hazard ratios, and optional landmark estimates for efficacy
#' endpoints like OS, PFS, EFS.
#'
#' @param data An ADaMData object (with domain "ADTTE") or an ADTTE data frame.
#'   Required columns include: time variable (default: "AVAL"), event variable
#'   (default: "CNSR"), and the treatment variable (default: "TRT01P" for data
#'   frames, or @trt_var for ADaMData).
#' @param time_var Time variable name (default: "AVAL")
#' @param event_var Event indicator variable. If "CNSR" (ADaM censoring flag),
#'   it will be automatically inverted (0=event becomes 1=event).
#'   Otherwise expects 1=event, 0=censor. Default: "CNSR"
#' @param trt_var Treatment variable name (default: "TRT01P"). Ignored for
#'   ADaMData objects which use their own trt_var property.
#' @param ref_group Reference group for HR calculation. If NULL, uses first
#'   level of treatment variable.
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param check_ph Logical. Whether to test proportional hazards assumption and
#'   warn on violations (default: TRUE)
#' @param landmarks Numeric vector of timepoints for landmark survival
#'   estimates (e.g., c(12, 24) for 12 and 24 month rates). NULL for none.
#' @param time_unit Character string for time unit display (default: "months")
#' @param title Table title
#' @param footnotes Character vector of footnotes to append to the table.
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @param ... Additional arguments passed to \code{\link{create_clinical_table}}
#'
#' @return A ClinicalTable object with TTE summary statistics
#'
#' @examples
#' # Create time-to-event summary with data frame
#' adtte <- data.frame(
#'   USUBJID = sprintf("SUBJ%02d", 1:40),
#'   TRT01P = rep(c("Placebo", "Active"), each = 20),
#'   AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
#'   CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3))
#' )
#' table <- create_tte_summary_table(adtte)
#' print(table)
#'
#' @export
create_tte_summary_table <- function(
	data,
	time_var = "AVAL",
	event_var = "CNSR",
	trt_var = "TRT01P",
	ref_group = NULL,
	conf_level = 0.95,
	check_ph = TRUE,
	landmarks = NULL,
	time_unit = "months",
	title = "Time-to-Event Summary",
	footnotes = character(),
	autofit = TRUE,
	...
) {
	# Ensure ADaMData object with proper domain, passing trt_var for data frames
	data <- .ensure_adam_data(
		data,
		"ADTTE",
		trt_var = trt_var,
		subject_var = "USUBJID"
	)

	# Use trt_var from ADaMData object (set during coercion)
	trt_var <- data@trt_var

	# Use filtered_data (respects population filter)
	df <- data@filtered_data

	# Validate required columns
	required_cols <- c(time_var, event_var, trt_var)
	missing_cols <- setdiff(required_cols, names(df))
	if (length(missing_cols) > 0) {
		ph_abort(
			sprintf(
				"'data' is missing required columns: %s. Required: %s",
				paste(missing_cols, collapse = ", "),
				paste(required_cols, collapse = ", ")
			)
		)
	}

	# Summarize TTE data
	tte_summary <- .summarize_tte(
		df = df,
		time_var = time_var,
		event_var = event_var,
		trt_var = trt_var,
		ref_group = ref_group,
		conf_level = conf_level,
		check_ph = check_ph,
		landmarks = landmarks,
		time_unit = time_unit
	)

	# Format for display
	tte_wide <- .format_tte_table(tte_summary, landmarks, time_unit)

	# Create footnotes
	trt_levels <- levels(factor(df[[trt_var]]))
	default_footnotes <- c(
		paste(data@population, "Population"),
		paste("Time unit:", time_unit),
		if (length(trt_levels) > 1) {
			paste("HR reference group:", tte_summary$ref_group)
		} else {
			NULL
		},
		if (tte_summary$time_summary_label == "RMST") {
			"RMST = Restricted mean survival time"
		} else {
			NULL
		},
		"NE = Not Estimable"
	)
	all_footnotes <- c(footnotes, default_footnotes)

	# Use factory function
	clinical_tbl <- create_clinical_table(
		data = tte_wide,
		type = "tte_summary",
		title = title,
		footnotes = all_footnotes,
		autofit = autofit,
		...
	)

	# Add TTE-specific metadata
	clinical_tbl@metadata <- c(
		clinical_tbl@metadata,
		list(
			km_fit = tte_summary$km_fit,
			cox_fit = tte_summary$cox_fit,
			ph_test = tte_summary$ph_test,
			ref_group = tte_summary$ref_group,
			landmarks = landmarks,
			time_unit = time_unit
		)
	)

	clinical_tbl
}

#' Test Proportional Hazards Assumption
#'
#' Tests the proportional hazards assumption for Cox regression models
#' using Schoenfeld residuals (cox.zph test).
#'
#' @param data An ADaMData object (with domain "ADTTE") or an ADTTE data frame
#'   with time-to-event data, or a coxph model object
#' @param time_var Character. Time variable (required if data is a data frame)
#' @param event_var Character. Event variable (required if data is a data frame)
#' @param trt_var Treatment variable name (default: "TRT01P"). Ignored for
#'   ADaMData objects which use their own trt_var property.
#' @param covariates Character vector. Additional covariates to include
#' @param alpha Numeric. Significance level for flagging violations
#'   (default: 0.05)
#' @param plot Logical. Whether to create diagnostic plot (default: FALSE)
#'
#' @return A list with:
#'   - results: Data frame with variable, rho, chisq, p-value, violation flag
#'   - global_test: Global test result (p-value)
#'   - violation: Logical, TRUE if any p < alpha
#'   - model: The fitted coxph model
#'   - zph: The cox.zph result object
#'   - plot: ClinicalPlot if plot=TRUE
#'
#' @export
#'
#' @references
#' Grambsch, P. M. and Therneau, T. M. (1994).
#' Proportional hazards tests and diagnostics based on weighted residuals.
#' Biometrika, 81, 515-26.
#'
#' IQWiG Methods v8.0, Section 10.3.12, p. 235-237.
#'
#' @examples
#' \dontrun{
#' result <- test_ph_assumption(
#'   data = adtte,
#'   time_var = "AVAL",
#'   event_var = "CNSR",
#'   trt_var = "TRT01P",
#'   plot = TRUE
#' )
#' }
test_ph_assumption <- function(
	data,
	time_var = NULL,
	event_var = NULL,
	trt_var = "TRT01P",
	covariates = character(),
	alpha = 0.05,
	plot = FALSE
) {
	if (!requireNamespace("survival", quietly = TRUE)) {
		ph_abort("Package 'survival' is required for proportional hazards tests")
	}

	if (is.null(covariates)) {
		covariates <- character()
	}
	if (!is.character(covariates)) {
		ph_abort("'covariates' must be a character vector")
	}
	if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
		ph_abort("'plot' must be TRUE or FALSE")
	}

	assert_numeric_scalar(alpha, "alpha")
	assert_in_range(alpha, 0, 1, "alpha")

	# Save whether original input was a data frame (before coercion)
	was_data_frame <- !inherits(data, "coxph") && !S7::S7_inherits(data, ADaMData)

	if (inherits(data, "coxph")) {
		model <- data
	} else {
		# Auto-coerce data.frame to ADaMData
		data <- .ensure_adam_data(
			data,
			"ADTTE",
			trt_var = trt_var,
			subject_var = "USUBJID"
		)

		# Use trt_var from ADaMData object, warn if explicitly passed for ADaMData
		if (missing(trt_var) || trt_var == "TRT01P") {
			trt_var <- data@trt_var
		} else if (S7::S7_inherits(data, ADaMData) && !was_data_frame) {
			ph_warn(
				"'trt_var' argument is ignored for ADaMData objects. ",
				"Using stored 'trt_var' property."
			)
			trt_var <- data@trt_var
		}

		# Use filtered_data (respects population filter)
		df <- data@filtered_data

		# For data frames, require time_var and event_var
		# For ADaMData, use defaults if not specified
		if (was_data_frame) {
			if (is.null(time_var)) {
				ph_abort("'time_var' is required when 'data' is a data frame")
			}
			if (is.null(event_var)) {
				ph_abort("'event_var' is required when 'data' is a data frame")
			}
		} else {
			# ADaMData object: use default column names
			if (is.null(time_var)) {
				time_var <- "AVAL"
			}
			if (is.null(event_var)) {
				event_var <- "CNSR"
			}
		}

		assert_character_scalar(time_var, "time_var")
		assert_character_scalar(event_var, "event_var")
		assert_character_scalar(trt_var, "trt_var")

		required_cols <- c(time_var, event_var, trt_var, covariates)
		missing_cols <- setdiff(required_cols, names(df))
		if (length(missing_cols) > 0) {
			ph_abort(sprintf(
				"'data' is missing required columns: %s",
				paste(missing_cols, collapse = ", ")
			))
		}

		# Handle CNSR inversion (ADaM: 0=event, 1=censor -> survival: 1=event)
		if (event_var == "CNSR") {
			df$event <- 1 - df[[event_var]]
			event_var_use <- "event"
		} else {
			event_var_use <- event_var
		}

		df[[trt_var]] <- as.factor(df[[trt_var]])
		predictors <- c(trt_var, covariates)
		formula_str <- paste0(
			"survival::Surv(",
			time_var,
			", ",
			event_var_use,
			") ~ ",
			paste(predictors, collapse = " + ")
		)

		model <- survival::coxph(stats::as.formula(formula_str), data = df)
	}

	zph <- survival::cox.zph(model)
	zph_table <- as.data.frame(zph$table)
	p_col <- if ("p" %in% names(zph_table)) {
		"p"
	} else if ("p.value" %in% names(zph_table)) {
		"p.value"
	} else {
		names(zph_table)[ncol(zph_table)]
	}
	zph_table$variable <- rownames(zph$table)

	global_row <- zph_table[zph_table$variable == "GLOBAL", , drop = FALSE]
	global_test <- if (nrow(global_row) > 0) {
		global_row[[p_col]][1]
	} else if (nrow(zph_table) == 1) {
		zph_table[[p_col]][1]
	} else {
		NA_real_
	}

	results_df <- zph_table[zph_table$variable != "GLOBAL", , drop = FALSE]
	rho_values <- if ("rho" %in% names(results_df)) {
		results_df[["rho"]]
	} else {
		rep(NA_real_, nrow(results_df))
	}
	chisq_values <- if ("chisq" %in% names(results_df)) {
		results_df[["chisq"]]
	} else {
		rep(NA_real_, nrow(results_df))
	}
	results_df <- data.frame(
		variable = results_df$variable,
		rho = rho_values,
		chisq = chisq_values,
		p_value = results_df[[p_col]],
		stringsAsFactors = FALSE
	)
	results_df$violation <- !is.na(results_df$p_value) &
		results_df$p_value < alpha

	plot_obj <- NULL
	if (isTRUE(plot)) {
		plot_obj <- build_schoenfeld_plot(zph)
	}

	list(
		results = results_df,
		global_test = global_test,
		violation = any(results_df$violation, na.rm = TRUE),
		model = model,
		zph = zph,
		plot = plot_obj
	)
}

#' @keywords internal
build_schoenfeld_plot <- function(
	zph,
	title = "Schoenfeld Residuals",
	base_size = 11
) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		ph_abort("Package 'ggplot2' is required for Schoenfeld residual plots")
	}

	time_values <- zph$time
	if (is.null(time_values)) {
		time_values <- zph$x
	}
	if (is.null(time_values)) {
		ph_abort("Schoenfeld residuals do not contain time values for plotting")
	}

	resid_matrix <- zph$y
	if (is.null(resid_matrix)) {
		ph_abort("Schoenfeld residuals are missing from the cox.zph result")
	}
	if (is.null(dim(resid_matrix))) {
		resid_matrix <- matrix(resid_matrix, ncol = 1)
		if (!is.null(names(zph$y))) {
			colnames(resid_matrix) <- names(zph$y)
		}
	}
	if (is.null(colnames(resid_matrix))) {
		colnames(resid_matrix) <- paste0("term_", seq_len(ncol(resid_matrix)))
	}

	order_idx <- order(time_values)
	plot_time <- time_values[order_idx]
	resid_matrix <- resid_matrix[order_idx, , drop = FALSE]

	plot_df <- data.frame(
		time = rep(plot_time, times = ncol(resid_matrix)),
		residual = as.vector(resid_matrix),
		term = rep(colnames(resid_matrix), each = length(plot_time)),
		stringsAsFactors = FALSE
	)
	plot_df <- plot_df[!is.na(plot_df$residual), , drop = FALSE]
	plot_df$term <- factor(plot_df$term, levels = unique(colnames(resid_matrix)))

	base_plot <- ggplot2::ggplot(
		plot_df,
		ggplot2::aes(x = .data$time, y = .data$residual)
	) +
		ggplot2::geom_hline(
			yintercept = 0,
			linetype = "dashed",
			color = "gray50"
		) +
		ggplot2::geom_point(alpha = 0.6, size = 1) +
		ggplot2::facet_wrap(~term, scales = "free_y", ncol = 2) +
		ggplot2::labs(
			title = title,
			x = "Time",
			y = "Scaled Schoenfeld Residuals"
		) +
		.pharmhand_theme(base_size = base_size) +
		ggplot2::theme(
			plot.title = ggplot2::element_text(hjust = 0.5),
			panel.grid.minor = ggplot2::element_blank()
		)

	if (length(unique(plot_time)) >= 4) {
		base_plot <- base_plot +
			ggplot2::geom_smooth(
				method = "loess",
				formula = y ~ x,
				se = FALSE,
				color = "#0072B2",
				linewidth = 0.8
			)
	}

	n_terms <- ncol(resid_matrix)
	n_rows <- ceiling(n_terms / 2)
	height <- max(4, 1.8 * n_rows + 2)

	ClinicalPlot(
		plot = base_plot,
		title = title,
		width = 7,
		height = height,
		dpi = 300
	)
}
