#' @title Meta-Analysis Functions
#' @name meta_analysis
#' @description Functions for performing meta-analyses and evidence synthesis.
NULL

#' Perform Meta-Analysis
#'
#' Conducts fixed-effect or random-effects meta-analysis on a set of studies.
#' Supports binary, continuous, and time-to-event outcomes.
#'
#' @param data Data frame with study-level summary statistics OR a
#'   StudySet object
#' @param yi Numeric vector of effect estimates (log scale for ratios)
#' @param sei Numeric vector of standard errors
#' @param ni Numeric vector of sample sizes (optional)
#' @param study_labels Character vector of study names
#' @param effect_measure Character. Type of effect: "hr", "or", "rr",
#'   "rd", "md", "smd"
#' @param model Character. "fixed" or "random". Default: "random"
#' @param method Character. Estimation method for random effects:
#'   "DL" (DerSimonian-Laird), "REML", "PM" (Paule-Mandel), "ML".
#'   Default: "REML"
#' @param knapp_hartung Logical. Apply Knapp-Hartung adjustment. Default: TRUE
#' @param conf_level Numeric. Confidence level. Default: 0.95
#' @param prediction Logical. Calculate prediction interval. Default: TRUE
#'
#' @return A MetaResult S7 object with pooled estimate and heterogeneity stats
#' @export
#'
#' @examples
#' # Random-effects meta-analysis of 5 studies with hazard ratios
#' result <- meta_analysis(
#'   yi = log(c(0.75, 0.82, 0.68, 0.91, 0.77)),
#'   sei = c(0.12, 0.15, 0.18, 0.14, 0.11),
#'   study_labels = paste("Study", 1:5),
#'   effect_measure = "hr",
#'   model = "random",
#'   method = "REML",
#'   knapp_hartung = TRUE
#' )
#' result@estimate
#' result@ci
#' result@heterogeneity$I2
meta_analysis <- function(
	data = NULL,
	yi = NULL,
	sei = NULL,
	ni = NULL,
	study_labels = NULL,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	model = c("random", "fixed"),
	method = c("REML", "DL", "PM", "ML"),
	knapp_hartung = TRUE,
	conf_level = 0.95,
	prediction = TRUE
) {
	effect_measure <- match.arg(effect_measure)
	model <- match.arg(model)
	method <- match.arg(method)

	# Handle StudySet input
	if (S7::S7_inherits(data, StudySet)) {
		# Extract summary statistics from StudySet
		# This would be implemented based on how results are stored
		ph_abort("StudySet input not yet implemented - provide yi and sei directly")
	}

	# Handle data frame input
	if (!is.null(data) && is.data.frame(data)) {
		if ("yi" %in% names(data)) {
			yi <- data$yi
		}
		if ("sei" %in% names(data)) {
			sei <- data$sei
		}
		if ("ni" %in% names(data)) {
			ni <- data$ni
		}
		if ("study" %in% names(data)) study_labels <- data$study
	}

	# Validate inputs
	if (is.null(yi) || is.null(sei)) {
		ph_abort("Effect estimates (yi) and standard errors (sei) are required")
	}

	if (length(yi) != length(sei)) {
		ph_abort("yi and sei must have the same length")
	}

	k <- length(yi) # number of studies

	if (k < 2) {
		ph_abort("At least 2 studies required for meta-analysis")
	}

	if (is.null(study_labels)) {
		study_labels <- paste("Study", seq_len(k))
	}

	# Calculate weights (inverse variance)
	wi <- 1 / (sei^2)

	# Fixed-effect estimate
	theta_fe <- sum(wi * yi) / sum(wi)
	se_fe <- sqrt(1 / sum(wi))

	# Heterogeneity statistics
	Q <- sum(wi * (yi - theta_fe)^2)
	df <- k - 1
	Q_pvalue <- stats::pchisq(Q, df = df, lower.tail = FALSE)

	# I-squared
	I2 <- if (Q > 0) max(0, (Q - df) / Q) * 100 else 0

	# H-squared
	H2 <- Q / df

	# Tau-squared estimation
	if (model == "random") {
		tau2 <- switch(
			method,
			"DL" = {
				# DerSimonian-Laird
				c_val <- sum(wi) - sum(wi^2) / sum(wi)
				max(0, (Q - df) / c_val)
			},
			"PM" = {
				# Paule-Mandel (iterative)
				pm_tau2 <- 0
				for (iter in 1:100) {
					wi_star <- 1 / (sei^2 + pm_tau2)
					theta_star <- sum(wi_star * yi) / sum(wi_star)
					Q_star <- sum(wi_star * (yi - theta_star)^2)
					if (Q_star <= df) {
						break
					}
					pm_tau2 <- pm_tau2 +
						(Q_star - df) / sum(wi_star * (1 - wi_star / sum(wi_star)))
					pm_tau2 <- max(0, pm_tau2)
				}
				pm_tau2
			},
			"REML" = {
				# REML via Newton-Raphson iteration
				tau2_reml <- max(0, (Q - df) / (sum(wi) - sum(wi^2) / sum(wi)))
				for (iter in 1:50) {
					wi_star <- 1 / (sei^2 + tau2_reml)
					theta_star <- sum(wi_star * yi) / sum(wi_star)
					resid <- yi - theta_star
					ll_deriv <- -0.5 * sum(wi_star) + 0.5 * sum(wi_star^2 * resid^2)
					ll_deriv2 <- 0.5 * sum(wi_star^2) - sum(wi_star^3 * resid^2)
					if (abs(ll_deriv2) < 1e-10) {
						break
					}
					tau2_new <- tau2_reml - ll_deriv / ll_deriv2
					if (abs(tau2_new - tau2_reml) < 1e-8) {
						break
					}
					tau2_reml <- max(0, tau2_new)
				}
				tau2_reml
			},
			"ML" = {
				# Maximum likelihood
				tau2_ml <- max(0, (Q - df) / (sum(wi) - sum(wi^2) / sum(wi)))
				for (iter in 1:50) {
					wi_star <- 1 / (sei^2 + tau2_ml)
					theta_star <- sum(wi_star * yi) / sum(wi_star)
					resid <- yi - theta_star
					ll_deriv <- -0.5 * sum(wi_star) + 0.5 * sum(wi_star^2 * resid^2)
					if (abs(ll_deriv) < 1e-8) {
						break
					}
					tau2_ml <- tau2_ml + 0.1 * sign(ll_deriv)
					tau2_ml <- max(0, tau2_ml)
				}
				tau2_ml
			}
		)

		# Random effects weights and estimate
		wi_re <- 1 / (sei^2 + tau2)
		theta_re <- sum(wi_re * yi) / sum(wi_re)
		se_re <- sqrt(1 / sum(wi_re))

		# Knapp-Hartung adjustment
		if (knapp_hartung && k > 2) {
			resid_re <- yi - theta_re
			s2 <- sum(wi_re * resid_re^2) / (k - 1)
			se_re <- se_re * sqrt(s2)
			use_t <- TRUE
		} else {
			use_t <- FALSE
		}

		theta <- theta_re
		se <- se_re
		weights <- wi_re / sum(wi_re)
	} else {
		theta <- theta_fe
		se <- se_fe
		tau2 <- 0
		weights <- wi / sum(wi)
		use_t <- FALSE
	}

	# Confidence interval
	alpha <- 1 - conf_level
	if (use_t) {
		crit <- stats::qt(1 - alpha / 2, df = k - 1)
	} else {
		crit <- stats::qnorm(1 - alpha / 2)
	}
	ci_lower <- theta - crit * se
	ci_upper <- theta + crit * se

	# P-value
	if (use_t) {
		z <- theta / se
		p_value <- 2 * stats::pt(-abs(z), df = k - 1)
	} else {
		z <- theta / se
		p_value <- 2 * stats::pnorm(-abs(z))
	}

	# Prediction interval (for random effects)
	pred_interval <- NULL
	if (prediction && model == "random" && tau2 > 0 && k > 2) {
		se_pred <- sqrt(se^2 + tau2)
		if (use_t) {
			crit_pred <- stats::qt(1 - alpha / 2, df = k - 2)
		} else {
			# Even without Knapp-Hartung, use t-distribution for prediction intervals
			crit_pred <- stats::qt(1 - alpha / 2, df = k - 2)
		}
		pred_interval <- c(theta - crit_pred * se_pred, theta + crit_pred * se_pred)
	}

	# Transform back from log scale if ratio measure
	if (effect_measure %in% c("hr", "or", "rr")) {
		display_estimate <- exp(theta)
		display_ci <- exp(c(ci_lower, ci_upper))
		if (!is.null(pred_interval)) {
			display_pred <- exp(pred_interval)
		}
	} else {
		display_estimate <- theta
		display_ci <- c(ci_lower, ci_upper)
		display_pred <- pred_interval
	}

	# Create individual study results
	study_results <- lapply(seq_len(k), function(i) {
		if (effect_measure %in% c("hr", "or", "rr")) {
			est <- exp(yi[i])
			ci <- exp(yi[i] + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sei[i])
		} else {
			est <- yi[i]
			ci <- yi[i] + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sei[i]
		}
		ComparisonResult(
			estimate = est,
			ci = ci,
			ci_level = conf_level,
			effect_measure = effect_measure,
			n = if (!is.null(ni)) ni[i] else NA_integer_,
			method = "Individual study"
		)
	})
	names(study_results) <- study_labels

	# Return MetaResult object
	MetaResult(
		estimate = display_estimate,
		ci = display_ci,
		ci_level = conf_level,
		p_value = p_value,
		method = sprintf(
			"%s-effects meta-analysis (%s%s)",
			tools::toTitleCase(model),
			method,
			if (knapp_hartung && model == "random") " with Knapp-Hartung" else ""
		),
		n = k,
		model = model,
		effect_measure = effect_measure,
		heterogeneity = list(
			Q = Q,
			Q_df = df,
			Q_pvalue = Q_pvalue,
			I2 = I2,
			H2 = H2,
			tau2 = tau2,
			tau = sqrt(tau2)
		),
		weights = stats::setNames(weights, study_labels),
		prediction_interval = if (!is.null(pred_interval)) display_pred else NULL,
		study_results = study_results,
		metadata = list(
			knapp_hartung = knapp_hartung,
			method = method,
			yi = yi,
			sei = sei,
			study_labels = study_labels
		)
	)
}

# =============================================================================
# Helper Functions
# =============================================================================

#' Calculate Heterogeneity Statistics
#'
#' Calculates Q, I2, tau2, and H2 statistics for meta-analysis heterogeneity.
#'
#' @param yi Numeric vector of effect estimates
#' @param sei Numeric vector of standard errors
#' @param method Character. tau2 estimation method. Default: "REML"
#'
#' @return A list with components:
#' \describe{
#' \item{Q}{Cochran's Q statistic}
#' \item{Q_df}{Degrees of freedom for Q test}
#' \item{Q_pvalue}{P-value for Q test}
#' \item{I2}{I-squared heterogeneity percentage (0-100)}
#' \item{I2_ci}{95 percent CI for I-squared}
#' \item{H2}{H-squared statistic}
#' \item{H}{H statistic (sqrt of H2)}
#' \item{tau2}{Between-study variance estimate}
#' \item{tau}{Between-study standard deviation}
#' \item{method}{Estimation method used}
#' \item{k}{Number of studies}
#' \item{interpretation}{Verbal interpretation of I2 level}
#' }
#' @export
#' @examples
#' # Calculate heterogeneity for 5 studies
#' yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
#' sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#' het <- calculate_heterogeneity(yi, sei, method = "REML")
#' het$Q
#' het$I2
#' het$tau2
#' het$interpretation
calculate_heterogeneity <- function(
	yi,
	sei,
	method = c("REML", "DL", "PM", "ML")
) {
	method <- match.arg(method)

	k <- length(yi)
	wi <- 1 / (sei^2)

	# Fixed effect estimate
	theta_fe <- sum(wi * yi) / sum(wi)

	# Q statistic
	Q <- sum(wi * (yi - theta_fe)^2)
	df <- k - 1
	Q_pvalue <- stats::pchisq(Q, df = df, lower.tail = FALSE)

	# I-squared with confidence interval (Higgins & Thompson method)
	I2 <- if (Q > 0) max(0, (Q - df) / Q) * 100 else 0

	# I2 confidence interval (using test-based approach)
	if (Q > df) {
		se_lnH <- sqrt((log(Q) - log(df))^2 / (2 * Q - 2))
		lnH <- 0.5 * log(Q / df)
		H_lower <- exp(lnH - 1.96 * se_lnH)
		H_upper <- exp(lnH + 1.96 * se_lnH)
		I2_lower <- max(0, (1 - 1 / H_lower^2) * 100)
		I2_upper <- min(100, (1 - 1 / H_upper^2) * 100)
	} else {
		I2_lower <- 0
		I2_upper <- 0
	}

	# H-squared
	H2 <- Q / df
	H <- sqrt(H2)

	# Tau-squared (using specified method)
	c_val <- sum(wi) - sum(wi^2) / sum(wi)
	tau2_dl <- max(0, (Q - df) / c_val)

	tau2 <- switch(
		method,
		"DL" = tau2_dl,
		"PM" = {
			pm_tau2 <- tau2_dl
			for (iter in 1:100) {
				wi_star <- 1 / (sei^2 + pm_tau2)
				theta_star <- sum(wi_star * yi) / sum(wi_star)
				Q_star <- sum(wi_star * (yi - theta_star)^2)
				if (Q_star <= df) {
					break
				}
				delta <- (Q_star - df) / sum(wi_star * (1 - wi_star / sum(wi_star)))
				pm_tau2 <- max(0, pm_tau2 + delta)
				if (abs(delta) < 1e-8) break
			}
			pm_tau2
		},
		"REML" = {
			tau2_reml <- tau2_dl
			for (iter in 1:50) {
				wi_star <- 1 / (sei^2 + tau2_reml)
				theta_star <- sum(wi_star * yi) / sum(wi_star)
				resid <- yi - theta_star
				ll_deriv <- -0.5 * sum(wi_star) + 0.5 * sum(wi_star^2 * resid^2)
				ll_deriv2 <- 0.5 * sum(wi_star^2) - sum(wi_star^3 * resid^2)
				if (abs(ll_deriv2) < 1e-10) {
					break
				}
				tau2_new <- tau2_reml - ll_deriv / ll_deriv2
				if (abs(tau2_new - tau2_reml) < 1e-8) {
					break
				}
				tau2_reml <- max(0, tau2_new)
			}
			tau2_reml
		},
		"ML" = tau2_dl # Simplified: uses DL as approximation.
		# For true ML, use metafor::rma()
	)

	# Interpretation
	i2_interpretation <- if (I2 < 25) {
		"Low heterogeneity"
	} else if (I2 < 50) {
		"Moderate heterogeneity"
	} else if (I2 < 75) {
		"Substantial heterogeneity"
	} else {
		"Considerable heterogeneity"
	}

	list(
		Q = Q,
		Q_df = df,
		Q_pvalue = Q_pvalue,
		I2 = I2,
		I2_ci = c(I2_lower, I2_upper),
		H2 = H2,
		H = H,
		tau2 = tau2,
		tau = sqrt(tau2),
		method = method,
		k = k,
		interpretation = i2_interpretation
	)
}


#' Perform Leave-One-Out Sensitivity Analysis
#'
#' Recalculates meta-analysis results leaving out each study one at a time
#' to assess the influence of individual studies.
#'
#' @param meta_result A MetaResult object from meta_analysis()
#' @param yi Numeric vector of effect estimates (optional if in meta_result)
#' @param sei Numeric vector of standard errors (optional if in meta_result)
#'
#' @return A list with components:
#' \describe{
#' \item{results}{Data frame with estimates when each study is excluded}
#' \item{influential_studies}{Character vector of influential studies}
#' \item{n_influential}{Number of influential studies}
#' \item{effect_measure}{Effect measure used}
#' \item{model}{Model type (fixed/random)}
#' }
#' @export
#'
#' @examples
#' # Leave-one-out sensitivity analysis
#' yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
#' sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)
#' meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
#' loo <- leave_one_out(meta_res)
#' loo$results
#' loo$influential_studies
leave_one_out <- function(
	meta_result = NULL,
	yi = NULL,
	sei = NULL
) {
	# Extract from MetaResult if provided
	if (!is.null(meta_result) && S7::S7_inherits(meta_result, MetaResult)) {
		if (!is.null(meta_result@metadata$yi)) {
			yi <- meta_result@metadata$yi
		}
		if (!is.null(meta_result@metadata$sei)) {
			sei <- meta_result@metadata$sei
		}
		study_labels <- meta_result@metadata$study_labels
		effect_measure <- meta_result@effect_measure
		model <- meta_result@model
		original_estimate <- if (effect_measure %in% c("hr", "or", "rr")) {
			log(meta_result@estimate)
		} else {
			meta_result@estimate
		}
	} else {
		study_labels <- paste("Study", seq_along(yi))
		effect_measure <- "md"
		model <- "random"
		original_estimate <- NA
	}

	if (is.null(yi) || is.null(sei)) {
		ph_abort("Effect estimates (yi) and standard errors (sei) required")
	}

	k <- length(yi)

	# Leave-one-out analysis
	loo_results <- lapply(seq_len(k), function(i) {
		yi_loo <- yi[-i]
		sei_loo <- sei[-i]

		# Simple random-effects calculation
		wi <- 1 / (sei_loo^2)
		theta_fe <- sum(wi * yi_loo) / sum(wi)
		Q <- sum(wi * (yi_loo - theta_fe)^2)
		df <- length(yi_loo) - 1
		c_val <- sum(wi) - sum(wi^2) / sum(wi)
		tau2 <- max(0, (Q - df) / c_val)

		if (model == "random") {
			wi_re <- 1 / (sei_loo^2 + tau2)
			estimate <- sum(wi_re * yi_loo) / sum(wi_re)
			se <- sqrt(1 / sum(wi_re))
		} else {
			estimate <- theta_fe
			se <- sqrt(1 / sum(wi))
		}

		I2 <- if (Q > 0) max(0, (Q - df) / Q) * 100 else 0

		list(
			estimate = estimate,
			se = se,
			ci_lower = estimate - 1.96 * se,
			ci_upper = estimate + 1.96 * se,
			I2 = I2,
			tau2 = tau2,
			excluded = study_labels[i]
		)
	})

	# Create summary data frame
	loo_df <- data.frame(
		excluded_study = sapply(loo_results, function(x) x$excluded),
		estimate = sapply(loo_results, function(x) x$estimate),
		se = sapply(loo_results, function(x) x$se),
		ci_lower = sapply(loo_results, function(x) x$ci_lower),
		ci_upper = sapply(loo_results, function(x) x$ci_upper),
		I2 = sapply(loo_results, function(x) x$I2),
		stringsAsFactors = FALSE
	)

	# Transform for ratio measures
	if (effect_measure %in% c("hr", "or", "rr")) {
		loo_df$estimate_display <- exp(loo_df$estimate)
		loo_df$ci_lower_display <- exp(loo_df$ci_lower)
		loo_df$ci_upper_display <- exp(loo_df$ci_upper)
	} else {
		loo_df$estimate_display <- loo_df$estimate
		loo_df$ci_lower_display <- loo_df$ci_lower
		loo_df$ci_upper_display <- loo_df$ci_upper
	}

	# Identify influential studies (estimate changes by > 10%)
	if (!is.na(original_estimate)) {
		if (abs(original_estimate) > 1e-10) {
			loo_df$pct_change <- 100 *
				(loo_df$estimate - original_estimate) /
				abs(original_estimate)
		} else {
			# When original estimate is ~0, use absolute change instead
			loo_df$pct_change <- 100 * (loo_df$estimate - original_estimate)
		}
		influential <- abs(loo_df$pct_change) > 10
	} else {
		loo_df$pct_change <- NA
		influential <- rep(FALSE, k)
	}

	list(
		results = loo_df,
		influential_studies = study_labels[influential],
		n_influential = sum(influential),
		effect_measure = effect_measure,
		model = model
	)
}


#' Create Forest Plot for Meta-Analysis
#'
#' Creates a forest plot displaying individual study effects and pooled estimate
#' from a meta-analysis.
#'
#' @param meta_result A MetaResult object from meta_analysis()
#' @param title Character. Plot title. Default: NULL
#' @param xlab Character. X-axis label. Default: based on effect_measure
#' @param show_weights Logical. Show study weights. Default: TRUE
#' @param show_heterogeneity Logical. Show heterogeneity stats. Default: TRUE
#' @param show_prediction Logical. Show prediction interval. Default: TRUE
#' @param null_value Numeric. Reference line value. Default: 1 for ratios,
#'   0 for differences
#' @param xlim Numeric vector. X-axis limits. Default: NULL (auto)
#' @param palette Character vector. Colors. Default: NULL
#' @param base_size Numeric. Base font size. Default: 11
#'
#' @return A ClinicalPlot object containing the forest plot
#' @export
#'
#' @examples
#' # Meta-analysis forest plot
#' yi <- log(c(0.75, 0.82, 0.68, 0.91))
#' sei <- c(0.12, 0.15, 0.18, 0.14)
#' result <- meta_analysis(
#'   yi = yi, sei = sei,
#'   study_labels = c("Study A", "Study B", "Study C", "Study D"),
#'   effect_measure = "hr"
#' )
#' plot <- create_meta_forest_plot(result, title = "Treatment Effect")
#' plot@type
create_meta_forest_plot <- function(
	meta_result,
	title = NULL,
	xlab = NULL,
	show_weights = TRUE,
	show_heterogeneity = TRUE,
	show_prediction = TRUE,
	null_value = NULL,
	xlim = NULL,
	palette = NULL,
	base_size = 11
) {
	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	effect_measure <- meta_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Set defaults based on effect measure
	if (is.null(null_value)) {
		null_value <- if (is_ratio) 1 else 0
	}

	if (is.null(xlab)) {
		xlab <- switch(
			effect_measure,
			"hr" = "Hazard Ratio",
			"or" = "Odds Ratio",
			"rr" = "Risk Ratio",
			"rd" = "Risk Difference",
			"md" = "Mean Difference",
			"smd" = "Standardized Mean Difference",
			"Effect Size"
		)
	}

	# Extract study data
	study_results <- meta_result@study_results
	study_labels <- names(study_results)
	k <- length(study_results)
	weights <- meta_result@weights

	# Build data frame for plotting
	plot_data <- data.frame(
		study = study_labels,
		estimate = sapply(study_results, function(x) x@estimate),
		ci_lower = sapply(study_results, function(x) x@ci[1]),
		ci_upper = sapply(study_results, function(x) x@ci[2]),
		weight = as.numeric(weights) * 100,
		stringsAsFactors = FALSE
	)

	# Add pooled estimate row
	pooled_row <- data.frame(
		study = "Overall",
		estimate = meta_result@estimate,
		ci_lower = meta_result@ci[1],
		ci_upper = meta_result@ci[2],
		weight = 100,
		stringsAsFactors = FALSE
	)

	plot_data <- rbind(plot_data, pooled_row)
	plot_data$is_pooled <- c(rep(FALSE, k), TRUE)
	plot_data$row <- seq_len(nrow(plot_data))

	# Set y-axis order (pooled at bottom)
	plot_data$y_pos <- rev(seq_len(nrow(plot_data)))

	# Transform to log scale for ratios
	if (is_ratio) {
		plot_data$x_plot <- plot_data$estimate
		plot_data$x_lower <- plot_data$ci_lower
		plot_data$x_upper <- plot_data$ci_upper
		x_trans <- "log10"
	} else {
		plot_data$x_plot <- plot_data$estimate
		plot_data$x_lower <- plot_data$ci_lower
		plot_data$x_upper <- plot_data$ci_upper
		x_trans <- "identity"
	}

	# Calculate plot limits
	if (is.null(xlim)) {
		all_vals <- c(plot_data$x_lower, plot_data$x_upper)
		all_vals <- all_vals[is.finite(all_vals)]
		xlim <- range(all_vals)
		# Expand slightly
		xlim <- xlim + c(-1, 1) * diff(xlim) * 0.1
	}

	# Build plot
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$x_plot, y = .data$y_pos)
	)

	# Reference line
	p <- p +
		ggplot2::geom_vline(
			xintercept = null_value,
			linetype = "dashed",
			color = "gray50"
		)

	# Prediction interval (if available and requested)
	if (show_prediction && !is.null(meta_result@prediction_interval)) {
		pred <- meta_result@prediction_interval
		pooled_y <- plot_data$y_pos[plot_data$is_pooled]
		p <- p +
			ggplot2::geom_segment(
				data = data.frame(
					x = pred[1],
					xend = pred[2],
					y = pooled_y - 0.3,
					yend = pooled_y - 0.3
				),
				ggplot2::aes(
					x = .data$x,
					xend = .data$xend,
					y = .data$y,
					yend = .data$yend
				),
				color = "gray60",
				linewidth = 0.5
			)
	}

	# Error bars
	p <- p +
		ggplot2::geom_errorbar(
			ggplot2::aes(xmin = .data$x_lower, xmax = .data$x_upper),
			height = 0.2,
			linewidth = 0.6,
			orientation = "y"
		)

	# Points (diamonds for pooled, squares for studies)
	study_data <- plot_data[!plot_data$is_pooled, ]
	pooled_data <- plot_data[plot_data$is_pooled, ]

	# Study points (size proportional to weight)
	p <- p +
		ggplot2::geom_point(
			data = study_data,
			ggplot2::aes(size = .data$weight),
			shape = 15,
			color = "black"
		)

	# Pooled estimate (diamond)
	p <- p +
		ggplot2::geom_point(
			data = pooled_data,
			shape = 23,
			size = 5,
			fill = "black",
			color = "black"
		)

	# Y-axis labels
	p <- p +
		ggplot2::scale_y_continuous(
			breaks = plot_data$y_pos,
			labels = plot_data$study,
			expand = ggplot2::expansion(mult = 0.05)
		)

	# X-axis transformation
	if (is_ratio) {
		p <- p + ggplot2::scale_x_log10(limits = xlim)
	} else {
		p <- p + ggplot2::scale_x_continuous(limits = xlim)
	}

	# Size scale
	p <- p +
		ggplot2::scale_size_continuous(
			range = c(1, 5),
			guide = if (show_weights) "legend" else "none"
		)

	# Labels
	p <- p +
		ggplot2::labs(
			x = xlab,
			y = NULL,
			title = title,
			size = "Weight (%)"
		)

	# Theme
	p <- p + .pharmhand_theme(base_size = base_size)
	p <- p +
		ggplot2::theme(
			axis.text.y = ggplot2::element_text(hjust = 0),
			panel.grid.major.y = ggplot2::element_blank()
		)

	# Add heterogeneity annotation
	if (show_heterogeneity) {
		het <- meta_result@heterogeneity
		het_text <- sprintf(
			"I2 = %.1f%%, tau2 = %.3f, Q = %.1f (p %s)",
			het$I2,
			het$tau2,
			het$Q,
			format_pvalue(het$Q_pvalue)
		)
		p <- p + ggplot2::labs(caption = het_text)
	}

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = plot_data,
		type = "forest_meta",
		title = title,
		metadata = list(
			effect_measure = effect_measure,
			model = meta_result@model,
			k = k
		)
	)
}

#' Create Funnel Plot for Publication Bias Assessment
#'
#' Creates a funnel plot to visually assess publication bias in meta-analysis.
#' Studies are plotted by effect size vs. precision (1/SE).
#'
#' @param meta_result A MetaResult object from meta_analysis()
#' @param show_ci Logical. Show pseudo 95% CI region. Default: TRUE
#' @param show_egger Logical. Show Egger's regression line. Default: TRUE
#' @param title Character. Plot title. Default: "Funnel Plot"
#' @param xlab Character. X-axis label. Default: based on effect_measure
#' @param ylab Character. Y-axis label. Default: "Standard Error"
#' @param contour_levels Numeric vector. Contour p-value levels.
#'   Default: c(0.1, 0.05, 0.01)
#' @param base_size Numeric. Base font size. Default: 11
#'
#' @return A ClinicalPlot object containing the funnel plot
#' @export
create_funnel_plot <- function(
	meta_result,
	show_ci = TRUE,
	show_egger = TRUE,
	title = "Funnel Plot",
	xlab = NULL,
	ylab = "Standard Error",
	contour_levels = c(0.1, 0.05, 0.01),
	base_size = 11
) {
	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	effect_measure <- meta_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Get data from MetaResult metadata
	yi <- meta_result@metadata$yi
	sei <- meta_result@metadata$sei
	study_labels <- meta_result@metadata$study_labels

	if (is.null(yi) || is.null(sei)) {
		ph_abort("MetaResult must contain yi and sei in metadata")
	}

	# Pooled estimate (on log scale for ratios)
	if (is_ratio) {
		pooled <- log(meta_result@estimate)
	} else {
		pooled <- meta_result@estimate
	}

	# Set x-axis label
	if (is.null(xlab)) {
		xlab <- switch(
			effect_measure,
			"hr" = "Log Hazard Ratio",
			"or" = "Log Odds Ratio",
			"rr" = "Log Risk Ratio",
			"rd" = "Risk Difference",
			"md" = "Mean Difference",
			"smd" = "Standardized Mean Difference",
			"Effect Size"
		)
	}

	# Create plot data
	plot_data <- data.frame(
		study = study_labels,
		effect = yi,
		se = sei,
		precision = 1 / sei,
		stringsAsFactors = FALSE
	)

	# Perform Egger's test
	egger <- eggers_test(yi = yi, sei = sei)

	# Build funnel plot
	p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$effect, y = .data$se))

	# Add contour regions if requested
	if (show_ci) {
		# Create data for pseudo-confidence regions
		se_range <- c(0, max(sei) * 1.1)
		se_seq <- seq(0.001, se_range[2], length.out = 100)

		ci_data <- data.frame(
			se = se_seq,
			lower_95 = pooled - 1.96 * se_seq,
			upper_95 = pooled + 1.96 * se_seq
		)

		p <- p +
			ggplot2::geom_polygon(
				data = data.frame(
					x = c(pooled, ci_data$lower_95, rev(ci_data$upper_95)),
					y = c(0, ci_data$se, rev(ci_data$se))
				),
				ggplot2::aes(x = .data$x, y = .data$y),
				fill = "white",
				color = "gray70",
				linetype = "dashed",
				alpha = 0.5
			)
	}

	# Vertical line at pooled estimate
	p <- p +
		ggplot2::geom_vline(
			xintercept = pooled,
			linetype = "solid",
			color = "gray40"
		)

	# Egger's regression line
	if (show_egger && !is.na(egger$intercept)) {
		# Line: effect = intercept * se + slope (where slope approximates pooled)
		se_range <- range(plot_data$se)
		egger_line <- data.frame(
			se = se_range,
			effect = egger$slope + egger$intercept * se_range
		)
		p <- p +
			ggplot2::geom_line(
				data = egger_line,
				ggplot2::aes(x = .data$effect, y = .data$se),
				color = "red",
				linetype = "dotted",
				linewidth = 1
			)
	}

	# Study points
	p <- p + ggplot2::geom_point(size = 3, shape = 19)

	# Reverse y-axis (smaller SE at top)
	p <- p + ggplot2::scale_y_reverse()

	# Labels
	p <- p +
		ggplot2::labs(
			x = xlab,
			y = ylab,
			title = title
		)

	# Theme
	p <- p + .pharmhand_theme(base_size = base_size)

	# Add Egger's test result as caption
	if (show_egger) {
		egger_text <- sprintf(
			"Egger's test: intercept = %.2f, t = %.2f, p %s",
			egger$intercept,
			egger$t_value,
			format_pvalue(egger$p_value)
		)
		p <- p + ggplot2::labs(caption = egger_text)
	}

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = plot_data,
		type = "funnel",
		title = title,
		metadata = list(
			effect_measure = effect_measure,
			egger_test = egger,
			pooled_estimate = pooled
		)
	)
}


#' Egger's Test for Funnel Plot Asymmetry
#'
#' Performs Egger's linear regression test to assess funnel plot asymmetry,
#' which may indicate publication bias.
#'
#' @param yi Numeric vector of effect estimates
#' @param sei Numeric vector of standard errors
#' @param meta_result A MetaResult object (alternative to yi/sei)
#'
#' @return List with intercept, slope, standard error, t-value,
#'   p-value, and interpretation
#' @export
eggers_test <- function(
	yi = NULL,
	sei = NULL,
	meta_result = NULL
) {
	# Extract from MetaResult if provided
	if (!is.null(meta_result) && S7::S7_inherits(meta_result, MetaResult)) {
		yi <- meta_result@metadata$yi
		sei <- meta_result@metadata$sei
	}

	if (is.null(yi) || is.null(sei)) {
		ph_abort("Effect estimates (yi) and standard errors (sei) required")
	}

	k <- length(yi)

	if (k < 3) {
		return(list(
			intercept = NA_real_,
			slope = NA_real_,
			se = NA_real_,
			t_value = NA_real_,
			p_value = NA_real_,
			df = NA_integer_,
			interpretation = "Insufficient studies (< 3) for Egger's test"
		))
	}

	# Egger's regression: yi/sei ~ 1/sei
	# Equivalent to: yi = intercept * sei + slope + error
	# Under no bias, intercept should be 0

	zi <- yi / sei # standardized effect
	precision <- 1 / sei

	# Weighted least squares regression
	fit <- stats::lm(zi ~ precision)
	fit_summary <- summary(fit)
	coefs <- fit_summary$coefficients

	# Safe extraction with fallbacks
	if (!is.null(coefs) && nrow(coefs) >= 2) {
		intercept <- coefs[1, "Estimate"]
		slope <- coefs[2, "Estimate"]
		se_intercept <- coefs[1, "Std. Error"]
		t_value <- coefs[1, "t value"]
		p_value <- coefs[1, "Pr(>|t|)"]
	} else {
		# Fallback for unexpected structure
		intercept <- NA_real_
		slope <- NA_real_
		se_intercept <- NA_real_
		t_value <- NA_real_
		p_value <- NA_real_
	}

	# Interpretation
	if (is.na(p_value)) {
		interpretation <- "Unable to calculate Egger's test"
	} else if (p_value < 0.05) {
		interpretation <- paste0(
			"Significant asymmetry detected (p < 0.05), ",
			"suggesting potential publication bias"
		)
	} else if (p_value < 0.10) {
		interpretation <- paste0(
			"Marginal asymmetry (0.05 <= p < 0.10), ",
			"publication bias possible"
		)
	} else {
		interpretation <- "No significant asymmetry detected (p >= 0.10)"
	}

	list(
		intercept = intercept,
		slope = slope,
		se = se_intercept,
		t_value = t_value,
		p_value = p_value,
		df = k - 2,
		interpretation = interpretation
	)
}


#' Perform Indirect Comparison Using Bucher Method
#'
#' Calculates indirect treatment effect comparing A vs C through common
#' comparator B using the Bucher method (anchored indirect comparison).
#'
#' @param effect_ab Effect estimate for A vs B comparison
#' @param se_ab Standard error for A vs B
#' @param effect_bc Effect estimate for B vs C comparison
#' @param se_bc Standard error for B vs C
#' @param effect_measure Character. Effect type: "hr", "or", "rr",
#'   "rd", "md", "smd"
#' @param conf_level Numeric. Confidence level. Default: 0.95
#' @param label_a Character. Label for treatment A. Default: "A"
#' @param label_b Character. Label for treatment B (comparator). Default: "B"
#' @param label_c Character. Label for treatment C. Default: "C"
#'
#' @return ComparisonResult object with indirect effect estimate A vs C
#' @export
#'
#' @examples
#' \dontrun{
#' # Drug A vs Placebo: HR = 0.75, SE(log) = 0.12
#' # Drug B vs Placebo: HR = 0.85, SE(log) = 0.10
#' # What is Drug A vs Drug B indirectly?
#' result <- indirect_comparison(
#'   effect_ab = log(0.75),  # A vs Placebo
#'   se_ab = 0.12,
#'   effect_bc = log(0.85),  # B vs Placebo
#'   se_bc = 0.10,
#'   effect_measure = "hr"
#' )
#' # Result: A vs B (through Placebo)
#' }
indirect_comparison <- function(
	effect_ab,
	se_ab,
	effect_bc,
	se_bc,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	conf_level = 0.95,
	label_a = "A",
	label_b = "B",
	label_c = "C"
) {
	effect_measure <- match.arg(effect_measure)
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Validate inputs
	admiraldev::assert_numeric_vector(effect_ab, len = 1)
	admiraldev::assert_numeric_vector(se_ab, len = 1)
	admiraldev::assert_numeric_vector(effect_bc, len = 1)
	admiraldev::assert_numeric_vector(se_bc, len = 1)

	if (se_ab <= 0 || se_bc <= 0) {
		ph_abort("Standard errors must be positive")
	}

	# Bucher method: effect_AC = effect_AB - effect_BC
	# (on log scale for ratios, where B is common comparator)
	# If we have A vs B and B vs C, then A vs C = (A vs B) - (B vs C)
	# Note: This assumes consistent direction (all vs same reference direction)

	effect_ac <- effect_ab - effect_bc

	# Standard error (assuming independence)
	se_ac <- sqrt(se_ab^2 + se_bc^2)

	# Confidence interval
	alpha <- 1 - conf_level
	z <- stats::qnorm(1 - alpha / 2)
	ci_lower <- effect_ac - z * se_ac
	ci_upper <- effect_ac + z * se_ac

	# P-value (two-sided test against null)
	z_stat <- effect_ac / se_ac
	p_value <- 2 * stats::pnorm(-abs(z_stat))

	# Transform back from log scale for ratio measures
	if (is_ratio) {
		display_estimate <- exp(effect_ac)
		display_ci <- c(exp(ci_lower), exp(ci_upper))
	} else {
		display_estimate <- effect_ac
		display_ci <- c(ci_lower, ci_upper)
	}

	# Create ComparisonResult
	ComparisonResult(
		estimate = display_estimate,
		ci = display_ci,
		ci_level = conf_level,
		p_value = p_value,
		method = sprintf(
			"Bucher indirect comparison (%s vs %s via %s)",
			label_a,
			label_c,
			label_b
		),
		effect_measure = effect_measure,
		treatment = label_a,
		control = label_c,
		metadata = list(
			effect_ab = effect_ab,
			se_ab = se_ab,
			effect_bc = effect_bc,
			se_bc = se_bc,
			common_comparator = label_b,
			indirect_se = se_ac
		)
	)
}


#' Compare Direct and Indirect Evidence
#'
#' Compares direct evidence (from head-to-head trials) with indirect evidence
#' (from anchored comparison) to assess consistency.
#'
#' @param direct_result ComparisonResult or list with estimate and se
#'   for direct evidence
#' @param indirect_result ComparisonResult from indirect_comparison()
#' @param effect_measure Character. Effect type (if not in results)
#' @param conf_level Numeric. Confidence level. Default: 0.95
#'
#' @return A list with components:
#' \describe{
#' \item{direct_estimate}{Direct evidence effect estimate}
#' \item{indirect_estimate}{Indirect evidence effect estimate}
#' \item{pooled_estimate}{Inverse-variance weighted pooled estimate}
#' \item{pooled_ci}{CI for pooled estimate}
#' \item{inconsistency_p}{P-value for inconsistency test}
#' \item{effect_measure}{Effect measure used}
#' \item{is_consistent}{Logical; TRUE if p > 0.05}
#' }
#' @export
compare_direct_indirect <- function(
	direct_result,
	indirect_result,
	effect_measure = NULL,
	conf_level = 0.95
) {
	# Extract direct evidence
	if (S7::S7_inherits(direct_result, ComparisonResult)) {
		effect_measure <- direct_result@effect_measure
		is_ratio <- effect_measure %in% c("hr", "or", "rr")
		direct_est <- if (is_ratio) {
			log(direct_result@estimate)
		} else {
			direct_result@estimate
		}
		# Approximate SE from CI
		ci <- direct_result@ci
		if (is_ratio) {
			ci <- log(ci)
		}
		ci_level_direct <- direct_result@ci_level
		z_direct <- stats::qnorm((1 + ci_level_direct) / 2)
		direct_se <- (ci[2] - ci[1]) / (2 * z_direct)
	} else if (is.list(direct_result)) {
		direct_est <- direct_result$estimate
		direct_se <- direct_result$se
		if (is.null(effect_measure)) {
			effect_measure <- "md"
		}
		is_ratio <- effect_measure %in% c("hr", "or", "rr")
	} else {
		ph_abort(
			"direct_result must be ComparisonResult or list with estimate and se"
		)
	}

	# Extract indirect evidence
	if (S7::S7_inherits(indirect_result, ComparisonResult)) {
		indirect_est <- if (is_ratio) {
			log(indirect_result@estimate)
		} else {
			indirect_result@estimate
		}
		ci <- indirect_result@ci
		if (is_ratio) {
			ci <- log(ci)
		}
		ci_level_indirect <- indirect_result@ci_level
		z_indirect <- stats::qnorm((1 + ci_level_indirect) / 2)
		indirect_se <- (ci[2] - ci[1]) / (2 * z_indirect)
	} else {
		ph_abort("indirect_result must be ComparisonResult")
	}

	# Test for inconsistency (difference between direct and indirect)
	diff <- direct_est - indirect_est
	se_diff <- sqrt(direct_se^2 + indirect_se^2)
	z_inconsistency <- diff / se_diff
	p_inconsistency <- 2 * stats::pnorm(-abs(z_inconsistency))

	# Pool direct and indirect (inverse variance weighting)
	w_direct <- 1 / direct_se^2
	w_indirect <- 1 / indirect_se^2
	pooled_est <- (w_direct * direct_est + w_indirect * indirect_est) /
		(w_direct + w_indirect)
	pooled_se <- sqrt(1 / (w_direct + w_indirect))

	alpha <- 1 - conf_level
	z <- stats::qnorm(1 - alpha / 2)
	pooled_ci <- c(pooled_est - z * pooled_se, pooled_est + z * pooled_se)

	# Transform for display
	if (is_ratio) {
		display_direct <- exp(direct_est)
		display_indirect <- exp(indirect_est)
		display_pooled <- exp(pooled_est)
		display_pooled_ci <- exp(pooled_ci)
	} else {
		display_direct <- direct_est
		display_indirect <- indirect_est
		display_pooled <- pooled_est
		display_pooled_ci <- pooled_ci
	}

	# Interpretation
	if (p_inconsistency < 0.05) {
		interpretation <- paste0(
			"Significant inconsistency between direct and ",
			"indirect evidence (p < 0.05)"
		)
		consistency <- "Inconsistent"
	} else if (p_inconsistency < 0.10) {
		interpretation <- "Marginal inconsistency (0.05 <= p < 0.10)"
		consistency <- "Marginally consistent"
	} else {
		interpretation <- "No significant inconsistency detected (p >= 0.10)"
		consistency <- "Consistent"
	}

	list(
		direct_estimate = display_direct,
		indirect_estimate = display_indirect,
		difference = if (is_ratio) exp(diff) else diff,
		inconsistency_test = list(
			z = z_inconsistency,
			p_value = p_inconsistency,
			interpretation = interpretation
		),
		consistency = consistency,
		pooled = list(
			estimate = display_pooled,
			ci = display_pooled_ci,
			se = pooled_se
		),
		effect_measure = effect_measure
	)
}


#' Assess Transitivity for Indirect Comparisons
#'
#' Evaluates the transitivity assumption by comparing population characteristics
#' across studies in a network. Transitivity requires that studies are similar
#' enough that indirect comparisons are valid.
#'
#' @param study_characteristics Data frame with study-level
#'   characteristics. Must include 'study_id' and 'treatment'
#'   columns, plus characteristics to compare.
#' @param char_vars Character vector. Names of characteristic
#'   variables to assess.
#' @param treatment_var Character. Name of treatment variable.
#'   Default: "treatment"
#' @param continuous_vars Character vector. Which char_vars are
#'   continuous (vs categorical)
#' @param threshold_smd Numeric. SMD threshold for imbalance.
#'   Default: 0.1
#'
#' @return A list with components:
#' \describe{
#' \item{summaries}{Summary statistics by treatment}
#' \item{comparison_tables}{Pairwise comparison tables}
#' \item{imbalance_scores}{SMDs for continuous variables}
#' \item{overall_assessment}{Text summary of concerns}
#' \item{n_treatments}{Number of unique treatments}
#' \item{n_characteristics}{Number of characteristics assessed}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' chars <- data.frame(
#'   study_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
#'   treatment = c("A", "B", "B", "C", "A", "C"),
#'   mean_age = c(55, 55, 58, 58, 52, 52),
#'   pct_male = c(60, 60, 65, 65, 55, 55),
#'   disease_stage = c("II", "II", "III", "III", "II", "II")
#' )
#'
#' result <- assess_transitivity(
#'   study_characteristics = chars,
#'   char_vars = c("mean_age", "pct_male", "disease_stage"),
#'   continuous_vars = c("mean_age", "pct_male")
#' )
#' }
assess_transitivity <- function(
	study_characteristics,
	char_vars,
	treatment_var = "treatment",
	continuous_vars = NULL,
	threshold_smd = 0.1
) {
	admiraldev::assert_data_frame(study_characteristics)

	if (!"study_id" %in% names(study_characteristics)) {
		ph_abort("study_characteristics must contain 'study_id' column")
	}
	if (!treatment_var %in% names(study_characteristics)) {
		ph_abort(sprintf("Treatment variable '%s' not found", treatment_var))
	}

	missing_vars <- setdiff(char_vars, names(study_characteristics))
	if (length(missing_vars) > 0) {
		ph_abort(sprintf(
			"Characteristic variables not found: %s",
			paste(missing_vars, collapse = ", ")
		))
	}

	if (is.null(continuous_vars)) {
		continuous_vars <- character(0)
	}

	# Get unique treatments and comparisons
	treatments <- unique(study_characteristics[[treatment_var]])
	n_treatments <- length(treatments)

	# Get study-level data (one row per study)
	study_data <- study_characteristics |>
		dplyr::group_by(.data$study_id) |>
		dplyr::slice(1) |>
		dplyr::ungroup()

	# Get which treatments are compared in each study
	study_treatments <- study_characteristics |>
		dplyr::group_by(.data$study_id) |>
		dplyr::summarise(
			treatments = list(unique(.data[[treatment_var]])),
			.groups = "drop"
		)

	# Identify unique comparisons
	comparisons <- utils::combn(treatments, 2, simplify = FALSE)

	# For each characteristic, compare across comparison types
	char_summaries <- list()

	for (var in char_vars) {
		is_continuous <- var %in% continuous_vars

		# Summarize by comparison type
		comparison_stats <- lapply(comparisons, function(comp) {
			# Find studies with this comparison
			studies_with_comp <- study_treatments$study_id[
				vapply(
					study_treatments$treatments,
					function(t) {
						all(comp %in% t)
					},
					logical(1)
				)
			]

			if (length(studies_with_comp) == 0) {
				return(NULL)
			}

			data_subset <- study_data[study_data$study_id %in% studies_with_comp, ]
			values <- data_subset[[var]]

			if (is_continuous) {
				list(
					comparison = paste(comp, collapse = " vs "),
					n_studies = length(studies_with_comp),
					mean = mean(values, na.rm = TRUE),
					sd = stats::sd(values, na.rm = TRUE),
					median = stats::median(values, na.rm = TRUE),
					range = paste(range(values, na.rm = TRUE), collapse = "-")
				)
			} else {
				tab <- table(values)
				list(
					comparison = paste(comp, collapse = " vs "),
					n_studies = length(studies_with_comp),
					categories = paste(names(tab), collapse = ", "),
					distribution = paste(
						sprintf("%s: %d", names(tab), tab),
						collapse = "; "
					)
				)
			}
		})

		comparison_stats <- comparison_stats[
			!vapply(comparison_stats, is.null, logical(1))
		]

		if (length(comparison_stats) > 0) {
			char_summaries[[var]] <- do.call(
				rbind,
				lapply(comparison_stats, as.data.frame)
			)
		}
	}

	# Calculate imbalance measures for continuous variables
	imbalance_results <- list()

	for (var in intersect(char_vars, continuous_vars)) {
		if (!is.null(char_summaries[[var]]) && nrow(char_summaries[[var]]) > 1) {
			means <- char_summaries[[var]]$mean
			sds <- char_summaries[[var]]$sd

			# Calculate pairwise SMDs between comparison types
			n_comp <- length(means)
			if (n_comp >= 2) {
				pooled_sd <- sqrt(mean(sds^2, na.rm = TRUE))
				if (!is.na(pooled_sd) && is.finite(pooled_sd) && pooled_sd > 0) {
					max_diff <- max(means, na.rm = TRUE) - min(means, na.rm = TRUE)
					smd <- max_diff / pooled_sd
				} else {
					smd <- 0
					max_diff <- 0
				}

				imbalance_results[[var]] <- list(
					smd = smd,
					imbalanced = smd > threshold_smd,
					max_difference = max_diff
				)
			}
		}
	}

	# Overall transitivity assessment
	n_imbalanced <- sum(vapply(
		imbalance_results,
		function(x) {
			if (is.null(x$imbalanced)) FALSE else x$imbalanced
		},
		logical(1)
	))
	n_assessed <- length(imbalance_results)

	if (n_assessed == 0) {
		overall <- "Unable to assess (no continuous variables)"
	} else if (n_imbalanced == 0) {
		overall <- "Transitivity assumption appears reasonable"
	} else if (n_imbalanced <= n_assessed / 3) {
		overall <- "Minor concerns about transitivity"
	} else {
		overall <- "Substantial concerns about transitivity"
	}

	list(
		summaries = char_summaries,
		imbalance = imbalance_results,
		n_treatments = n_treatments,
		treatments = treatments,
		n_comparisons = length(comparisons),
		overall_assessment = overall,
		threshold = threshold_smd,
		interpretation = sprintf(
			"Assessed %d characteristics across %d treatment comparisons. %s",
			length(char_vars),
			length(comparisons),
			overall
		)
	)
}


#' Perform Network Meta-Analysis
#'
#' Conducts network meta-analysis (NMA) to compare multiple treatments
#' simultaneously using direct and indirect evidence.
#'
#' @param data Data frame with study-level data. Required columns:
#'   study (study identifier), treat1 (treatment 1), treat2 (treatment 2),
#'   effect (effect estimate), se (standard error)
#' @param study_var Character. Study identifier column. Default: "study"
#' @param treat1_var Character. Treatment 1 column. Default: "treat1"
#' @param treat2_var Character. Treatment 2 column. Default: "treat2"
#' @param effect_var Character. Effect estimate column. Default: "effect"
#' @param se_var Character. Standard error column. Default: "se"
#' @param reference Character. Reference treatment.
#'   Default: first alphabetically
#' @param effect_measure Character. Effect type: "hr", "or", "rr",
#'   "rd", "md", "smd"
#' @param model Character. "fixed" or "random". Default: "random"
#' @param method Character. NMA method: "bucher" (simple),
#'   "graph" (if netmeta available)
#' @param conf_level Numeric. Confidence level. Default: 0.95
#'
#' @return List with relative effects, rankings, and network structure
#' @export
#'
#' @examples
#' \dontrun{
#' nma_data <- data.frame(
#'   study = c("S1", "S2", "S3", "S4"),
#'   treat1 = c("A", "B", "A", "B"),
#'   treat2 = c("B", "C", "C", "D"),
#'   effect = log(c(0.75, 0.90, 0.80, 0.85)),
#'   se = c(0.12, 0.15, 0.18, 0.14)
#' )
#'
#' result <- network_meta(nma_data, effect_measure = "hr")
#' }
network_meta <- function(
	data,
	study_var = "study",
	treat1_var = "treat1",
	treat2_var = "treat2",
	effect_var = "effect",
	se_var = "se",
	reference = NULL,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	model = c("random", "fixed"),
	method = c("bucher", "graph"),
	conf_level = 0.95
) {
	effect_measure <- match.arg(effect_measure)
	model <- match.arg(model)
	method <- match.arg(method)

	admiraldev::assert_data_frame(data)

	# Rename columns for internal use
	df <- data.frame(
		study = data[[study_var]],
		treat1 = data[[treat1_var]],
		treat2 = data[[treat2_var]],
		effect = data[[effect_var]],
		se = data[[se_var]],
		stringsAsFactors = FALSE
	)

	# Get all treatments
	treatments <- sort(unique(c(df$treat1, df$treat2)))
	n_treatments <- length(treatments)

	if (is.null(reference)) {
		reference <- treatments[1]
	}

	if (!reference %in% treatments) {
		ph_abort(sprintf("Reference treatment '%s' not in network", reference))
	}

	# Build network structure
	edges <- unique(df[, c("treat1", "treat2")])
	edges$n_studies <- sapply(seq_len(nrow(edges)), function(i) {
		sum(df$treat1 == edges$treat1[i] & df$treat2 == edges$treat2[i])
	})

	# Check connectivity (simplified - just check if all treatments appear)
	connected_treatments <- unique(c(edges$treat1, edges$treat2))
	if (length(connected_treatments) < n_treatments) {
		ph_warn("Network may not be fully connected")
	}

	# Simple NMA using Bucher method for each comparison vs reference
	# This is a simplified approach when netmeta is not available

	# First, get direct comparisons vs reference
	direct_vs_ref <- df[df$treat1 == reference | df$treat2 == reference, ]

	# Meta-analyze each direct comparison
	direct_results <- list()
	for (trt in setdiff(treatments, reference)) {
		# Studies with this comparison
		studies <- df[
			(df$treat1 == reference & df$treat2 == trt) |
				(df$treat1 == trt & df$treat2 == reference),
		]

		if (nrow(studies) > 0) {
			# Ensure direction is ref -> trt
			effects <- ifelse(
				studies$treat1 == reference,
				studies$effect,
				-studies$effect
			)
			ses <- studies$se

			if (length(effects) == 1) {
				est <- effects
				se <- ses
			} else {
				# Simple inverse-variance pooling
				wi <- 1 / ses^2
				est <- sum(wi * effects) / sum(wi)
				se <- sqrt(1 / sum(wi))
			}

			direct_results[[trt]] <- list(
				estimate = est,
				se = se,
				n_studies = nrow(studies),
				evidence = "direct"
			)
		}
	}

	# For treatments without direct comparison to reference, use indirect
	indirect_results <- list()
	for (trt in setdiff(treatments, c(reference, names(direct_results)))) {
		# Try to find indirect path through another treatment
		for (bridge in names(direct_results)) {
			# Check if trt vs bridge exists
			bridge_studies <- df[
				(df$treat1 == bridge & df$treat2 == trt) |
					(df$treat1 == trt & df$treat2 == bridge),
			]

			if (nrow(bridge_studies) > 0) {
				# Effect of bridge vs trt
				effects <- ifelse(
					bridge_studies$treat1 == bridge,
					bridge_studies$effect,
					-bridge_studies$effect
				)
				ses <- bridge_studies$se

				if (length(effects) > 1) {
					wi <- 1 / ses^2
					bridge_est <- sum(wi * effects) / sum(wi)
					bridge_se <- sqrt(1 / sum(wi))
				} else {
					bridge_est <- effects
					bridge_se <- ses
				}

				# Indirect: ref vs trt = (ref vs bridge) + (bridge vs trt)
				ref_bridge <- direct_results[[bridge]]
				ind_est <- ref_bridge$estimate + bridge_est
				ind_se <- sqrt(ref_bridge$se^2 + bridge_se^2)

				indirect_results[[trt]] <- list(
					estimate = ind_est,
					se = ind_se,
					n_studies = ref_bridge$n_studies + nrow(bridge_studies),
					evidence = "indirect",
					via = bridge
				)
				break
			}
		}
	}

	# Combine all results
	all_results <- c(direct_results, indirect_results)

	# Create comparison table
	is_ratio <- effect_measure %in% c("hr", "or", "rr")
	alpha <- 1 - conf_level
	z <- stats::qnorm(1 - alpha / 2)

	comparison_table <- data.frame(
		treatment = names(all_results),
		vs = reference,
		estimate = sapply(all_results, function(x) {
			if (is_ratio) exp(x$estimate) else x$estimate
		}),
		ci_lower = sapply(all_results, function(x) {
			val <- x$estimate - z * x$se
			if (is_ratio) exp(val) else val
		}),
		ci_upper = sapply(all_results, function(x) {
			val <- x$estimate + z * x$se
			if (is_ratio) exp(val) else val
		}),
		se = sapply(all_results, function(x) x$se),
		n_studies = sapply(all_results, function(x) x$n_studies),
		evidence = sapply(all_results, function(x) x$evidence),
		stringsAsFactors = FALSE
	)

	# Simple ranking based on point estimates
	if (is_ratio) {
		# Lower is better for HR/OR/RR
		comparison_table$rank <- rank(comparison_table$estimate)
	} else {
		# Direction depends on outcome (assume lower is better)
		comparison_table$rank <- rank(comparison_table$estimate)
	}

	# Add reference
	ref_row <- data.frame(
		treatment = reference,
		vs = reference,
		estimate = if (is_ratio) 1 else 0,
		ci_lower = if (is_ratio) 1 else 0,
		ci_upper = if (is_ratio) 1 else 0,
		se = 0,
		n_studies = NA,
		evidence = "reference",
		rank = NA,
		stringsAsFactors = FALSE
	)
	comparison_table <- rbind(ref_row, comparison_table)

	list(
		comparisons = comparison_table,
		network = list(
			treatments = treatments,
			n_treatments = n_treatments,
			edges = edges,
			reference = reference
		),
		model = model,
		effect_measure = effect_measure,
		method = "bucher_chain",
		n_studies = nrow(df)
	)
}


#' Trim-and-Fill Method for Publication Bias
#'
#' Performs the Duval & Tweedie trim-and-fill method to estimate the number
#' of missing studies and adjust the pooled effect for publication bias.
#'
#' @param meta_result A MetaResult object from meta_analysis()
#' @param side Character. Side to impute: "left", "right", or "auto".
#'   Default: "auto"
#' @param estimator Character. Method to estimate missing studies:
#'   "L0", "R0", "Q0". Default: "L0"
#' @param maxiter Integer. Maximum iterations. Default: 100
#'
#' @return A list with components:
#' \describe{
#' \item{original}{Original MetaResult object}
#' \item{adjusted}{Adjusted MetaResult with imputed studies}
#' \item{n_missing}{Estimated number of missing studies}
#' \item{side}{Side where studies were imputed}
#' \item{imputed_studies}{Data frame with imputed effects}
#' \item{estimator}{Estimator method used}
#' \item{summary}{Text summary of the adjustment}
#' }
#'
#' @examples
#' \dontrun{
#' # Run meta-analysis
#' meta_res <- meta_analysis(
#'   yi = c(0.2, 0.4, 0.3, 0.5, 0.6),
#'   sei = c(0.1, 0.12, 0.08, 0.15, 0.11),
#'   study_labels = paste("Study", 1:5)
#' )
#'
#' # Apply trim-and-fill for publication bias
#' adjusted <- trim_and_fill(meta_res)
#'
#' # Check number of imputed studies
#' adjusted$n_missing
#' }
#' @export
trim_and_fill <- function(
	meta_result,
	side = c("auto", "left", "right"),
	estimator = c("L0", "R0", "Q0"),
	maxiter = 100
) {
	side <- match.arg(side)
	estimator <- match.arg(estimator)

	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	# Extract data
	yi <- meta_result@metadata$yi
	sei <- meta_result@metadata$sei
	effect_measure <- meta_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	if (is.null(yi) || is.null(sei)) {
		ph_abort("MetaResult must contain yi and sei in metadata")
	}

	k <- length(yi)

	# Initial pooled estimate
	wi <- 1 / (sei^2)
	theta0 <- sum(wi * yi) / sum(wi)

	# Determine side automatically if needed
	if (side == "auto") {
		# Check asymmetry using Egger's test
		egger <- eggers_test(yi = yi, sei = sei)
		if (!is.na(egger$intercept)) {
			side <- if (egger$intercept > 0) "right" else "left"
		} else {
			side <- "left" # default
		}
	}

	# Trim-and-fill algorithm
	# 1. Rank studies by their distance from pooled estimate
	# 2. Iteratively trim and estimate missing studies

	yi_work <- yi
	sei_work <- sei
	k0 <- 0 # number of imputed studies

	for (iter in seq_len(maxiter)) {
		# Current pooled estimate
		wi_work <- 1 / (sei_work^2)
		theta <- sum(wi_work * yi_work) / sum(wi_work)

		# Rank studies by deviation from theta
		di <- yi_work - theta

		if (side == "right") {
			# Looking for missing studies on left (negative effects)
			# Rank by positive deviation
			ranks <- rank(di)
			extreme_positive <- which(di > 0)
		} else {
			# Looking for missing studies on right (positive effects)
			# Rank by negative deviation
			ranks <- rank(-di)
			extreme_positive <- which(di < 0)
		}

		# Estimate number of missing studies using L0 estimator
		n <- length(yi_work)
		if (estimator == "L0") {
			# L0: based on rank correlation
			abs_ranks <- rank(abs(di))
			Tn <- sum((abs_ranks - (n + 1) / 2) * sign(di))
			k0_new <- max(0, round((4 * Tn + n * (n + 1)) / (2 * n + 1) - n))
		} else if (estimator == "R0") {
			# R0: simpler rank-based estimator
			if (side == "right") {
				k0_new <- max(
					0,
					2 * sum(ranks > (n + 1) / 2 & di > 0) - length(extreme_positive)
				)
			} else {
				k0_new <- max(
					0,
					2 * sum(ranks > (n + 1) / 2 & di < 0) - length(extreme_positive)
				)
			}
		} else {
			# Q0: based on Q statistic
			Q <- sum(wi_work * (yi_work - theta)^2)
			k0_new <- max(0, round(Q / (n - 1) - 1))
		}

		k0_new <- min(k0_new, n - 1) # Can't impute more than n-1 studies

		if (k0_new == k0 && iter > 1) {
			break
		}
		k0 <- k0_new

		if (k0 == 0) {
			break
		}

		# Trim the most extreme studies on one side
		if (side == "right") {
			trim_idx <- order(di, decreasing = TRUE)[seq_len(k0)]
		} else {
			trim_idx <- order(di, decreasing = FALSE)[seq_len(k0)]
		}

		# Trimmed data
		yi_trim <- yi_work[-trim_idx]
		sei_trim <- sei_work[-trim_idx]

		# Recalculate theta with trimmed data
		wi_trim <- 1 / (sei_trim^2)
		theta <- sum(wi_trim * yi_trim) / sum(wi_trim)

		# Now fill: impute mirror images of trimmed studies
		yi_filled <- 2 * theta - yi_work[trim_idx]
		sei_filled <- sei_work[trim_idx]

		# Combine original + filled
		yi_work <- c(yi, yi_filled)
		sei_work <- c(sei, sei_filled)
	}

	# Final adjusted analysis
	if (k0 > 0) {
		adjusted_result <- meta_analysis(
			yi = yi_work,
			sei = sei_work,
			effect_measure = effect_measure,
			model = meta_result@model,
			method = "DL" # Use DL for simplicity
		)

		# Imputed study data
		imputed_yi <- yi_work[(k + 1):length(yi_work)]
		imputed_sei <- sei_work[(k + 1):length(sei_work)]

		if (is_ratio) {
			imputed_effects <- exp(imputed_yi)
		} else {
			imputed_effects <- imputed_yi
		}
	} else {
		adjusted_result <- meta_result
		imputed_effects <- numeric(0)
		imputed_yi <- numeric(0)
		imputed_sei <- numeric(0)
	}

	# Prepare results
	original_est <- meta_result@estimate
	adjusted_est <- adjusted_result@estimate

	list(
		original = list(
			estimate = original_est,
			ci = meta_result@ci,
			k = k
		),
		adjusted = list(
			estimate = adjusted_est,
			ci = adjusted_result@ci,
			k = k + k0
		),
		n_imputed = k0,
		imputed_studies = if (k0 > 0) {
			data.frame(
				effect = imputed_effects,
				se = imputed_sei,
				stringsAsFactors = FALSE
			)
		} else {
			data.frame(effect = numeric(0), se = numeric(0))
		},
		side = side,
		estimator = estimator,
		effect_measure = effect_measure,
		interpretation = if (k0 > 0) {
			sprintf(
				paste0(
					"Estimated %d missing studies on %s side. ",
					"Original estimate: %.3f, Adjusted: %.3f"
				),
				k0,
				side,
				original_est,
				adjusted_est
			)
		} else {
			"No missing studies detected"
		}
	)
}

#' Visualize Network Geometry
#'
#' Creates a network graph visualization showing the structure of treatment
#' comparisons in a network meta-analysis.
#'
#' @param nma_result Result from network_meta() or data with network structure
#' @param node_size Character. Size nodes by "equal", "n_studies", or
#'   "n_patients". Default: "equal"
#' @param edge_width Character. Width edges by "equal" or "n_studies".
#'   Default: "n_studies"
#' @param show_labels Logical. Show edge labels with study counts. Default: TRUE
#' @param highlight_ref Logical. Highlight reference treatment. Default: TRUE
#' @param title Character. Plot title. Default: "Network Geometry"
#' @param layout Character. Layout algorithm: "circle", "star",
#'   "auto". Default: "circle"
#' @param palette Character vector. Node colors. Default: NULL
#' @param base_size Numeric. Base font size. Default: 11
#'
#' @return A ClinicalPlot object containing the network graph
#' @export
#'
#' @examples
#' \dontrun{
#' # Network meta-analysis data
#' nma_data <- data.frame(
#'   study = c("S1", "S1", "S2", "S2", "S3", "S3"),
#'   treatment = c("A", "B", "A", "C", "B", "C"),
#'   responders = c(20, 25, 18, 22, 30, 28),
#'   n = c(100, 100, 90, 95, 110, 105)
#' )
#'
#' # Run NMA
#' nma_result <- network_meta(nma_data, reference = "A")
#'
#' # Create network geometry plot
#' plot <- create_network_plot(nma_result, title = "Treatment Network")
#' }
create_network_plot <- function(
	nma_result,
	node_size = c("equal", "n_studies", "n_patients"),
	edge_width = c("n_studies", "equal"),
	show_labels = TRUE,
	highlight_ref = TRUE,
	title = "Network Geometry",
	layout = c("circle", "star", "auto"),
	palette = NULL,
	base_size = 11
) {
	node_size <- match.arg(node_size)
	edge_width <- match.arg(edge_width)
	layout <- match.arg(layout)

	# Extract network structure
	if (is.list(nma_result) && "network" %in% names(nma_result)) {
		network <- nma_result$network
		treatments <- network$treatments
		edges <- network$edges
		reference <- network$reference
	} else {
		ph_abort("nma_result must contain network structure")
	}

	n_nodes <- length(treatments)

	# Calculate node positions based on layout
	if (layout == "circle") {
		angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[1:n_nodes]
		node_x <- cos(angles)
		node_y <- sin(angles)
	} else if (layout == "star") {
		# Reference in center, others around
		ref_idx <- which(treatments == reference)
		other_idx <- setdiff(seq_along(treatments), ref_idx)
		n_other <- length(other_idx)

		node_x <- numeric(n_nodes)
		node_y <- numeric(n_nodes)
		node_x[ref_idx] <- 0
		node_y[ref_idx] <- 0

		angles <- seq(0, 2 * pi, length.out = n_other + 1)[1:n_other]
		node_x[other_idx] <- cos(angles)
		node_y[other_idx] <- sin(angles)
	} else {
		# Auto: try to minimize edge crossings (simplified)
		angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[1:n_nodes]
		node_x <- cos(angles)
		node_y <- sin(angles)
	}

	# Node data
	node_data <- data.frame(
		treatment = treatments,
		x = node_x,
		y = node_y,
		is_reference = treatments == reference,
		stringsAsFactors = FALSE
	)

	# Calculate node sizes
	if (node_size == "n_studies") {
		# Count studies involving each treatment
		node_data$n_studies <- sapply(treatments, function(t) {
			sum(edges$treat1 == t | edges$treat2 == t)
		})
		node_data$size <- 3 + 5 * (node_data$n_studies / max(node_data$n_studies))
	} else {
		node_data$size <- 5
	}

	# Edge data
	edge_data <- data.frame(
		treat1 = edges$treat1,
		treat2 = edges$treat2,
		n_studies = edges$n_studies,
		stringsAsFactors = FALSE
	)

	# Add coordinates
	edge_data$x1 <- node_data$x[match(edge_data$treat1, node_data$treatment)]
	edge_data$y1 <- node_data$y[match(edge_data$treat1, node_data$treatment)]
	edge_data$x2 <- node_data$x[match(edge_data$treat2, node_data$treatment)]
	edge_data$y2 <- node_data$y[match(edge_data$treat2, node_data$treatment)]

	# Edge midpoints for labels
	edge_data$xmid <- (edge_data$x1 + edge_data$x2) / 2
	edge_data$ymid <- (edge_data$y1 + edge_data$y2) / 2

	# Edge widths
	if (edge_width == "n_studies") {
		edge_data$width <- 0.5 +
			2 * (edge_data$n_studies / max(edge_data$n_studies))
	} else {
		edge_data$width <- 1
	}

	# Build plot
	p <- ggplot2::ggplot()

	# Draw edges
	p <- p +
		ggplot2::geom_segment(
			data = edge_data,
			ggplot2::aes(
				x = .data$x1,
				y = .data$y1,
				xend = .data$x2,
				yend = .data$y2,
				linewidth = .data$width
			),
			color = "gray60"
		)

	# Edge labels (number of studies)
	if (show_labels) {
		p <- p +
			ggplot2::geom_label(
				data = edge_data,
				ggplot2::aes(
					x = .data$xmid,
					y = .data$ymid,
					label = .data$n_studies
				),
				size = base_size / 4,
				fill = "white",
				label.padding = ggplot2::unit(0.15, "lines")
			)
	}

	# Draw nodes
	if (highlight_ref) {
		# Non-reference nodes
		p <- p +
			ggplot2::geom_point(
				data = node_data[!node_data$is_reference, ],
				ggplot2::aes(x = .data$x, y = .data$y, size = .data$size),
				color = "steelblue",
				fill = "steelblue",
				shape = 21
			)

		# Reference node (highlighted)
		p <- p +
			ggplot2::geom_point(
				data = node_data[node_data$is_reference, ],
				ggplot2::aes(x = .data$x, y = .data$y, size = .data$size),
				color = "darkred",
				fill = "darkred",
				shape = 21
			)
	} else {
		p <- p +
			ggplot2::geom_point(
				data = node_data,
				ggplot2::aes(x = .data$x, y = .data$y, size = .data$size),
				color = "steelblue",
				fill = "steelblue",
				shape = 21
			)
	}

	# Node labels (treatment names)
	# Offset labels based on position
	node_data$label_x <- node_data$x * 1.15
	node_data$label_y <- node_data$y * 1.15

	p <- p +
		ggplot2::geom_text(
			data = node_data,
			ggplot2::aes(
				x = .data$label_x,
				y = .data$label_y,
				label = .data$treatment
			),
			size = base_size / 3,
			fontface = "bold"
		)

	# Remove axes and apply theme
	p <- p +
		ggplot2::coord_fixed() +
		ggplot2::scale_size_identity() +
		ggplot2::scale_linewidth_identity() +
		ggplot2::theme_void(base_size = base_size) +
		ggplot2::theme(
			plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
			legend.position = "none"
		) +
		ggplot2::ggtitle(title)

	# Return as ClinicalPlot
	ClinicalPlot(
		plot = p,
		data = list(nodes = node_data, edges = edge_data),
		type = "network_geometry",
		title = title,
		metadata = list(
			n_treatments = n_nodes,
			n_edges = nrow(edge_data),
			reference = reference
		)
	)
}


#' Test for Inconsistency Using Node-Splitting
#'
#' Separates direct and indirect evidence for each comparison and tests
#' for inconsistency between them.
#'
#' @param nma_result Result from network_meta()
#' @param data Original NMA data frame
#' @param conf_level Numeric. Confidence level. Default: 0.95
#'
#' @return Data frame with direct, indirect, and inconsistency test results
#' @note This is a simplified implementation. Full node-splitting requires
#'   re-running the network meta-analysis excluding direct evidence for each
#'   comparison, which is computationally intensive. Consider using specialized
#'   NMA packages (e.g., gemtc, netmeta) for rigorous inconsistency assessment.
#' @export
node_splitting <- function(
	nma_result,
	data = NULL,
	conf_level = 0.95
) {
	if (!is.list(nma_result) || !"network" %in% names(nma_result)) {
		ph_abort("nma_result must be from network_meta()")
	}

	network <- nma_result$network
	edges <- network$edges
	reference <- network$reference
	effect_measure <- nma_result$effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# For each comparison with both direct and indirect evidence
	results <- list()

	for (i in seq_len(nrow(edges))) {
		t1 <- edges$treat1[i]
		t2 <- edges$treat2[i]
		n_direct <- edges$n_studies[i]

		# Skip if only 1 study (can't split)
		if (n_direct < 1) {
			next
		}

		# Get comparison info from nma_result
		comp_row <- nma_result$comparisons[
			nma_result$comparisons$treatment == t2 &
				nma_result$comparisons$vs == reference,
		]

		if (nrow(comp_row) == 0) {
			next
		}

		# For proper node-splitting, we'd need to re-run the analysis
		# excluding direct evidence. Here we provide a simplified version.

		# Direct evidence (from the edge)
		direct_est <- comp_row$estimate[1]
		direct_se <- comp_row$se[1]

		# Check if there's potential indirect path
		has_indirect <- comp_row$evidence[1] != "direct" ||
			any(edges$n_studies[edges$treat1 != t1 | edges$treat2 != t2] > 0)

		if (has_indirect) {
			# Simplified: flag for manual review
			results[[paste(t1, t2, sep = "_vs_")]] <- data.frame(
				comparison = paste(t1, "vs", t2),
				direct_estimate = direct_est,
				direct_se = direct_se,
				n_direct = n_direct,
				indirect_available = has_indirect,
				inconsistency_p = NA_real_, # Would need full implementation
				stringsAsFactors = FALSE
			)
		}
	}

	if (length(results) == 0) {
		return(data.frame(
			comparison = character(0),
			direct_estimate = numeric(0),
			note = "No comparisons with both direct and indirect evidence"
		))
	}

	result_df <- do.call(rbind, results)
	rownames(result_df) <- NULL

	list(
		results = result_df,
		effect_measure = effect_measure,
		note = paste0(
			"Full node-splitting requires re-analysis excluding ",
			"direct evidence. Results shown are simplified."
		)
	)
}


#' Calculate Treatment Rankings (P-scores/SUCRA)
#'
#' Calculates ranking probabilities and SUCRA (Surface Under Cumulative
#' Ranking curve) or P-scores for treatments in network meta-analysis.
#'
#' @param nma_result Result from network_meta()
#' @param lower_better Logical. Is lower estimate better?
#'   Default: TRUE for ratios
#' @param n_sim Integer. Number of simulations for ranking. Default: 1000
#'
#' @return List with rankings, SUCRA/P-scores, and rankogram data
#' @export
calculate_sucra <- function(
	nma_result,
	lower_better = NULL,
	n_sim = 1000
) {
	if (!is.list(nma_result) || !"comparisons" %in% names(nma_result)) {
		ph_abort("nma_result must be from network_meta()")
	}

	comparisons <- nma_result$comparisons
	effect_measure <- nma_result$effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Default direction
	if (is.null(lower_better)) {
		lower_better <- is_ratio # Lower HR/OR/RR is typically better
	}

	treatments <- comparisons$treatment
	n_treat <- length(treatments)
	estimates <- comparisons$estimate
	ses <- comparisons$se

	# Handle reference (no SE)
	ref_idx <- which(ses == 0 | is.na(ses))
	if (length(ref_idx) > 0) {
		ses[ref_idx] <- min(ses[ses > 0], na.rm = TRUE) / 2
	}

	# Simulate rankings
	rank_matrix <- matrix(0, nrow = n_sim, ncol = n_treat)
	colnames(rank_matrix) <- treatments

	set.seed(42) # For reproducibility
	for (sim in seq_len(n_sim)) {
		# Sample from normal distribution
		sampled <- stats::rnorm(n_treat, mean = estimates, sd = ses)

		# Rank (1 = best)
		if (lower_better) {
			ranks <- rank(sampled)
		} else {
			ranks <- rank(-sampled)
		}

		rank_matrix[sim, ] <- ranks
	}

	# Calculate ranking probabilities
	rank_probs <- matrix(0, nrow = n_treat, ncol = n_treat)
	rownames(rank_probs) <- treatments
	colnames(rank_probs) <- paste0("Rank_", seq_len(n_treat))

	for (i in seq_len(n_treat)) {
		for (r in seq_len(n_treat)) {
			rank_probs[i, r] <- mean(rank_matrix[, i] == r)
		}
	}

	# Calculate SUCRA (or P-score for frequentist)
	# SUCRA = mean of cumulative ranking probabilities
	sucra <- numeric(n_treat)
	names(sucra) <- treatments

	for (i in seq_len(n_treat)) {
		cum_probs <- cumsum(rank_probs[i, 1:(n_treat - 1)])
		sucra[i] <- mean(cum_probs)
	}

	# Mean rank
	mean_rank <- colMeans(rank_matrix)

	# Create ranking summary
	ranking_summary <- data.frame(
		treatment = treatments,
		mean_rank = mean_rank,
		sucra = sucra * 100, # as percentage
		prob_best = rank_probs[, 1],
		prob_worst = rank_probs[, n_treat],
		stringsAsFactors = FALSE
	)
	ranking_summary <- ranking_summary[
		order(ranking_summary$sucra, decreasing = TRUE),
	]
	ranking_summary$final_rank <- seq_len(n_treat)

	list(
		ranking = ranking_summary,
		rank_probabilities = rank_probs,
		sucra = sucra,
		mean_rank = mean_rank,
		n_treatments = n_treat,
		n_simulations = n_sim,
		lower_better = lower_better,
		interpretation = sprintf(
			"Treatment ranking by %s (SUCRA, %%). Best: %s (%.1f%%), Worst: %s (%.1f%%)",
			if (lower_better) "lower is better" else "higher is better",
			ranking_summary$treatment[1],
			ranking_summary$sucra[1],
			ranking_summary$treatment[n_treat],
			ranking_summary$sucra[n_treat]
		)
	)
}

#' Create League Table for Network Meta-Analysis
#'
#' Generates a league table showing all pairwise treatment comparisons
#' from a network meta-analysis.
#'
#' @param nma_result Result from network_meta()
#' @param digits Integer. Decimal places for estimates. Default: 2
#' @param show_ci Logical. Show confidence intervals. Default: TRUE
#' @param highlight_sig Logical. Highlight significant comparisons.
#'   Default: TRUE
#'
#' @return ClinicalTable with league table matrix
#' @export
create_league_table <- function(
	nma_result,
	digits = 2,
	show_ci = TRUE,
	highlight_sig = TRUE
) {
	if (!is.list(nma_result) || !"comparisons" %in% names(nma_result)) {
		ph_abort("nma_result must be from network_meta()")
	}

	comparisons <- nma_result$comparisons
	treatments <- nma_result$network$treatments
	n_treat <- length(treatments)
	effect_measure <- nma_result$effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")
	null_value <- if (is_ratio) 1 else 0

	# Create matrix for league table
	# Row treatment vs Column treatment
	# Upper triangle: row vs col
	# Lower triangle: col vs row (reciprocal)

	league_matrix <- matrix("", nrow = n_treat, ncol = n_treat)
	rownames(league_matrix) <- treatments
	colnames(league_matrix) <- treatments

	# Fill diagonal with treatment names
	diag(league_matrix) <- treatments

	# Create a lookup for estimates
	# We need to calculate all pairwise comparisons
	# From NMA results, we have comparisons vs reference

	ref <- nma_result$network$reference
	ref_idx <- which(treatments == ref)

	# Get estimates vs reference for each treatment
	est_vs_ref <- stats::setNames(
		c(
			if (is_ratio) 1 else 0,
			comparisons$estimate[comparisons$treatment != ref]
		),
		c(ref, comparisons$treatment[comparisons$treatment != ref])
	)

	ci_lower_vs_ref <- stats::setNames(
		c(
			if (is_ratio) 1 else 0,
			comparisons$ci_lower[comparisons$treatment != ref]
		),
		c(ref, comparisons$treatment[comparisons$treatment != ref])
	)

	ci_upper_vs_ref <- stats::setNames(
		c(
			if (is_ratio) 1 else 0,
			comparisons$ci_upper[comparisons$treatment != ref]
		),
		c(ref, comparisons$treatment[comparisons$treatment != ref])
	)

	se_vs_ref <- stats::setNames(
		c(0, comparisons$se[comparisons$treatment != ref]),
		c(ref, comparisons$treatment[comparisons$treatment != ref])
	)

	# Calculate all pairwise
	for (i in seq_len(n_treat)) {
		for (j in seq_len(n_treat)) {
			if (i == j) {
				next
			}

			t_row <- treatments[i]
			t_col <- treatments[j]

			# Calculate row vs col
			# If we have A vs ref and B vs ref,
			# then A vs B = (A vs ref) / (B vs ref) for ratios
			# or A vs B = (A vs ref) - (B vs ref) for differences

			if (is_ratio) {
				# On log scale
				log_est_row <- log(est_vs_ref[t_row])
				log_est_col <- log(est_vs_ref[t_col])
				log_diff <- log_est_row - log_est_col

				estimate <- exp(log_diff)

				# Approximate SE for the difference
				se_row <- se_vs_ref[t_row]
				se_col <- se_vs_ref[t_col]
				# Handle zero SEs
				se_row <- if (is.na(se_row) || se_row == 0) 0.001 else se_row
				se_col <- if (is.na(se_col) || se_col == 0) 0.001 else se_col
				se_diff <- sqrt(se_row^2 + se_col^2)

				ci_lower <- exp(log_diff - 1.96 * se_diff)
				ci_upper <- exp(log_diff + 1.96 * se_diff)
			} else {
				est_row <- est_vs_ref[t_row]
				est_col <- est_vs_ref[t_col]
				estimate <- est_row - est_col

				se_row <- se_vs_ref[t_row]
				se_col <- se_vs_ref[t_col]
				se_row <- if (is.na(se_row) || se_row == 0) 0.001 else se_row
				se_col <- if (is.na(se_col) || se_col == 0) 0.001 else se_col
				se_diff <- sqrt(se_row^2 + se_col^2)

				ci_lower <- estimate - 1.96 * se_diff
				ci_upper <- estimate + 1.96 * se_diff
			}

			# Check significance
			is_sig <- (ci_lower > null_value) || (ci_upper < null_value)

			# Format cell
			if (show_ci) {
				cell <- sprintf(
					"%s (%s, %s)",
					format_number(estimate, digits = digits),
					format_number(ci_lower, digits = digits),
					format_number(ci_upper, digits = digits)
				)
			} else {
				cell <- format_number(estimate, digits = digits)
			}

			# Add significance marker
			if (highlight_sig && is_sig) {
				cell <- paste0(cell, "*")
			}

			league_matrix[i, j] <- cell
		}
	}

	# Convert to data frame
	league_df <- as.data.frame(league_matrix)
	league_df <- cbind(Treatment = rownames(league_df), league_df)
	rownames(league_df) <- NULL

	ClinicalTable(
		data = league_df,
		type = "league_table",
		title = sprintf("League Table (%s)", toupper(effect_measure)),
		metadata = list(
			effect_measure = effect_measure,
			n_treatments = n_treat,
			reference = ref,
			note = paste0(
				"Row treatment vs Column treatment. ",
				"* indicates statistical significance."
			)
		)
	)
}

#' Bayesian Meta-Analysis Interface
#'
#' Interface for Bayesian meta-analysis using brms/rstan when available.
#' Provides guidance when dependencies are not installed.
#'
#' @param yi Numeric vector of effect estimates
#' @param sei Numeric vector of standard errors
#' @param study_labels Character vector of study names
#' @param effect_measure Character. Effect type
#' @param prior_mu Prior for overall effect: list(mean, sd)
#' @param prior_tau Prior for heterogeneity: list(type, params)
#' @param chains Integer. Number of MCMC chains. Default: 4
#' @param iter Integer. Total iterations per chain. Default: 4000
#' @param warmup Integer. Warmup iterations. Default: 2000
#' @param seed Integer. Random seed
#' @param ... Additional arguments passed to brms::brm
#'
#' @return List with posterior summaries or guidance for installation
#' @export
bayesian_meta_analysis <- function(
	yi,
	sei,
	study_labels = NULL,
	effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
	prior_mu = list(mean = 0, sd = 10),
	prior_tau = list(type = "half_cauchy", scale = 0.5),
	chains = 4,
	iter = 4000,
	warmup = 2000,
	seed = NULL,
	...
) {
	effect_measure <- match.arg(effect_measure)

	# Check for brms availability
	if (!requireNamespace("brms", quietly = TRUE)) {
		ph_inform(paste(
			"Bayesian meta-analysis requires the 'brms' package.",
			"Install with: install.packages('brms')",
			"",
			"brms also requires rstan or cmdstanr. See:",
			"https://mc-stan.org/users/interfaces/rstan",
			sep = "\n"
		))

		# Return frequentist result as fallback with guidance
		ph_inform("Returning frequentist random-effects estimate as fallback...")

		return(meta_analysis(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			effect_measure = effect_measure,
			model = "random",
			method = "REML"
		))
	}

	k <- length(yi)

	if (is.null(study_labels)) {
		study_labels <- paste("Study", seq_len(k))
	}

	# Prepare data for brms
	data <- data.frame(
		yi = yi,
		sei = sei,
		study = study_labels
	)

	# Set seed if provided
	if (!is.null(seed)) {
		set.seed(seed)
	}

	# Build brms formula
	# Random-effects meta-analysis: yi ~ 1 + (1|study), with known SE
	# This is equivalent to a Bayesian random-effects model

	# Use brms with se() to specify known standard errors
	formula <- brms::bf(yi | se(sei) ~ 1 + (1 | study))

	# Build priors
	priors <- c(
		brms::prior_string(
			sprintf("normal(%f, %f)", prior_mu$mean, prior_mu$sd),
			class = "Intercept"
		),
		brms::prior_string(
			sprintf(
				"%s(0, %f)",
				if (prior_tau$type == "half_cauchy") "cauchy" else "normal",
				prior_tau$scale
			),
			class = "sd"
		)
	)

	# Fit model
	fit <- brms::brm(
		formula = formula,
		data = data,
		prior = priors,
		chains = chains,
		iter = iter,
		warmup = warmup,
		seed = seed,
		refresh = 0, # Suppress iteration output
		...
	)

	# Extract posteriors
	posterior_samples <- brms::as_draws_df(fit)

	# Summarize
	mu_samples <- posterior_samples$b_Intercept
	tau_samples <- posterior_samples$sd_study__Intercept

	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	if (is_ratio) {
		mu_display <- exp(mu_samples)
	} else {
		mu_display <- mu_samples
	}

	# Posterior summaries
	result <- list(
		posterior_mean = mean(mu_display),
		posterior_median = stats::median(mu_display),
		ci_95 = stats::quantile(mu_display, c(0.025, 0.975)),
		ci_80 = stats::quantile(mu_display, c(0.10, 0.90)),
		tau_mean = mean(tau_samples),
		tau_median = stats::median(tau_samples),
		tau_ci_95 = stats::quantile(tau_samples, c(0.025, 0.975)),
		prob_positive = if (is_ratio) {
			mean(mu_display > 1)
		} else {
			mean(mu_display > 0)
		},
		prob_negative = if (is_ratio) {
			mean(mu_display < 1)
		} else {
			mean(mu_display < 0)
		},
		n_studies = k,
		effect_measure = effect_measure,
		model_type = "bayesian_random_effects",
		fit = fit, # Full brms fit object
		interpretation = sprintf(
			"Posterior %s: %.3f (95%% CrI: %.3f to %.3f). P(%s < %s) = %.1f%%",
			if (is_ratio) "median" else "mean",
			if (is_ratio) stats::median(mu_display) else mean(mu_display),
			stats::quantile(mu_display, 0.025),
			stats::quantile(mu_display, 0.975),
			if (is_ratio) "effect" else "effect",
			if (is_ratio) "1" else "0",
			100 * if (is_ratio) mean(mu_display < 1) else mean(mu_display < 0)
		)
	)

	class(result) <- c("bayesian_meta_result", class(result))
	result
}
