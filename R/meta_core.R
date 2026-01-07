#' @title Meta-Analysis Core Functions
#' @name meta_core
#' @description Core functions for performing meta-analyses including pooled
#'   estimates and heterogeneity assessment.
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
#'   "DL" (DerSimonian-Laird), "REML", "PM" (Paule-Mandel).
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
	method = c("REML", "DL", "PM"),
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

	# Validate sei values
	if (any(is.na(sei)) || any(sei <= 0)) {
		ph_abort("All standard errors (sei) must be positive and non-missing")
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
				converged <- FALSE
				for (iter in 1:50) {
					wi_star <- 1 / (sei^2 + tau2_reml)
					theta_star <- sum(wi_star * yi) / sum(wi_star)
					resid <- yi - theta_star
					ll_deriv <- -0.5 * sum(wi_star) + 0.5 * sum(wi_star^2 * resid^2)
					ll_deriv2 <- 0.5 * sum(wi_star^2) - sum(wi_star^3 * resid^2)
					if (abs(ll_deriv2) < 1e-10) {
						converged <- TRUE
						break
					}
					tau2_new <- tau2_reml - ll_deriv / ll_deriv2
					if (abs(tau2_new - tau2_reml) < 1e-8) {
						converged <- TRUE
						break
					}
					tau2_reml <- max(0, tau2_new)
				}
				if (!converged) {
					ph_warn("REML estimation did not converge within 50 iterations")
				}
				tau2_reml
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
		# Always use t-distribution for prediction intervals
		crit_pred <- stats::qt(1 - alpha / 2, df = k - 2)
		pred_interval <- c(theta - crit_pred * se_pred, theta + crit_pred * se_pred)
	}

	# Transform back from log scale if ratio measure
	if (effect_measure %in% c("hr", "or", "rr")) {
		display_estimate <- exp(theta)
		display_ci <- exp(c(ci_lower, ci_upper))
		if (!is.null(pred_interval)) {
			display_pred <- exp(pred_interval)
		} else {
			display_pred <- NULL
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
	method = c("REML", "DL", "PM")
) {
	method <- match.arg(method)

	k <- length(yi)

	# Validate sei
	if (any(is.na(sei)) || any(sei <= 0)) {
		ph_abort("All standard errors (sei) must be positive and non-missing")
	}

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
		}
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

	# Extract confidence level from meta_result if available
	conf_level <- if (!is.null(meta_result) && !is.null(meta_result@ci_level)) {
		meta_result@ci_level
	} else {
		0.95
	}
	z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

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
			ci_lower = estimate - z_crit * se,
			ci_upper = estimate + z_crit * se,
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
