#' @title Publication Bias Assessment Functions
#' @name meta_bias
#' @description Functions for assessing and adjusting for publication bias in
#'   meta-analysis.
#' @seealso
#'   \code{\link[=eggers_test]{eggers_test}} for Egger's regression test
#'   of funnel plot asymmetry.
#'
#'   \code{\link[=trim_and_fill]{trim_and_fill}} for Duval & Tweedie
#'   trim-and-fill method.
NULL


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
#'
#' @examples
#' # Egger's test for funnel plot asymmetry
#' yi <- c(0.5, 0.6, 0.4, 0.3, 0.8, 1.0)
#' sei <- c(0.2, 0.15, 0.25, 0.18, 0.12, 0.08)
#' result <- eggers_test(yi = yi, sei = sei)
#' result$p_value
#' result$interpretation
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

	# Validate sei
	if (anyNA(sei) || any(sei <= 0)) {
		invalid_idx <- which(is.na(sei) | sei <= 0)
		ph_abort(sprintf(
			paste(
				"All standard errors (sei) must be positive and non-missing.",
				"Invalid entries at positions: %s"
			),
			paste(invalid_idx, collapse = ", ")
		))
	}

	# Egger's regression: (yi/sei) ~ (1/sei)
	# Model: zi ~ precision, where zi = yi/sei and precision = 1/sei
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


#'
#' Performs the Duval & Tweedie trim-and-fill method to estimate the number
#' of missing studies and adjust the pooled effect for publication bias.
#'
#' @param meta_result A MetaResult object from meta_analysis()
#' @param side Character. Side where excess studies are trimmed: "left",
#'   "right", or "auto". Missing studies are imputed on opposite side.
#'   Default: "auto"
#' @param estimator Character. Method to estimate missing studies:
#'   "L0", "R0", "Q0". Default: "L0"
#' @param maxiter Integer. Maximum iterations. Default: 100
#'
#' @return A list with components:
#' \describe{
#' \item{original}{Original MetaResult object}
#' \item{adjusted}{Adjusted MetaResult with imputed studies}
#' \item{n_imputed}{Estimated number of missing studies}
#' \item{side}{Side where studies were imputed}
#' \item{imputed_studies}{Data frame with imputed effects}
#' \item{estimator}{Estimator method used}
#' \item{summary}{Text summary of the adjustment}
#' }
#'
#' @examples
#' # Trim-and-fill for publication bias adjustment
#' yi <- c(-0.5, -0.4, -0.3, -0.1, 0.0, 0.5, 0.6)
#' sei <- c(0.1, 0.12, 0.15, 0.18, 0.2, 0.1, 0.12)
#' meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")
#' adjusted <- trim_and_fill(meta_res)
#' adjusted$n_imputed
#' adjusted$interpretation
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

	# Validate sei before computing weights
	if (anyNA(sei) || any(sei <= 0)) {
		invalid_idx <- which(is.na(sei) | sei <= 0)
		ph_abort(sprintf(
			paste(
				"All standard errors (sei) must be positive and non-missing.",
				"Invalid entries at positions: %s"
			),
			paste(invalid_idx, collapse = ", ")
		))
	}

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
			# L0 estimator per Duval & Tweedie (2000)
			k0_new <- max(0, round((4 * Tn - n * (n + 1)) / (2 * n - 1)))
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
			# Q0: based on Wilcoxon rank statistic (Duval & Tweedie, 2000)
			# Calculate Wilcoxon rank statistic from signed ranks
			abs_dev <- abs(di)
			sign_dev <- sign(di)
			# Rank absolute deviations (ties handled by rank() default)
			abs_ranks <- rank(abs_dev)
			signed_ranks <- sign_dev * abs_ranks
			# Wilcoxon statistic: sum of positive signed ranks
			Tn <- sum(signed_ranks[signed_ranks > 0])
			# Q0 estimator per Duval & Tweedie (2000)
			k0_new <- max(0, floor((n - 1) / 2 - sqrt(2 * n^2 - 4 * Tn + 1 / 4)))
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
			method = meta_result@metadata$method %||% "DL" # Preserve original method
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
