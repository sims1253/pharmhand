#' GBA/AMNOG Utilities for Health Technology Assessment
#'
#' Functions for standardized mean difference (SMD) calculation and baseline
#' balance assessment, commonly required for German HTA (G-BA/AMNOG) dossiers
#' and other regulatory submissions.
#'
#' @name gba_utils
#' @keywords internal
NULL

# =============================================================================
# BalanceAssessment S7 Class
# =============================================================================

#' BalanceAssessment S7 Class
#'
#' Container for comprehensive baseline balance assessment results including
#' SMD calculations, imbalance flags, and data for Love plots.
#'
#' @param smd_results Data frame containing SMD results for each variable
#' @param imbalanced_vars Character vector of variable names exceeding threshold
#' @param threshold Numeric threshold used for imbalance flagging
#' @param n_treatment Integer count of subjects in treatment group
#' @param n_control Integer count of subjects in control group
#' @param summary_stats List of summary statistics
#' @param love_plot_data Data frame formatted for Love plot creation
#' @param metadata List of additional metadata
#'
#' @return A BalanceAssessment object
#' @export
BalanceAssessment <- S7::new_class(
	"BalanceAssessment",
	package = "pharmhand",
	properties = list(
		smd_results = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame())
		),
		imbalanced_vars = S7::new_property(
			S7::class_character,
			default = character()
		),
		threshold = S7::new_property(
			S7::class_numeric,
			default = 0.1,
			validator = function(value) {
				if (length(value) != 1 || value <= 0) {
					return("threshold must be a single positive number")
				}
				NULL
			}
		),
		n_treatment = S7::new_property(S7::class_integer, default = 0L),
		n_control = S7::new_property(S7::class_integer, default = 0L),
		summary_stats = S7::new_property(S7::class_list, default = quote(list())),
		love_plot_data = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame())
		),
		metadata = S7::new_property(S7::class_list, default = quote(list())),
		# Computed properties
		n_vars = S7::new_property(
			class = S7::class_integer,
			getter = function(self) nrow(self@smd_results)
		),
		n_imbalanced = S7::new_property(
			class = S7::class_integer,
			getter = function(self) length(self@imbalanced_vars)
		),
		balanced = S7::new_property(
			class = S7::class_logical,
			getter = function(self) length(self@imbalanced_vars) == 0
		)
	)
)

# =============================================================================
# Core SMD Calculation Functions
# =============================================================================

#' Calculate Standardized Mean Difference for Continuous Variables
#'
#' Calculates the standardized mean difference (SMD) between two groups for
#' continuous variables using either Cohen's d or Hedges' g. SMD is a key
#' metric for assessing baseline balance in randomized controlled trials,
#' particularly important for GBA/AMNOG dossier submissions.
#'
#' @param mean1 Numeric. Mean of group 1 (treatment group).
#' @param sd1 Numeric. Standard deviation of group 1. Must be positive.
#' @param n1 Integer. Sample size of group 1. Must be >= 2.
#' @param mean2 Numeric. Mean of group 2 (control/reference group).
#' @param sd2 Numeric. Standard deviation of group 2. Must be positive.
#' @param n2 Integer. Sample size of group 2. Must be >= 2.
#' @param method Character. Method for calculating SMD. One of:
#'   - `"cohens_d"` (default): Uses pooled standard deviation
#'   - `"hedges_g"`: Applies small-sample bias correction to Cohen's d
#' @param conf_level Numeric. Confidence level for CI calculation
#'   (default: 0.95).
#'
#' @details
#' **Cohen's d** is calculated as:
#' \deqn{d = \frac{\bar{x}_1 - \bar{x}_2}{s_{pooled}}}
#'
#' where \eqn{s_{pooled} = \sqrt{\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}}}
#'
#' **Hedges' g** applies a correction factor for small samples:
#' \deqn{g = d \times \left(1 - \frac{3}{4(n_1+n_2)-9}\right)}
#'
#' The 95% confidence interval is calculated using the large-sample variance:
#' \deqn{Var(d) = \frac{n_1+n_2}{n_1 n_2} + \frac{d^2}{2(n_1+n_2)}}
#'
#' @return A named list with components:
#'   - `smd`: The standardized mean difference
#'   - `ci_lower`: Lower bound of confidence interval
#'   - `ci_upper`: Upper bound of confidence interval
#'   - `method`: Method used ("cohens_d" or "hedges_g")
#'   - `se`: Standard error of the SMD
#'
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
#' (2nd ed.). Lawrence Erlbaum Associates.
#'
#' Hedges, L. V., & Olkin, I. (1985). Statistical Methods for Meta-Analysis.
#' Academic Press.
#'
#' @seealso [calculate_smd_binary()] for binary variables,
#'   [calculate_smd_from_data()] for calculating directly from data
#'
#' @export
#'
#' @examples
#' # Cohen's d for age between treatment groups
#' calculate_smd(
#'   mean1 = 55.2, sd1 = 12.3, n1 = 150,
#'   mean2 = 53.8, sd2 = 11.9, n2 = 148
#' )
#'
#' # Hedges' g with small sample correction
#' calculate_smd(
#'   mean1 = 55.2, sd1 = 12.3, n1 = 25,
#'   mean2 = 53.8, sd2 = 11.9, n2 = 23,
#'   method = "hedges_g"
#' )
calculate_smd <- function(
	mean1,
	sd1,
	n1,
	mean2,
	sd2,
	n2,
	method = c("cohens_d", "hedges_g"),
	conf_level = 0.95
) {
	method <- match.arg(method)

	# Input validation
	assert_numeric_scalar(mean1, "mean1")
	assert_numeric_scalar(mean2, "mean2")
	assert_positive(sd1, "sd1")
	assert_positive(sd2, "sd2")
	assert_numeric_scalar(n1, "n1")
	if (n1 < 2) {
		ph_abort("'n1' must be a single integer >= 2")
	}
	assert_numeric_scalar(n2, "n2")
	if (n2 < 2) {
		ph_abort("'n2' must be a single integer >= 2")
	}
	assert_numeric_scalar(conf_level, "conf_level")
	assert_in_range(conf_level, 0, 1, "conf_level")

	n1 <- as.integer(n1)
	n2 <- as.integer(n2)

	# Calculate pooled standard deviation
	pooled_sd <- sqrt(
		((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2)
	)

	# Handle edge case where pooled_sd is 0 or very small
	if (pooled_sd < .Machine$double.eps) {
		ph_warn("Pooled standard deviation is essentially zero. Returning NA.")
		return(list(
			smd = NA_real_,
			ci_lower = NA_real_,
			ci_upper = NA_real_,
			method = method,
			se = NA_real_
		))
	}

	# Calculate Cohen's d
	d <- (mean1 - mean2) / pooled_sd

	# Apply Hedges' correction if requested
	if (method == "hedges_g") {
		# Small sample correction factor (Hedges & Olkin, 1985)
		correction <- 1 - (3 / (4 * (n1 + n2) - 9))
		d <- d * correction
	}

	# Calculate standard error (large sample approximation)
	se <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))

	# Calculate confidence interval
	z_crit <- stats::qnorm((1 + conf_level) / 2)
	ci_lower <- d - z_crit * se
	ci_upper <- d + z_crit * se

	list(
		smd = d,
		ci_lower = ci_lower,
		ci_upper = ci_upper,
		method = method,
		se = se
	)
}

#' Calculate Standardized Mean Difference for Binary Variables
#'
#' Calculates the standardized mean difference (SMD) between two groups for
#' binary or categorical variables using the arcsine transformation or logit
#' method. This is essential for assessing baseline balance of categorical
#' covariates in clinical trials.
#'
#' @param p1 Numeric. Proportion in group 1 (treatment). Must be 0 to 1.
#' @param n1 Integer. Sample size of group 1. Must be >= 2.
#' @param p2 Numeric. Proportion in group 2 (control). Must be 0 to 1.
#' @param n2 Integer. Sample size of group 2. Must be >= 2.
#' @param method Character. Method for calculating SMD. One of:
#'   - `"arcsine"` (default): Uses arcsine square root transformation
#'   - `"logit"`: Uses logit transformation (log odds)
#'   - `"raw"`: Raw proportion difference standardized by pooled variance
#' @param conf_level Numeric. Confidence level for CI (default: 0.95)
#'
#' @details
#' **Arcsine method** (recommended for proportions):
#' \deqn{SMD = 2 \times (\arcsin(\sqrt{p_1}) - \arcsin(\sqrt{p_2}))}
#'
#' This transformation stabilizes variance across the range of proportions
#' and is bounded, making it suitable for proportions near 0 or 1.
#'
#' **Logit method** (log odds ratio):
#' Calculates the log odds ratio and standardizes:
#' \deqn{SMD = \frac{\log(OR)}{\pi/\sqrt{3}}}
#'
#' where \eqn{OR = \frac{p_1/(1-p_1)}{p_2/(1-p_2)}}
#'
#' **Raw method**:
#' \deqn{SMD = \frac{p_1 - p_2}{\sqrt{p(1-p)}}}
#'
#' where \eqn{p = \frac{n_1 p_1 + n_2 p_2}{n_1 + n_2}}
#'
#' @return A named list with components:
#'   - `smd`: The standardized mean difference
#'   - `ci_lower`: Lower bound of confidence interval
#'   - `ci_upper`: Upper bound of confidence interval
#'   - `method`: Method used
#'   - `se`: Standard error of the SMD
#'
#' @references
#' Austin, P. C. (2009). Balance diagnostics for comparing the distribution of
#' baseline covariates between treatment groups in propensity-score matched
#' samples. Statistics in Medicine, 28(25), 3083-3107.
#'
#' @seealso [calculate_smd()] for continuous variables,
#'   [calculate_smd_from_data()] for calculating directly from data
#'
#' @export
#'
#' @examples
#' # Compare sex distribution (60% female in treatment, 55% in control)
#' calculate_smd_binary(p1 = 0.60, n1 = 150, p2 = 0.55, n2 = 148)
#'
#' # Using logit transformation
#' calculate_smd_binary(
#'   p1 = 0.60, n1 = 150,
#'   p2 = 0.55, n2 = 148,
#'   method = "logit"
#' )
calculate_smd_binary <- function(
	p1,
	n1,
	p2,
	n2,
	method = c("arcsine", "logit", "raw"),
	conf_level = 0.95
) {
	method <- match.arg(method)

	# Input validation
	if (!is.numeric(p1) || length(p1) != 1 || is.na(p1)) {
		ph_abort("'p1' must be a single numeric value")
	}
	if (!is.numeric(p2) || length(p2) != 1 || is.na(p2)) {
		ph_abort("'p2' must be a single numeric value")
	}
	if (p1 < 0 || p1 > 1) {
		ph_abort("'p1' must be between 0 and 1")
	}
	if (p2 < 0 || p2 > 1) {
		ph_abort("'p2' must be between 0 and 1")
	}
	if (!is.numeric(n1) || length(n1) != 1 || is.na(n1) || n1 < 2) {
		ph_abort("'n1' must be a single integer >= 2")
	}
	if (!is.numeric(n2) || length(n2) != 1 || is.na(n2) || n2 < 2) {
		ph_abort("'n2' must be a single integer >= 2")
	}
	assert_numeric_scalar(conf_level, "conf_level")
	assert_in_range(conf_level, 0, 1, "conf_level")

	n1 <- as.integer(n1)
	n2 <- as.integer(n2)

	# Apply small continuity correction for extreme proportions
	eps <- 0.5 / max(n1, n2)
	p1_adj <- pmin(pmax(p1, eps), 1 - eps)
	p2_adj <- pmin(pmax(p2, eps), 1 - eps)

	if (method == "arcsine") {
		# Arcsine transformation (Cohen's h)
		# SMD = 2 * (asin(sqrt(p1)) - asin(sqrt(p2)))
		smd <- 2 * (asin(sqrt(p1_adj)) - asin(sqrt(p2_adj)))

		# SE for arcsine SMD
		se <- sqrt(1 / n1 + 1 / n2)
	} else if (method == "logit") {
		# Logit transformation - convert log OR to SMD scale
		# Using the relationship: d = ln(OR) * sqrt(3) / pi

		log_odds1 <- log(p1_adj / (1 - p1_adj))
		log_odds2 <- log(p2_adj / (1 - p2_adj))
		log_or <- log_odds1 - log_odds2

		# Convert to SMD scale
		smd <- log_or * sqrt(3) / pi

		# SE of log OR, then convert to SMD scale
		se_log_or <- sqrt(
			1 / (n1 * p1_adj * (1 - p1_adj)) + 1 / (n2 * p2_adj * (1 - p2_adj))
		)
		se <- se_log_or * sqrt(3) / pi
	} else {
		# Raw proportion difference standardized
		p_pooled <- (n1 * p1 + n2 * p2) / (n1 + n2)

		# Handle edge case where pooled proportion is 0 or 1
		if (
			p_pooled < .Machine$double.eps ||
				p_pooled > 1 - .Machine$double.eps
		) {
			ph_warn("Pooled proportion is at boundary. Returning NA.")
			return(list(
				smd = NA_real_,
				ci_lower = NA_real_,
				ci_upper = NA_real_,
				method = method,
				se = NA_real_
			))
		}

		pooled_sd <- sqrt(p_pooled * (1 - p_pooled))
		smd <- (p1 - p2) / pooled_sd

		# SE using delta method approximation
		se <- sqrt(
			(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2) / (p_pooled * (1 - p_pooled))
		)
	}

	# Calculate confidence interval
	z_crit <- stats::qnorm((1 + conf_level) / 2)
	ci_lower <- smd - z_crit * se
	ci_upper <- smd + z_crit * se

	list(
		smd = smd,
		ci_lower = ci_lower,
		ci_upper = ci_upper,
		method = method,
		se = se
	)
}

#' Calculate SMD Directly from Data
#'
#' Calculates the standardized mean difference for a variable directly from
#' a data frame, automatically detecting whether the variable is continuous
#' or categorical.
#'
#' @param data A data frame containing the analysis data.
#' @param var Character. Name of the variable to calculate SMD for.
#' @param trt_var Character. Name of the treatment group variable.
#' @param ref_group Character or NULL. Value of the reference (control) group.
#'   If NULL, uses the first level of the treatment variable.
#' @param method Character. Method for SMD calculation:
#'   - `"cohens_d"`: Cohen's d for continuous variables
#'   - `"hedges_g"`: Hedges' g (bias-corrected) for continuous variables
#'   - `"arcsine"`: Arcsine transformation for binary/categorical
#'   - `"logit"`: Logit transformation for binary/categorical variables
#'   - `"raw"`: Raw proportions/means without transformation
#'   - `"auto"` (default): Automatically selects based on variable type
#' @param conf_level Numeric. Confidence level for CI (default: 0.95)
#' @param continuous_threshold Integer. Minimum number of unique values to treat
#'   numeric variables as continuous (default: 10). Used only when
#'   `method = "auto"`.
#'
#' @details
#' When `method = "auto"`:
#' - Numeric variables with > continuous_threshold unique values are treated as
#'   continuous (using Cohen's d)
#' - Numeric variables with <= continuous_threshold unique values are treated as
#'   categorical
#' - Character/factor variables are treated as categorical (using arcsine)
#'
#' For categorical variables with more than 2 levels, the function calculates
#' the maximum absolute SMD across all pairwise level comparisons.
#'
#' **Method-specific considerations:**
#' - `"logit"`: Useful for binary variables but requires boundary handling
#'   for proportions at 0 or 1 (adds 0.5/N continuity correction). Results
#'   are on the logit scale; back-transformation is not straightforward.
#' - `"raw"`: Appropriate when no transformation is desired. Calculates SMD
#'   directly from raw proportions for binary variables, standardized by the
#'   pooled standard deviation of the binary variable.
#'
#' @return A named list with components:
#'   - `smd`: The standardized mean difference. For multi-level categorical
#'     variables, returns the SMD with the maximum absolute value, preserving
#'     sign.
#'   - `ci_lower`: Lower bound of confidence interval
#'   - `ci_upper`: Upper bound of confidence interval
#'   - `method`: Method used
#'   - `var_type`: Detected variable type ("continuous" or "categorical")
#'   - `se`: Standard error of the SMD
#'
#' @seealso [calculate_smd()], [calculate_smd_binary()],
#'   [assess_baseline_balance()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create example data
#' adsl <- data.frame(
#'   AGE = c(rnorm(100, 55, 12), rnorm(100, 54, 11)),
#'   SEX = c(sample(c("M", "F"), 100, replace = TRUE, prob = c(0.4, 0.6)),
#'           sample(c("M", "F"), 100, replace = TRUE, prob = c(0.45, 0.55))),
#'   TRT01P = rep(c("Treatment", "Placebo"), each = 100)
#' )
#'
#' # Continuous variable
#' calculate_smd_from_data(adsl, "AGE", "TRT01P", ref_group = "Placebo")
#'
#' # Categorical variable
#' calculate_smd_from_data(adsl, "SEX", "TRT01P", ref_group = "Placebo")
#' }
calculate_smd_from_data <- function(
	data,
	var,
	trt_var,
	ref_group = NULL,
	method = c("auto", "cohens_d", "hedges_g", "arcsine", "logit", "raw"),
	conf_level = 0.95,
	continuous_threshold = 10
) {
	method <- match.arg(method)

	assert_data_frame(data, "data")
	assert_column_exists(data, var, "data")
	assert_column_exists(data, trt_var, "data")
	assert_positive(continuous_threshold, "continuous_threshold")

	# Get treatment groups
	trt_vals <- unique(data[[trt_var]])
	trt_vals <- trt_vals[!is.na(trt_vals)]

	if (length(trt_vals) < 2) {
		ph_abort(paste(
			"Treatment variable '",
			trt_var,
			"' must have at least 2 groups",
			sep = ""
		))
	}

	if (is.null(ref_group)) {
		if (is.factor(data[[trt_var]])) {
			ref_group <- levels(data[[trt_var]])[1]
		} else {
			ref_group <- sort(trt_vals)[1]
		}
	}

	if (!ref_group %in% trt_vals) {
		ph_abort(paste(
			"Reference group '",
			ref_group,
			"' not found in '",
			trt_var,
			"'",
			sep = ""
		))
	}

	# Get comparison group (first non-reference group)
	trt_group <- trt_vals[trt_vals != ref_group][1]

	if (length(trt_vals) > 2) {
		ph_warn(
			paste0(
				"Treatment variable '",
				trt_var,
				"' has ",
				length(trt_vals),
				" groups. ",
				"Only the first non-reference group ('",
				trt_group,
				"') is used. Reference group is '",
				ref_group,
				"'."
			),
			call. = FALSE
		)
	}

	# Split data by treatment
	data_trt <- data[data[[trt_var]] == trt_group & !is.na(data[[var]]), ]
	data_ref <- data[data[[trt_var]] == ref_group & !is.na(data[[var]]), ]

	n1 <- nrow(data_trt)
	n2 <- nrow(data_ref)

	if (n1 < 2 || n2 < 2) {
		ph_warn(paste(
			"Insufficient observations in one or both groups for '",
			var,
			"'",
			sep = ""
		))
		return(list(
			smd = NA_real_,
			ci_lower = NA_real_,
			ci_upper = NA_real_,
			method = method,
			var_type = NA_character_,
			se = NA_real_
		))
	}

	# Determine variable type
	x <- data[[var]]
	is_continuous <- is.numeric(x) &&
		length(unique(x[!is.na(x)])) > continuous_threshold

	if (method == "auto") {
		method <- if (is_continuous) "cohens_d" else "arcsine"
	}

	var_type <- if (is_continuous) "continuous" else "categorical"

	if (is_continuous) {
		# Continuous variable
		mean1 <- mean(data_trt[[var]], na.rm = TRUE)
		sd1 <- stats::sd(data_trt[[var]], na.rm = TRUE)
		mean2 <- mean(data_ref[[var]], na.rm = TRUE)
		sd2 <- stats::sd(data_ref[[var]], na.rm = TRUE)

		result <- calculate_smd(
			mean1 = mean1,
			sd1 = sd1,
			n1 = n1,
			mean2 = mean2,
			sd2 = sd2,
			n2 = n2,
			method = if (method %in% c("cohens_d", "hedges_g")) {
				method
			} else {
				"cohens_d"
			},
			conf_level = conf_level
		)
		result$var_type <- var_type
	} else {
		# Categorical variable
		x_trt <- data_trt[[var]]
		x_ref <- data_ref[[var]]
		all_levels <- unique(c(x_trt, x_ref))
		all_levels <- all_levels[!is.na(all_levels)]

		if (length(all_levels) == 2) {
			# Binary variable - simple case
			p1 <- mean(x_trt == all_levels[1], na.rm = TRUE)
			p2 <- mean(x_ref == all_levels[1], na.rm = TRUE)

			result <- calculate_smd_binary(
				p1 = p1,
				n1 = n1,
				p2 = p2,
				n2 = n2,
				method = if (method %in% c("arcsine", "logit", "raw")) {
					method
				} else {
					"arcsine"
				},
				conf_level = conf_level
			)
			result$var_type <- var_type
		} else {
			# Multi-level categorical - use maximum absolute SMD across levels
			smd_values <- numeric(length(all_levels))
			se_values <- numeric(length(all_levels))

			for (i in seq_along(all_levels)) {
				lvl <- all_levels[i]
				p1 <- mean(x_trt == lvl, na.rm = TRUE)
				p2 <- mean(x_ref == lvl, na.rm = TRUE)

				lvl_result <- calculate_smd_binary(
					p1 = p1,
					n1 = n1,
					p2 = p2,
					n2 = n2,
					method = if (method %in% c("arcsine", "logit", "raw")) {
						method
					} else {
						"arcsine"
					},
					conf_level = conf_level
				)
				smd_values[i] <- lvl_result$smd
				se_values[i] <- lvl_result$se
			}

			# Use maximum absolute SMD
			max_idx <- which.max(abs(smd_values))
			max_smd <- smd_values[max_idx]
			max_se <- se_values[max_idx]

			z_crit <- stats::qnorm((1 + conf_level) / 2)

			result <- list(
				smd = max_smd,
				ci_lower = max_smd - z_crit * max_se,
				ci_upper = max_smd + z_crit * max_se,
				method = if (method %in% c("arcsine", "logit", "raw")) {
					method
				} else {
					"arcsine"
				},
				var_type = var_type,
				se = max_se
			)
		}
	}

	result
}

# =============================================================================
# Table Enhancement Functions
# =============================================================================

#' Add SMD Column to Demographics/Baseline Table
#'
#' Adds a standardized mean difference column to a baseline characteristics
#' table, flagging variables that exceed the imbalance threshold. This is
#' essential for GBA/AMNOG dossiers to demonstrate baseline comparability.
#'
#' @param data A data frame containing the baseline data.
#' @param trt_var Character. Name of the treatment variable.
#' @param vars Character vector. Names of variables to calculate SMD for.
#' @param ref_group Character or NULL. Reference (control) group value.
#'   If NULL, uses the first level.
#' @param threshold Numeric. SMD threshold for flagging imbalance
#'   (default: 0.1). Common thresholds are 0.1 (strict) and 0.25 (lenient).
#' @param conf_level Numeric. Confidence level for CI (default: 0.95)
#' @param continuous_threshold Integer. Minimum number of unique values to treat
#'   numeric variables as continuous (default: 10).
#' @param flag_symbol Character. Symbol to use for flagging imbalanced
#'   variables (default: "*")
#'
#' @return A data frame with columns:
#'   - `variable`: Variable name
#'   - `smd`: Standardized mean difference
#'   - `ci`: Formatted confidence interval
#'   - `imbalanced`: Logical flag for |SMD| > threshold
#'   - `smd_display`: Formatted SMD with flag if imbalanced
#'
#' @references
#' IQWiG (2020). General Methods: Version 6.0. Institute for Quality and
#' Efficiency in Health Care.
#'
#' @seealso [assess_baseline_balance()], [create_love_plot()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' adsl <- data.frame(
#'   AGE = rnorm(200, 55, 12),
#'   WEIGHT = rnorm(200, 75, 15),
#'   SEX = sample(c("M", "F"), 200, replace = TRUE),
#'   TRT01P = rep(c("Treatment", "Placebo"), each = 100)
#' )
#'
#' smd_table <- add_smd_to_table(
#'   data = adsl,
#'   trt_var = "TRT01P",
#'   vars = c("AGE", "WEIGHT", "SEX"),
#'   ref_group = "Placebo"
#' )
#' }
add_smd_to_table <- function(
	data,
	trt_var,
	vars,
	ref_group = NULL,
	threshold = 0.1,
	conf_level = 0.95,
	continuous_threshold = 10,
	flag_symbol = "*"
) {
	assert_data_frame(data, "data")
	assert_column_exists(data, trt_var, "data")
	assert_positive(threshold, "threshold")

	# Check which variables exist
	missing_vars <- vars[!vars %in% names(data)]
	if (length(missing_vars) > 0) {
		ph_warn(paste(
			"Variables not found in data: ",
			paste(missing_vars, collapse = ", "),
			sep = ""
		))
		vars <- vars[vars %in% names(data)]
	}

	if (length(vars) == 0) {
		ph_abort("No valid variables to calculate SMD for")
	}

	# Calculate SMD for each variable
	results <- lapply(vars, function(v) {
		smd_result <- calculate_smd_from_data(
			data = data,
			var = v,
			trt_var = trt_var,
			ref_group = ref_group,
			method = "auto",
			conf_level = conf_level,
			continuous_threshold = continuous_threshold
		)

		data.frame(
			variable = v,
			smd = smd_result$smd,
			se = smd_result$se,
			ci_lower = smd_result$ci_lower,
			ci_upper = smd_result$ci_upper,
			var_type = smd_result$var_type,
			stringsAsFactors = FALSE
		)
	})

	result_df <- do.call(rbind, results)

	# Add imbalance flag and formatted columns
	result_df$imbalanced <- abs(result_df$smd) > threshold

	# Format CI
	result_df$ci <- ifelse(
		is.na(result_df$ci_lower),
		"--",
		sprintf(
			"(%.3f, %.3f)",
			result_df$ci_lower,
			result_df$ci_upper
		)
	)

	# Format SMD with flag
	result_df$smd_display <- ifelse(
		is.na(result_df$smd),
		"--",
		ifelse(
			result_df$imbalanced,
			sprintf("%.3f%s", result_df$smd, flag_symbol),
			sprintf("%.3f", result_df$smd)
		)
	)

	result_df
}

# =============================================================================
# Comprehensive Balance Assessment
# =============================================================================

#' Assess Baseline Balance Between Treatment Groups
#'
#' Performs a comprehensive baseline balance assessment calculating SMD for
#' multiple continuous and categorical variables. Returns a BalanceAssessment
#' object that includes SMD results, imbalance flags, summary statistics, and
#' data formatted for Love plot visualization.
#'
#' @param data A data frame containing the baseline data.
#' @param trt_var Character. Name of the treatment variable.
#' @param continuous_vars Character vector. Names of continuous variables.
#' @param categorical_vars Character vector. Names of categorical variables.
#' @param ref_group Character or NULL. Reference (control) group value.
#' @param threshold Numeric. SMD threshold for imbalance (default: 0.1).
#' @param conf_level Numeric. Confidence level for CIs (default: 0.95).
#' @param continuous_threshold Integer. Minimum number of unique values to treat
#'   numeric variables as continuous (default: 10).
#' @param continuous_method Character. SMD method for continuous variables.
#'   One of "cohens_d" (default) or "hedges_g".
#' @param categorical_method Character. SMD method for categorical variables.
#'   One of "arcsine" (default), "logit", or "raw".
#'
#' @details
#' A threshold of 0.1 is commonly used in clinical trials to indicate
#' meaningful imbalance. Variables with |SMD| > threshold are flagged.
#'
#' The IQWiG (German HTA agency) recommends SMD assessment for all baseline
#' characteristics in benefit assessment dossiers.
#'
#' @return A BalanceAssessment S7 object with:
#'   - `smd_results`: Data frame with SMD for each variable
#'   - `imbalanced_vars`: Character vector of imbalanced variable names
#'   - `threshold`: The threshold used
#'   - `n_treatment`: Sample size in treatment group
#'   - `n_control`: Sample size in control group
#'   - `summary_stats`: List with summary statistics
#'   - `love_plot_data`: Data frame formatted for Love plot
#'
#' @references
#' Austin, P. C. (2009). Balance diagnostics for comparing the distribution of
#' baseline covariates between treatment groups in propensity-score matched
#' samples. Statistics in Medicine, 28(25), 3083-3107.
#'
#' @seealso [create_love_plot()], [add_smd_to_table()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assess baseline balance
#' balance <- assess_baseline_balance(
#'   data = adsl,
#'   trt_var = "TRT01P",
#'   continuous_vars = c("AGE", "WEIGHT", "HEIGHT", "BMI"),
#'   categorical_vars = c("SEX", "RACE", "ETHNIC"),
#'   ref_group = "Placebo",
#'   threshold = 0.1
#' )
#'
#' # Check if balanced
#' balance@balanced
#'
#' # View imbalanced variables
#' balance@imbalanced_vars
#'
#' # Create Love plot
#' create_love_plot(balance)
#' }
assess_baseline_balance <- function(
	data,
	trt_var,
	continuous_vars = character(),
	categorical_vars = character(),
	ref_group = NULL,
	threshold = 0.1,
	conf_level = 0.95,
	continuous_threshold = 10,
	continuous_method = c("cohens_d", "hedges_g"),
	categorical_method = c("arcsine", "logit", "raw")
) {
	continuous_method <- match.arg(continuous_method)
	categorical_method <- match.arg(categorical_method)

	assert_data_frame(data, "data")
	assert_column_exists(data, trt_var, "data")
	assert_positive(threshold, "threshold")

	all_vars <- c(continuous_vars, categorical_vars)
	if (length(all_vars) == 0) {
		ph_abort("At least one variable must be specified")
	}

	# Get treatment groups
	trt_vals <- unique(data[[trt_var]])
	trt_vals <- trt_vals[!is.na(trt_vals)]

	if (is.null(ref_group)) {
		if (is.factor(data[[trt_var]])) {
			ref_group <- levels(data[[trt_var]])[1]
		} else {
			ref_group <- sort(trt_vals)[1]
		}
	}

	trt_group <- trt_vals[trt_vals != ref_group][1]

	# Get sample sizes
	n_treatment <- sum(data[[trt_var]] == trt_group, na.rm = TRUE)
	n_control <- sum(data[[trt_var]] == ref_group, na.rm = TRUE)

	# Calculate SMD for continuous variables
	cont_results <- if (length(continuous_vars) > 0) {
		lapply(continuous_vars, function(v) {
			if (!v %in% names(data)) {
				ph_warn(paste0("Variable '", v, "' not found, skipping"), call. = FALSE)
				return(NULL)
			}
			smd_result <- calculate_smd_from_data(
				data = data,
				var = v,
				trt_var = trt_var,
				ref_group = ref_group,
				method = continuous_method,
				conf_level = conf_level,
				continuous_threshold = continuous_threshold
			)
			data.frame(
				variable = v,
				var_type = "continuous",
				smd = smd_result$smd,
				se = smd_result$se,
				ci_lower = smd_result$ci_lower,
				ci_upper = smd_result$ci_upper,
				method = smd_result$method,
				stringsAsFactors = FALSE
			)
		})
	} else {
		list()
	}

	# Calculate SMD for categorical variables
	cat_results <- if (length(categorical_vars) > 0) {
		lapply(categorical_vars, function(v) {
			if (!v %in% names(data)) {
				ph_warn(paste0("Variable '", v, "' not found, skipping"), call. = FALSE)
				return(NULL)
			}
			smd_result <- calculate_smd_from_data(
				data = data,
				var = v,
				trt_var = trt_var,
				ref_group = ref_group,
				method = categorical_method,
				conf_level = conf_level,
				continuous_threshold = continuous_threshold
			)
			data.frame(
				variable = v,
				var_type = "categorical",
				smd = smd_result$smd,
				se = smd_result$se,
				ci_lower = smd_result$ci_lower,
				ci_upper = smd_result$ci_upper,
				method = smd_result$method,
				stringsAsFactors = FALSE
			)
		})
	} else {
		list()
	}

	# Combine results
	all_results <- c(cont_results, cat_results)
	all_results <- all_results[!sapply(all_results, is.null)]

	if (length(all_results) == 0) {
		ph_abort("No valid variables to assess")
	}

	smd_results <- do.call(rbind, all_results)

	# Add imbalance flag
	smd_results$imbalanced <- abs(smd_results$smd) > threshold

	# Identify imbalanced variables
	imbalanced_vars <- smd_results$variable[
		smd_results$imbalanced & !is.na(smd_results$imbalanced)
	]

	# Create Love plot data (sorted by absolute SMD)
	love_plot_data <- smd_results[
		order(abs(smd_results$smd), decreasing = TRUE),
	]
	love_plot_data$variable <- factor(
		love_plot_data$variable,
		levels = rev(love_plot_data$variable)
	)

	# Summary statistics
	summary_stats <- list(
		n_vars = nrow(smd_results),
		n_continuous = sum(smd_results$var_type == "continuous"),
		n_categorical = sum(smd_results$var_type == "categorical"),
		n_imbalanced = length(imbalanced_vars),
		pct_imbalanced = 100 * length(imbalanced_vars) / nrow(smd_results),
		mean_abs_smd = mean(abs(smd_results$smd), na.rm = TRUE),
		max_abs_smd = max(abs(smd_results$smd), na.rm = TRUE),
		treatment_group = trt_group,
		control_group = ref_group
	)

	# Create BalanceAssessment object
	BalanceAssessment(
		smd_results = smd_results,
		imbalanced_vars = imbalanced_vars,
		threshold = threshold,
		n_treatment = as.integer(n_treatment),
		n_control = as.integer(n_control),
		summary_stats = summary_stats,
		love_plot_data = love_plot_data,
		metadata = list(
			conf_level = conf_level,
			continuous_method = continuous_method,
			categorical_method = categorical_method,
			created = Sys.time()
		)
	)
}

# =============================================================================
# Love Plot Visualization
# =============================================================================

#' Create Love Plot for SMD Visualization
#'
#' Creates a Love plot (also known as a covariate balance plot) showing
#' standardized mean differences for baseline variables with threshold
#' reference lines. This visualization is commonly used in HTA dossiers
#' and propensity score analysis to assess covariate balance.
#'
#' @param balance_assessment A BalanceAssessment object from
#'   [assess_baseline_balance()], or a data frame with columns `variable`,
#'   `smd`, and optionally `ci_lower`, `ci_upper`, `var_type`.
#' @param threshold Numeric. SMD threshold for reference lines (default: 0.1).
#'   Vertical lines are drawn at +/- threshold. When `balance_assessment` is a
#'   `BalanceAssessment` object, its stored threshold is used regardless of
#'   this parameter.
#' @param show_ci Logical. Show confidence intervals as error bars
#'   (default: TRUE).
#' @param title Character. Plot title
#'   (default: "Standardized Mean Differences").
#' @param xlab Character. X-axis label
#'   (default: "Standardized Mean Difference").
#' @param color_by_type Logical. Color points by variable type (continuous vs
#'   categorical) when available (default: TRUE).
#' @param sort_by Character. How to sort variables: "abs_smd" (default),
#'   "smd", "name", or "none".
#' @param colors Named character vector of colors for "continuous",
#'   "categorical", and "threshold" elements. NULL uses defaults.
#' @param point_size Numeric. Size of points (default: 3).
#' @param base_size Numeric. Base font size (default: 11).
#'
#' @return A ClinicalPlot object containing a ggplot2 Love plot.
#'
#' @details
#' The Love plot displays each baseline covariate on the y-axis with its
#' SMD on the x-axis. Vertical reference lines at +/- threshold help
#' identify imbalanced covariates.
#'
#' Points falling outside the threshold lines indicate potential baseline
#' imbalance that may require adjustment in the analysis.
#'
#' @references
#' Love, T. E. (2004). Demonstrating balance between groups in observational
#' studies. Presentation at the Cleveland Chapter of the American Statistical
#' Association.
#'
#' @seealso [assess_baseline_balance()], [add_smd_to_table()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # From BalanceAssessment object
#' balance <- assess_baseline_balance(
#'   data = adsl,
#'   trt_var = "TRT01P",
#'   continuous_vars = c("AGE", "WEIGHT"),
#'   categorical_vars = c("SEX", "RACE")
#' )
#' love_plot <- create_love_plot(balance)
#'
#' # From data frame
#' smd_data <- data.frame(
#'   variable = c("Age", "Sex", "Weight"),
#'   smd = c(0.05, -0.12, 0.08),
#'   ci_lower = c(-0.02, -0.20, 0.01),
#'   ci_upper = c(0.12, -0.04, 0.15)
#' )
#' create_love_plot(smd_data, threshold = 0.1)
#' }
create_love_plot <- function(
	balance_assessment,
	threshold = 0.1,
	show_ci = TRUE,
	title = "Standardized Mean Differences",
	xlab = "Standardized Mean Difference",
	color_by_type = TRUE,
	sort_by = c("abs_smd", "smd", "name", "none"),
	colors = NULL,
	point_size = 3,
	base_size = 11
) {
	sort_by <- match.arg(sort_by)

	# Extract data from BalanceAssessment or use data frame directly
	if (S7::S7_inherits(balance_assessment, BalanceAssessment)) {
		plot_data <- balance_assessment@love_plot_data
		threshold <- balance_assessment@threshold
	} else if (is.data.frame(balance_assessment)) {
		plot_data <- balance_assessment
		if (!"variable" %in% names(plot_data) || !"smd" %in% names(plot_data)) {
			ph_abort("Data frame must contain 'variable' and 'smd' columns")
		}
	} else {
		ph_abort(
			"'balance_assessment' must be a BalanceAssessment object or data frame"
		)
	}

	# Handle missing columns
	if (!"ci_lower" %in% names(plot_data)) {
		plot_data$ci_lower <- NA_real_
		show_ci <- FALSE
	}
	if (!"ci_upper" %in% names(plot_data)) {
		plot_data$ci_upper <- NA_real_
		show_ci <- FALSE
	}
	if (!"var_type" %in% names(plot_data)) {
		plot_data$var_type <- "unknown"
		color_by_type <- FALSE
	}

	# Sort data
	if (sort_by == "abs_smd") {
		plot_data <- plot_data[order(abs(plot_data$smd)), ]
	} else if (sort_by == "smd") {
		plot_data <- plot_data[order(plot_data$smd), ]
	} else if (sort_by == "name") {
		plot_data <- plot_data[order(plot_data$variable), ]
	}

	# Convert variable to factor with proper ordering
	plot_data$variable <- factor(
		plot_data$variable,
		levels = plot_data$variable
	)

	# Default colors
	if (is.null(colors)) {
		colors <- c(
			continuous = "#0072B2",
			categorical = "#D55E00",
			unknown = "#666666",
			threshold = "#CC79A7"
		)
	}

	# Build plot
	p <- ggplot2::ggplot(
		plot_data,
		ggplot2::aes(x = .data$smd, y = .data$variable)
	)

	# Add threshold reference lines
	p <- p +
		ggplot2::geom_vline(
			xintercept = 0,
			linetype = "solid",
			color = "gray40",
			linewidth = 0.5
		) +
		ggplot2::geom_vline(
			xintercept = c(-threshold, threshold),
			linetype = "dashed",
			color = colors["threshold"],
			linewidth = 0.6
		)

	# Add confidence intervals if requested
	if (show_ci && !all(is.na(plot_data$ci_lower))) {
		if (color_by_type) {
			p <- p +
				ggplot2::geom_errorbar(
					ggplot2::aes(
						xmin = .data$ci_lower,
						xmax = .data$ci_upper,
						color = .data$var_type
					),
					height = 0.2,
					linewidth = 0.6,
					na.rm = TRUE
				)
		} else {
			p <- p +
				ggplot2::geom_errorbar(
					ggplot2::aes(xmin = .data$ci_lower, xmax = .data$ci_upper),
					height = 0.2,
					linewidth = 0.6,
					color = "gray40",
					na.rm = TRUE
				)
		}
	}

	# Add points
	if (color_by_type) {
		p <- p +
			ggplot2::geom_point(
				ggplot2::aes(color = .data$var_type),
				size = point_size
			) +
			ggplot2::scale_color_manual(
				values = colors,
				name = "Variable Type",
				labels = function(x) tools::toTitleCase(x)
			)
	} else {
		p <- p +
			ggplot2::geom_point(size = point_size, color = colors["continuous"])
	}

	# Apply theme and labels
	p <- p +
		ggplot2::labs(
			title = title,
			x = xlab,
			y = NULL
		) +
		ggplot2::theme_minimal(base_size = base_size) +
		ggplot2::theme(
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor = ggplot2::element_blank(),
			plot.title = ggplot2::element_text(hjust = 0.5),
			legend.position = if (color_by_type) "bottom" else "none"
		)

	# Add annotation for threshold
	x_range <- range(
		c(
			plot_data$smd,
			plot_data$ci_lower,
			plot_data$ci_upper,
			-threshold,
			threshold
		),
		na.rm = TRUE
	)
	x_max <- max(abs(x_range)) * 1.1

	p <- p +
		ggplot2::coord_cartesian(xlim = c(-x_max, x_max)) +
		ggplot2::annotate(
			"text",
			x = threshold,
			y = Inf,
			label = paste0("threshold = ", threshold),
			hjust = -0.1,
			vjust = 1.5,
			size = 3,
			color = colors["threshold"]
		)

	# Calculate appropriate height based on number of variables
	n_vars <- nrow(plot_data)
	plot_height <- max(4, 2 + n_vars * 0.35)

	ClinicalPlot(
		plot = p,
		data = plot_data,
		type = "love_plot",
		title = title,
		width = 8,
		height = plot_height,
		dpi = 300,
		metadata = list(
			threshold = threshold,
			n_variables = n_vars,
			n_imbalanced = sum(abs(plot_data$smd) > threshold, na.rm = TRUE)
		)
	)
}

# =============================================================================
# Multiplicity Adjustment Functions
# =============================================================================

#' Adjust P-values for Multiple Comparisons
#'
#' Adjusts p-values for multiple testing using common methods. Essential for
#' subgroup analyses in GBA/AMNOG dossiers where multiple comparisons require
#' appropriate correction.
#'
#' @param p Numeric vector of p-values
#' @param method Character. Adjustment method:
#'   - "holm" (default): Holm-Bonferroni step-down (controls FWER)
#'   - "hochberg": Hochberg step-up (controls FWER)
#'   - "hommel": Hommel method (controls FWER)
#'   - "bonferroni": Bonferroni correction (controls FWER, conservative)
#'   - "BH" or "fdr": Benjamini-Hochberg (controls FDR)
#'   - "BY": Benjamini-Yekutieli (controls FDR under dependency)
#'   - "none": No adjustment
#' @param alpha Numeric. Significance level (default: 0.05)
#'
#' @return A data frame with columns:
#'   - original_p: Original p-values
#'   - adjusted_p: Adjusted p-values
#'   - significant_original: Logical for original significance
#'   - significant_adjusted: Logical for adjusted significance
#'   - method: Adjustment method used
#'
#' @details
#' FWER methods control the probability of making any false rejection.
#' FDR methods control the expected proportion of false rejections.
#'
#' For GBA submissions, Holm or Hochberg methods are commonly recommended
#' as they control FWER while being less conservative than Bonferroni.
#'
#' @export
#'
#' @examples
#' # Adjust p-values from multiple subgroup comparisons
#' p_values <- c(0.01, 0.04, 0.03, 0.15, 0.008)
#' adjust_pvalues(p_values, method = "holm")
#'
#' # Compare different adjustment methods
#' adjust_pvalues(p_values, method = "bonferroni")
#' adjust_pvalues(p_values, method = "BH")
#'
#' # With custom significance level
#' adjust_pvalues(p_values, method = "holm", alpha = 0.10)
adjust_pvalues <- function(
	p,
	method = c(
		"holm",
		"hochberg",
		"hommel",
		"bonferroni",
		"BH",
		"fdr",
		"BY",
		"none"
	),
	alpha = 0.05
) {
	method <- match.arg(method)

	# Input validation
	if (!is.numeric(p)) {
		ph_abort("'p' must be a numeric vector")
	}
	if (any(p < 0 | p > 1, na.rm = TRUE)) {
		ph_abort("All p-values must be between 0 and 1")
	}
	if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
		ph_abort("'alpha' must be a single number between 0 and 1")
	}

	# Handle "fdr" as alias for "BH"
	if (method == "fdr") {
		method <- "BH"
	}

	# Calculate adjusted p-values
	if (method == "none") {
		adjusted_p <- p
	} else {
		adjusted_p <- stats::p.adjust(p, method = method)
	}

	# Build result data frame
	result <- data.frame(
		original_p = p,
		adjusted_p = adjusted_p,
		significant_original = p < alpha,
		significant_adjusted = adjusted_p < alpha,
		method = method,
		stringsAsFactors = FALSE
	)

	# Handle NA values in significance columns
	result$significant_original[is.na(result$original_p)] <- NA
	result$significant_adjusted[is.na(result$adjusted_p)] <- NA

	result
}

#' Calculate Number Needed to Treat (NNT) / Number Needed to Harm (NNH)
#'
#' Calculates NNT or NNH from risk difference with confidence intervals.
#' Positive values indicate NNT (benefit), negative values indicate NNH (harm).
#'
#' @param rd Numeric. Risk difference (treatment - control)
#' @param rd_lower Numeric. Lower CI bound for risk difference
#' @param rd_upper Numeric. Upper CI bound for risk difference
#' @param event_type Character. "benefit" for favorable outcomes (default),
#'   "harm" for adverse events
#'
#' @return A list with:
#'   - nnt: Number needed to treat (positive) or harm (negative)
#'   - nnt_lower: Lower CI bound
#'   - nnt_upper: Upper CI bound
#'   - interpretation: Text interpretation
#'
#' @details
#' NNT = 1 / absolute_risk_difference
#'
#' When the CI for RD crosses zero, the NNT CI will include infinity,
#' indicating the treatment effect is not statistically significant.
#'
#' For GBA benefit assessment, NNT provides a clinically interpretable
#' measure of absolute treatment effect.
#'
#' @export
#'
#' @examples
#' # Treatment increases response rate (beneficial for benefit endpoint)
#' calculate_nnt(rd = 0.10, rd_lower = 0.05, rd_upper = 0.15)
#'
#' # Treatment reduces adverse events (beneficial for harm endpoint)
#' calculate_nnt(
#'   rd = -0.08, rd_lower = -0.14, rd_upper = -0.02, event_type = "harm"
#' )
#'
#' # Non-significant effect (CI crosses zero)
#' calculate_nnt(rd = -0.05, rd_lower = -0.12, rd_upper = 0.02)
calculate_nnt <- function(
	rd,
	rd_lower = NULL,
	rd_upper = NULL,
	event_type = c("benefit", "harm")
) {
	event_type <- match.arg(event_type)

	# Input validation
	if (!is.numeric(rd) || length(rd) != 1) {
		ph_abort("'rd' must be a single numeric value")
	}
	if (abs(rd) > 1) {
		ph_abort("'rd' must be between -1 and 1")
	}
	if (!is.null(rd_lower)) {
		if (!is.numeric(rd_lower) || length(rd_lower) != 1) {
			ph_abort("'rd_lower' must be a single numeric value")
		}
		if (rd_lower > rd) {
			ph_abort("'rd_lower' must be less than or equal to 'rd'")
		}
	}
	if (!is.null(rd_upper)) {
		if (!is.numeric(rd_upper) || length(rd_upper) != 1) {
			ph_abort("'rd_upper' must be a single numeric value")
		}
		if (rd_upper < rd) {
			ph_abort("'rd_upper' must be greater than or equal to 'rd'")
		}
	}

	# Calculate NNT (using absolute value of RD)
	if (abs(rd) < .Machine$double.eps) {
		nnt <- Inf
	} else {
		nnt <- 1 / abs(rd)
	}

	# Determine if this is NNT (benefit) or NNH (harm)
	# For benefit endpoints (e.g., response rate):
	#   rd > 0 means treatment increases beneficial events = good
	#   rd < 0 means treatment decreases beneficial events = bad
	# For harm endpoints (e.g., adverse events):
	#   rd < 0 means treatment reduces harmful events = good
	#   rd > 0 means treatment increases harmful events = bad
	is_beneficial <- (event_type == "benefit" && rd > 0) ||
		(event_type == "harm" && rd < 0)
	is_harmful <- (event_type == "benefit" && rd < 0) ||
		(event_type == "harm" && rd > 0)

	# Calculate CI for NNT
	# Note: CI bounds are inverted because NNT = 1/RD
	nnt_lower <- NA_real_
	nnt_upper <- NA_real_
	ci_crosses_zero <- FALSE

	if (!is.null(rd_lower) && !is.null(rd_upper)) {
		ci_crosses_zero <- rd_lower < 0 && rd_upper > 0

		if (ci_crosses_zero) {
			# CI crosses zero - NNT CI is not estimable
			# Set bounds to NA to avoid mixed positive/negative bounds
			nnt_lower <- NA_real_
			nnt_upper <- NA_real_
		} else if (rd > 0) {
			# Both bounds positive (harmful)
			nnt_lower <- 1 / rd_upper
			nnt_upper <- 1 / rd_lower
		} else {
			# Both bounds negative (beneficial)
			nnt_lower <- 1 / abs(rd_lower)
			nnt_upper <- 1 / abs(rd_upper)
		}
	}

	# Build interpretation
	if (abs(rd) < .Machine$double.eps) {
		interpretation <- "No difference between groups"
	} else if (ci_crosses_zero) {
		interpretation <- paste0(
			"NNT not estimable (CI crosses zero). ",
			"Point estimate NNT = ",
			round(nnt, 1),
			if (is_beneficial) " to benefit" else " to harm"
		)
	} else if (is_beneficial) {
		ci_text <- if (is.na(nnt_lower) || is.na(nnt_upper)) {
			"NA"
		} else {
			paste0(round(nnt_lower, 1), " to ", round(nnt_upper, 1))
		}
		interpretation <- paste0(
			"NNT = ",
			round(nnt, 1),
			" (95% CI: ",
			ci_text,
			"). ",
			"Treat ",
			round(nnt, 0),
			" patients for one additional patient to benefit."
		)
	} else if (is_harmful) {
		ci_text <- if (is.na(nnt_lower) || is.na(nnt_upper)) {
			"NA"
		} else {
			paste0(round(nnt_lower, 1), " to ", round(nnt_upper, 1))
		}
		interpretation <- paste0(
			"NNH = ",
			round(nnt, 1),
			" (95% CI: ",
			ci_text,
			"). ",
			"Treat ",
			round(nnt, 0),
			" patients for one additional patient to be harmed."
		)
	} else {
		interpretation <- "No treatment effect"
	}

	# Return signed NNT (positive = benefit, negative = harm)
	signed_nnt <- if (is_harmful) -nnt else nnt

	list(
		nnt = signed_nnt,
		nnt_lower = nnt_lower,
		nnt_upper = nnt_upper,
		rd = rd,
		rd_lower = rd_lower,
		rd_upper = rd_upper,
		ci_crosses_zero = ci_crosses_zero,
		event_type = event_type,
		interpretation = interpretation
	)
}
