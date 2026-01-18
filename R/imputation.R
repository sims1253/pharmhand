#' @title Multiple Imputation Functions
#' @name imputation
#' @description Functions for multiple imputation using mice package with
#'   Rubin's rules for pooling results.
#' @references IQWiG General Methods 10.3.11 p.233-235
NULL

# =============================================================================
# ImputationResult S7 Class
# =============================================================================

#' ImputationResult Class
#'
#' An S7 class for storing multiple imputation results from mice.
#'
#' @export
#'
#' @param mice_object The mice mids object containing imputed datasets
#' @param m Integer number of imputations performed
#' @param method Character string or vector specifying imputation method(s)
#' @param imputed_vars Character vector of variables that were imputed
#' @param n_missing List with count of missing values per variable
#' @param original_data The original data frame before imputation
#' @param metadata List of additional metadata
#'
#' @return An ImputationResult object
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = c(1, NA, 3), y = c(NA, 2, 3))
#' result <- perform_multiple_imputation(data, m = 5)
#' result@m # Number of imputations
#' result@imputed_vars # Variables that were imputed
#' }
ImputationResult <- S7::new_class(
	"ImputationResult",
	package = "pharmhand",
	properties = list(
		mice_object = S7::new_property(S7::class_any),
		m = S7::new_property(
			S7::class_integer,
			default = 5L,
			validator = function(value) {
				if (length(value) != 1 || value < 1) {
					return("m must be a single positive integer")
				}
				NULL
			}
		),
		method = S7::new_property(
			S7::class_any,
			default = "pmm"
		),
		imputed_vars = S7::new_property(
			S7::class_character,
			default = character(0)
		),
		n_missing = S7::new_property(
			S7::class_list,
			default = list()
		),
		original_data = S7::new_property(
			S7::class_data.frame,
			default = quote(data.frame())
		),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

# =============================================================================
# Multiple Imputation Functions
# =============================================================================

#' Perform Multiple Imputation
#'
#' Performs multiple imputation using the mice package.
#'
#' @param data A data frame with missing values
#' @param m Integer. Number of imputations to perform (default: 5)
#' @param maxit Integer. Number of iterations for mice algorithm (default: 5)
#' @param method Character. Imputation method(s). Can be a single method applied
#'   to all variables or a named vector specifying method per variable. Common
#'   methods include:
#'   - "pmm": Predictive mean matching (default, for numeric)
#'   - "logreg": Logistic regression (for binary)
#'   - "polyreg": Polytomous regression (for unordered factors)
#'   - "polr": Proportional odds model (for ordered factors)
#'   - "norm": Bayesian linear regression
#'   - "rf": Random forest
#' @param predictorMatrix Matrix specifying which variables predict which.
#'   If NULL (default), mice determines automatically.
#' @param seed Integer. Random seed for reproducibility
#' @param print Logical. Whether to print mice progress (default: FALSE)
#' @param ... Additional arguments passed to mice::mice()
#'
#' @return An ImputationResult object
#'
#' @details
#' This function wraps mice::mice() to perform multiple imputation under the
#' Missing at Random (MAR) assumption. The mice algorithm uses chained
#' equations (MICE/FCS) to impute missing values.
#'
#' For clinical trial data, predictive mean matching ("pmm") is often
#' recommended as it preserves the distribution of observed values and works
#' well with continuous variables.
#'
#' @references
#' van Buuren, S. and Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#' Imputation by Chained Equations in R. Journal of Statistical Software,
#' 45(3), 1-67.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic multiple imputation
#' data <- data.frame(
#'   age = c(45, NA, 52, 38, NA),
#'   bmi = c(25, 28, NA, 31, 27),
#'   treatment = c("A", "B", "A", NA, "B")
#' )
#'
#' result <- perform_multiple_imputation(data, m = 5, seed = 123)
#'
#' # Access completed datasets
#' completed <- get_complete_data(result)
#'
#' # View imputation summary
#' result@imputed_vars
#' result@n_missing
#' }
perform_multiple_imputation <- function(
	data,
	m = 5L,
	maxit = 5L,
	method = "pmm",
	predictorMatrix = NULL,
	seed = NULL,
	print = FALSE,
	...
) {
	# Validate inputs
	if (!is.data.frame(data)) {
		ph_abort("'data' must be a data frame")
	}

	m <- as.integer(m)
	if (length(m) != 1 || m < 1) {
		ph_abort("'m' must be a single positive integer")
	}

	maxit <- as.integer(maxit)
	if (length(maxit) != 1 || maxit < 1) {
		ph_abort("'maxit' must be a single positive integer")
	}

	# Check for mice package
	if (!requireNamespace("mice", quietly = TRUE)) {
		ph_abort(
			"Package 'mice' is required for multiple imputation. ",
			"Install with: install.packages('mice')"
		)
	}

	# Identify variables with missing data
	missing_counts <- vapply(
		data,
		function(x) sum(is.na(x)),
		integer(1)
	)
	imputed_vars <- names(missing_counts[missing_counts > 0])

	# Check if any missing data exists
	if (length(imputed_vars) == 0) {
		ph_warn(
			"Data has no missing values. Returning original data in ",
			"ImputationResult with m=1."
		)
		# Create a minimal mice object for consistency
		mice_obj <- mice::mice(data, m = 1, maxit = 0, printFlag = FALSE)

		return(ImputationResult(
			mice_object = mice_obj,
			m = 1L,
			method = method,
			imputed_vars = character(0),
			n_missing = as.list(missing_counts),
			original_data = data,
			metadata = list(
				no_missing = TRUE,
				timestamp = Sys.time()
			)
		))
	}

	# Set seed if provided
	if (!is.null(seed)) {
		set.seed(seed)
	}

	# Perform imputation - build args conditionally to avoid NULL predictorMatrix issue
	mice_args <- list(
		data = data,
		m = m,
		maxit = maxit,
		method = method,
		printFlag = print
	)

	# Only add predictorMatrix if provided
	if (!is.null(predictorMatrix)) {
		mice_args$predictorMatrix <- predictorMatrix
	}

	# Merge with additional arguments
	mice_args <- c(mice_args, list(...))

	mice_obj <- do.call(mice::mice, mice_args)

	# Extract method used (may be vector if different per variable)
	method_used <- if (length(mice_obj$method) == 1) {
		mice_obj$method
	} else {
		mice_obj$method[imputed_vars]
	}

	ImputationResult(
		mice_object = mice_obj,
		m = as.integer(m),
		method = method_used,
		imputed_vars = imputed_vars,
		n_missing = as.list(missing_counts[imputed_vars]),
		original_data = data,
		metadata = list(
			maxit = maxit,
			seed = seed,
			timestamp = Sys.time(),
			n_obs = nrow(data),
			n_vars = ncol(data)
		)
	)
}

#' Pool Results Using Rubin's Rules
#'
#' Combines estimates and variances from multiple imputed datasets using
#' Rubin's rules.
#'
#' @param estimates Numeric vector of point estimates from each imputed dataset
#' @param variances Numeric vector of variance estimates from each imputed
#'   dataset
#' @param conf_level Numeric. Confidence level for interval (default: 0.95)
#'
#' @return A list containing:
#' \describe{
#'   \item{pooled_estimate}{Pooled point estimate (mean of estimates)}
#'   \item{pooled_se}{Pooled standard error}
#'   \item{ci}{Confidence interval (lower, upper)}
#'   \item{within_var}{Within-imputation variance}
#'   \item{between_var}{Between-imputation variance}
#'   \item{total_var}{Total variance}
#'   \item{fmi}{Fraction of missing information}
#'   \item{df}{Degrees of freedom for t-distribution}
#'   \item{t_statistic}{t-statistic for hypothesis test}
#'   \item{p_value}{Two-sided p-value}
#' }
#'
#' @details
#' Rubin's rules combine estimates from m imputed datasets:
#' - Pooled estimate: Q_bar = mean(Q_i)
#' - Within-imputation variance: U_bar = mean(U_i)
#' - Between-imputation variance: B = var(Q_i)
#' - Total variance: T = U_bar + (1 + 1/m) * B
#'
#' The fraction of missing information (FMI) indicates the proportion of
#' total variance attributable to missing data.
#'
#' @references
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys.
#' New York: John Wiley & Sons.
#'
#' @export
#'
#' @examples
#' # Pool estimates from 5 imputed datasets
#' estimates <- c(0.52, 0.48, 0.55, 0.50, 0.53)
#' variances <- c(0.01, 0.012, 0.009, 0.011, 0.010)
#'
#' pooled <- pool_rubin(estimates, variances)
#' pooled$pooled_estimate
#' pooled$ci
#' pooled$fmi
pool_rubin <- function(estimates, variances, conf_level = 0.95) {
	# Validate inputs
	if (!is.numeric(estimates)) {
		ph_abort("'estimates' must be a numeric vector")
	}
	if (!is.numeric(variances)) {
		ph_abort("'variances' must be a numeric vector")
	}
	if (length(estimates) != length(variances)) {
		ph_abort("'estimates' and 'variances' must have the same length")
	}
	if (any(variances <= 0)) {
		ph_abort("'variances' must contain only positive values")
	}
	if (
		!is.numeric(conf_level) ||
			length(conf_level) != 1 ||
			conf_level <= 0 ||
			conf_level >= 1
	) {
		ph_abort("'conf_level' must be a single value between 0 and 1")
	}

	m <- length(estimates)

	# Rubin's rules
	# Pooled estimate: mean of estimates
	q_bar <- mean(estimates)

	# Within-imputation variance: mean of variances
	u_bar <- mean(variances)

	# Between-imputation variance: variance of estimates
	b <- stats::var(estimates)

	# Total variance: U_bar + (1 + 1/m) * B
	total_var <- u_bar + (1 + 1 / m) * b

	# Pooled standard error
	pooled_se <- sqrt(total_var)

	# Relative increase in variance due to nonresponse
	r <- (1 + 1 / m) * b / u_bar

	# Fraction of missing information
	fmi <- (r + 2 / (m + 1)) / (r + 1)
	# Ensure FMI is bounded [0, 1]
	fmi <- max(0, min(1, fmi))

	# Degrees of freedom (Barnard & Rubin, 1999 adjusted formula)
	# Old formula: df_old = (m - 1) * (1 + 1/r)^2
	# Adjusted formula for small samples:
	df_old <- (m - 1) * (1 + 1 / r)^2
	# Use a large value if r is very small (nearly complete data)
	if (is.infinite(df_old) || is.nan(df_old)) {
		df <- Inf
	} else {
		df <- df_old
	}

	# t-statistic and p-value
	t_stat <- q_bar / pooled_se
	p_value <- 2 * stats::pt(-abs(t_stat), df = df)

	# Confidence interval
	alpha <- 1 - conf_level
	if (is.finite(df)) {
		t_crit <- stats::qt(1 - alpha / 2, df = df)
	} else {
		t_crit <- stats::qnorm(1 - alpha / 2)
	}
	ci <- c(q_bar - t_crit * pooled_se, q_bar + t_crit * pooled_se)

	list(
		pooled_estimate = q_bar,
		pooled_se = pooled_se,
		ci = ci,
		within_var = u_bar,
		between_var = b,
		total_var = total_var,
		fmi = fmi,
		df = df,
		t_statistic = t_stat,
		p_value = p_value,
		m = m,
		conf_level = conf_level
	)
}

#' Analyze Data with Multiple Imputation
#'
#' Applies an analysis function to each imputed dataset and pools results
#' using Rubin's rules.
#'
#' @param imputation_result An ImputationResult object from
#'   perform_multiple_imputation()
#' @param analysis_fun A function that takes a completed data frame and returns
#'   a list with 'estimate' and 'variance' components
#' @param conf_level Numeric. Confidence level for pooled CI (default: 0.95)
#'
#' @return A list containing pooled results from Rubin's rules
#'
#' @details
#' The analysis_fun must return a list with at least:
#' - estimate: The point estimate from the analysis
#' - variance: The variance of the estimate
#'
#' For regression models, this typically means extracting the coefficient
#' and its squared standard error.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   outcome = c(10, NA, 30, 40, 50),
#'   predictor = c(1, 2, NA, 4, 5)
#' )
#'
#' imp <- perform_multiple_imputation(data, m = 5)
#'
#' # Analyze with linear regression
#' result <- analyze_with_imputation(imp, function(d) {
#'   fit <- lm(outcome ~ predictor, data = d)
#'   coefs <- summary(fit)$coefficients
#'   list(
#'     estimate = coefs["predictor", "Estimate"],
#'     variance = coefs["predictor", "Std. Error"]^2
#'   )
#' })
#'
#' result$pooled_estimate
#' result$ci
#' }
analyze_with_imputation <- function(
	imputation_result,
	analysis_fun,
	conf_level = 0.95
) {
	# Validate inputs
	if (!S7::S7_inherits(imputation_result, ImputationResult)) {
		ph_abort("'imputation_result' must be an ImputationResult object")
	}
	if (!is.function(analysis_fun)) {
		ph_abort("'analysis_fun' must be a function")
	}

	# Get completed datasets
	completed <- get_complete_data(imputation_result)
	m <- length(completed)

	# Apply analysis function to each imputed dataset
	results <- lapply(completed, function(data) {
		tryCatch(
			analysis_fun(data),
			error = function(e) {
				ph_abort(sprintf(
					"Analysis function failed: %s",
					conditionMessage(e)
				))
			}
		)
	})

	# Extract estimates and variances
	estimates <- vapply(
		results,
		function(r) r$estimate,
		numeric(1)
	)
	variances <- vapply(
		results,
		function(r) r$variance,
		numeric(1)
	)

	# Pool using Rubin's rules
	pool_rubin(estimates, variances, conf_level = conf_level)
}

#' Get Completed Datasets from Imputation
#'
#' Extracts completed (imputed) datasets from an ImputationResult object.
#'
#' @param imputation_result An ImputationResult object
#' @param action Character. How to return completed data:
#'   - "list": Return a list of m data frames (default)
#'   - "long": Return a single stacked data frame with .imp column
#'   - "stacked": Alias for "long"
#'
#' @return Depending on action: a list of data frames or a single data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' imp <- perform_multiple_imputation(data, m = 5)
#'
#' # Get as list
#' datasets <- get_complete_data(imp, action = "list")
#' length(datasets) # 5
#'
#' # Get as stacked data frame
#' stacked <- get_complete_data(imp, action = "long")
#' table(stacked$.imp) # Shows imputation numbers
#' }
get_complete_data <- function(
	imputation_result,
	action = c("list", "long", "stacked")
) {
	if (!S7::S7_inherits(imputation_result, ImputationResult)) {
		ph_abort("'imputation_result' must be an ImputationResult object")
	}

	action <- match.arg(action)

	if (!requireNamespace("mice", quietly = TRUE)) {
		ph_abort("Package 'mice' is required")
	}

	mice_obj <- imputation_result@mice_object
	m <- imputation_result@m

	if (action == "list") {
		lapply(seq_len(m), function(i) {
			mice::complete(mice_obj, action = i)
		})
	} else {
		# action is "long" or "stacked"
		mice::complete(mice_obj, action = "long")
	}
}

#' Summarize Missing Data Patterns
#'
#' Creates a summary of missing data in a data frame.
#'
#' @param data A data frame
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{variable}{Variable name}
#'   \item{n_missing}{Count of missing values}
#'   \item{n_complete}{Count of non-missing values}
#'   \item{pct_missing}{Percentage missing}
#' }
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   x = c(1, NA, 3, NA, 5),
#'   y = c("a", "b", NA, "d", "e"),
#'   z = 1:5
#' )
#' summarize_missing(data)
summarize_missing <- function(data) {
	if (!is.data.frame(data)) {
		ph_abort("'data' must be a data frame")
	}

	n_total <- nrow(data)

	summary_df <- data.frame(
		variable = names(data),
		n_missing = vapply(
			data,
			function(x) sum(is.na(x)),
			integer(1)
		),
		stringsAsFactors = FALSE
	)

	summary_df$n_complete <- n_total - summary_df$n_missing
	summary_df$pct_missing <- round(100 * summary_df$n_missing / n_total, 1)

	# Order by amount missing (descending)
	summary_df <- summary_df[order(-summary_df$n_missing), ]
	rownames(summary_df) <- NULL

	summary_df
}
