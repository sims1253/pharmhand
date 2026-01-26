#' @title Mixed Model Repeated Measures (MMRM)
#' @name mmrm
#' @description Functions for mixed model repeated measures analysis of
#'   longitudinal patient-reported outcomes (PRO) data.
#' @references IQWiG General Methods 10.3.9 p.229-232
NULL

# =============================================================================
# MMRMResult S7 Class
# =============================================================================

#' MMRMResult Class
#'
#' An S7 class for storing MMRM analysis results.
#'
#' @export
#'
#' @param model The mmrm model object
#' @param coefficients Named numeric vector of model coefficients
#' @param se Standard errors for coefficients
#' @param ci Matrix of confidence intervals (2 columns: lower, upper)
#' @param p_values P-values for coefficients
#' @param df Degrees of freedom for coefficients
#' @param sigma Residual standard error
#' @param log_likelihood Log-likelihood value
#' @param aic Akaike Information Criterion
#' @param bic Bayesian Information Criterion
#' @param covariance Covariance structure used
#' @param df_adjustment Degrees of freedom adjustment method
#' @param n_obs Number of observations
#' @param n_subjects Number of subjects
#' @param metadata List of additional metadata
#'
#' @return An MMRMResult object
MMRMResult <- S7::new_class(
	"MMRMResult",
	package = "pharmhand",
	properties = list(
		model = S7::new_property(S7::class_any),
		coefficients = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (anyNA(value)) {
					return("coefficients must not contain NA values")
				}
				NULL
			}
		),
		se = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (anyNA(value)) {
					return("se must not contain NA values")
				}
				if (any(value < 0)) {
					return("se must be positive")
				}
				NULL
			}
		),
		ci = S7::new_property(
			S7::class_any,
			validator = function(value) {
				if (!is.matrix(value)) {
					return("ci must be a matrix")
				}
				if (ncol(value) != 2) {
					return("ci must be a matrix with 2 columns")
				}
				if (anyNA(value)) {
					return("ci must not contain NA values")
				}
				NULL
			}
		),
		p_values = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (anyNA(value)) {
					return("p_values must not contain NA values")
				}
				if (any(value < 0 | value > 1)) {
					return("p_values must be between 0 and 1")
				}
				NULL
			}
		),
		df = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (anyNA(value)) {
					return("df must not contain NA values")
				}
				if (any(value <= 0)) {
					return("df must be positive")
				}
				NULL
			}
		),
		sigma = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (length(value) != 1 || value <= 0) {
					return("sigma must be a single positive value")
				}
				NULL
			}
		),
		log_likelihood = S7::new_property(S7::class_numeric),
		aic = S7::new_property(S7::class_numeric),
		bic = S7::new_property(S7::class_numeric),
		covariance = S7::new_property(
			S7::class_character,
			default = "unstructured"
		),
		df_adjustment = S7::new_property(
			S7::class_character,
			default = "Kenward-Roger"
		),
		n_obs = S7::new_property(S7::class_integer),
		n_subjects = S7::new_property(S7::class_integer),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

# =============================================================================
# MMRM Analysis Functions
# =============================================================================

#' MMRM Analysis
#'
#' Fits a Mixed Model Repeated Measures (MMRM) for longitudinal data analysis.
#'
#' @param data A data frame containing the longitudinal data
#' @param response_var Character. Name of the response variable
#' @param subject_var Character. Name of the subject identifier variable
#' @param trt_var Character. Name of the treatment variable
#' @param time_var Character. Name of the time/visit variable
#' @param covariates Character vector of covariate variable names
#' @param interaction Logical. Whether to include treatment by time interaction
#' @param cov_covariance Character. Covariance structure:
#'   "us" (unstructured), "cs" (compound symmetry), "ar1" (autoregressive),
#'   "ad" (ante-dependence), "toep" (Toeplitz), "sp_exp" (spatial exponential)
#' @param df_adjustment Character. Degrees of freedom adjustment:
#'   "Kenward-Roger", "Satterthwaite", "Residual"
#' @param method Character. Estimation method: "REML" (default) or "ML"
#' @param control List of control parameters for optimization, passed to
#'   \code{\link[mmrm]{mmrm}}. Typically created with
#'   \code{\link[mmrm]{mmrm_control}}.
#'
#' @return An MMRMResult object
#'
#' @details
#' The MMRM model fits:
#' Y_ij = X_ij * beta + epsilon_ij
#'
#' Where Y_ij is the response for subject i at time j,
#' X_ij contains fixed effects (treatment, time, covariates, interactions),
#' and epsilon_ij ~ N(0, Sigma) where Sigma is the covariance matrix.
#'
#' Common covariance structures:
#' - "us": Unstructured - most flexible, all covariances differ
#' - "cs": Compound symmetry - equal variances and covariances
#' - "ar1": Autoregressive - covariance decreases with time distance
#' - "ad": Ante-dependence - flexible for unequal time intervals
#'
#' @references
#' Mallinckrodt, C.H. et al. (2013). Choosing the optimal
#' mixed-effects model for repeated measures data.
#' Statistical Methods in Medical Research, 22(2), 113-138.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic MMRM with treatment and time effects
#' result <- mmrm_analysis(
#'   data = pro_data,
#'   response_var = "AVAL",
#'   subject_var = "USUBJID",
#'   trt_var = "TRT01P",
#'   time_var = "AVISITN",
#'   covariates = "BASE"
#' )
#'
#' # MMRM with interaction and custom covariance
#' result <- mmrm_analysis(
#'   data = pro_data,
#'   response_var = "AVAL",
#'   subject_var = "USUBJID",
#'   trt_var = "TRT01P",
#'   time_var = "AVISITN",
#'   covariates = c("BASE", "AGE"),
#'   interaction = TRUE,
#'   cov_covariance = "cs"
#' )
#' }
mmrm_analysis <- function(
	data,
	response_var,
	subject_var,
	trt_var,
	time_var,
	covariates = NULL,
	interaction = FALSE,
	cov_covariance = c(
		"us",
		"cs",
		"ar1",
		"ad",
		"toep",
		"sp_exp"
	),
	df_adjustment = c("Kenward-Roger", "Satterthwaite", "Residual"),
	method = c("REML", "ML"),
	control = NULL
) {
	# Validate inputs using assertion helpers
	assert_data_frame(data, arg = "data")
	assert_character_scalar(response_var, arg = "response_var")
	assert_character_scalar(subject_var, arg = "subject_var")
	assert_character_scalar(trt_var, arg = "trt_var")
	assert_character_scalar(time_var, arg = "time_var")

	if (!is.null(covariates)) {
		assert_character_vector(covariates, arg = "covariates")
	}

	assert_logical_scalar(interaction, arg = "interaction")
	# Convert to character vectors
	cov_covariance <- match.arg(cov_covariance)
	df_adjustment <- match.arg(df_adjustment)
	method <- match.arg(method)

	# Check mmrm package availability
	if (!requireNamespace("mmrm", quietly = TRUE)) {
		ph_abort(
			"Package 'mmrm' is required for MMRM analysis. ",
			"Install with: install.packages('mmrm')"
		)
	}

	# Check required variables exist
	assert_column_exists(data, response_var, data_arg = "data")
	assert_column_exists(data, subject_var, data_arg = "data")
	assert_column_exists(data, trt_var, data_arg = "data")
	assert_column_exists(data, time_var, data_arg = "data")

	# Check covariates exist
	if (!is.null(covariates)) {
		for (cov in covariates) {
			assert_column_exists(data, cov, data_arg = "data")
		}
	}

	# Ensure subject and time variables are factors (required by mmrm)
	if (!is.factor(data[[subject_var]]) && !is.character(data[[subject_var]])) {
		data[[subject_var]] <- as.factor(data[[subject_var]])
	}
	if (!is.factor(data[[time_var]])) {
		data[[time_var]] <- as.factor(data[[time_var]])
	}

	# Build model formula with covariance structure in formula
	# mmrm requires covariance as: cov_type(time_var | subject_var)
	cov_formula_part <- paste0(
		cov_covariance,
		"(",
		time_var,
		" | ",
		subject_var,
		")"
	)

	fixed_effects <- c(trt_var, time_var)
	if (!is.null(covariates)) {
		fixed_effects <- c(fixed_effects, covariates)
	}

	if (interaction) {
		fixed_effects <- c(fixed_effects, paste0(trt_var, ":", time_var))
	}

	formula_str <- sprintf(
		"%s ~ %s + %s",
		response_var,
		paste(fixed_effects, collapse = " + "),
		cov_formula_part
	)
	formula <- stats::as.formula(formula_str)

	# Map df_adjustment to mmrm method parameter
	method_map <- c(
		"Kenward-Roger" = "Kenward-Roger",
		"Satterthwaite" = "Satterthwaite",
		"Residual" = "Residual"
	)
	mmrm_method <- method_map[df_adjustment]

	# Build the MMRM model
	fit <- tryCatch(
		{
			# Build argument list conditionally
			args <- list(
				formula = formula,
				data = data,
				reml = method == "REML",
				method = mmrm_method
			)
			if (!is.null(control)) {
				args$control <- control
			}
			do.call(mmrm::mmrm, args)
		},
		error = function(e) {
			ph_abort(sprintf("MMRM model fitting failed: %s", conditionMessage(e)))
		}
	)

	# Extract model results
	coef_summary <- summary(fit)$coefficients
	sigma <- sigma(fit)
	loglik <- logLik(fit)
	aic_val <- AIC(fit)
	bic_val <- BIC(fit)

	# Extract components - handle variable column names
	# Some versions use "Std. Error", others use "Std.Error"
	se_col <- if ("Std. Error" %in% colnames(coef_summary)) {
		"Std. Error"
	} else if ("Std.Error" %in% colnames(coef_summary)) {
		"Std.Error"
	} else {
		ph_abort("Cannot find standard error column in summary")
	}

	df_col <- if ("df" %in% colnames(coef_summary)) {
		"df"
	} else if ("DF" %in% colnames(coef_summary)) {
		"DF"
	} else {
		ph_abort("Cannot find degrees of freedom column in summary")
	}

	pval_col <- NULL
	for (col in c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p")) {
		if (col %in% colnames(coef_summary)) {
			pval_col <- col
			break
		}
	}
	if (is.null(pval_col)) {
		ph_abort("Cannot find p-value column in summary")
	}

	# Filter out NA rows (singular coefficients)
	coef_summary <- coef_summary[complete.cases(coef_summary), ]

	estimates <- coef_summary[, "Estimate"]
	ses <- coef_summary[, se_col]
	dfs <- coef_summary[, df_col]

	# Ensure named vectors with proper names
	names(estimates) <- rownames(coef_summary)
	names(ses) <- rownames(coef_summary)
	names(dfs) <- rownames(coef_summary)

	# Compute t critical values (vectorized for different df per coefficient)
	t_crit <- qt(0.975, dfs)

	# Build CI matrix
	ci_matrix <- cbind(
		"2.5 %" = estimates - t_crit * ses,
		"97.5 %" = estimates + t_crit * ses
	)

	# Extract p-values
	p_values <- coef_summary[, pval_col]
	names(p_values) <- rownames(coef_summary)

	MMRMResult(
		model = fit,
		coefficients = estimates,
		se = ses,
		ci = ci_matrix,
		p_values = p_values,
		df = dfs,
		sigma = sigma,
		log_likelihood = loglik,
		aic = aic_val,
		bic = bic_val,
		covariance = cov_covariance,
		df_adjustment = df_adjustment,
		n_obs = nrow(data),
		n_subjects = length(unique(data[[subject_var]])),
		metadata = list(
			response_var = response_var,
			subject_var = subject_var,
			trt_var = trt_var,
			time_var = time_var,
			covariates = covariates,
			interaction = interaction,
			method = method,
			formula = formula_str
		)
	)
}

#' Extract MMRM Model Summary
#'
#' Extracts summary statistics from an MMRMResult object.
#'
#' @param result An MMRMResult object
#' @param digits Integer. Number of decimal places to round to
#'
#' @return A data frame with model summary statistics
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- summary_mmrm(result)
#' print(summary)
#' }
summary_mmrm <- function(result, digits = 3) {
	if (!S7::S7_inherits(result, MMRMResult)) {
		ph_abort("'result' must be an MMRMResult object")
	}

	assert_positive_integer(digits, arg = "digits")

	data.frame(
		Parameter = names(result@coefficients),
		Estimate = round(result@coefficients, digits),
		`Std.Error` = round(result@se, digits),
		`t value` = round(result@coefficients / result@se, digits),
		df = round(result@df, digits),
		`Pr(>|t|)` = format_pvalue(result@p_values),
		`2.5 %` = round(result@ci[, 1], digits),
		`97.5 %` = round(result@ci[, 2], digits),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)
}

#' Create MMRM Table
#'
#' Creates a clinical table from MMRM results.
#'
#' @param result An MMRMResult object
#' @param title Table title
#' @param footnotes Optional footnotes
#' @param autofit Logical. Whether to autofit column widths
#'
#' @return A ClinicalTable object
#' @export
#'
#' @examples
#' \dontrun{
#' table <- create_mmrm_table(result, title = "MMRM Results")
#' }
create_mmrm_table <- function(
	result,
	title = "Mixed Model Repeated Measures Analysis",
	footnotes = NULL,
	autofit = TRUE
) {
	if (!S7::S7_inherits(result, MMRMResult)) {
		ph_abort("'result' must be an MMRMResult object")
	}

	assert_character_scalar(title, arg = "title")

	# Get summary
	summary_df <- summary_mmrm(result)

	# Add metadata
	meta_footnotes <- c(
		paste("Covariance structure:", result@covariance),
		paste("DF adjustment:", result@df_adjustment),
		paste("Observations:", result@n_obs),
		paste("Subjects:", result@n_subjects),
		if (!is.null(footnotes)) footnotes
	)

	create_clinical_table(
		data = summary_df,
		type = "mmrm",
		title = title,
		footnotes = meta_footnotes,
		autofit = autofit
	)
}
