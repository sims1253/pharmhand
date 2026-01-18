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
				if (any(value < 0 | value > 1)) {
					return("p_values must be between 0 and 1")
				}
				if (anyNA(value)) {
					return("p_values must not contain NA values")
				}
				NULL
			}
		),
		df = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (any(value <= 0)) {
					return("df must be positive")
				}
				if (anyNA(value)) {
					return("df must not contain NA values")
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
#'   "unstructured", "compound_symmetry", "autoregressive", "ante_dependence",
#'   "toeplitz", "spatial_exponential", "spatial_power"
#' @param df_adjustment Character. Degrees of freedom adjustment:
#'   "Kenward-Roger", "Satterthwaite", "Residual"
#' @param method Character. Estimation method: "REML" (default) or "ML"
#' @param control List of control parameters for optimization
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
#' - "unstructured": Most flexible, allows all covariances to differ
#' - "compound_symmetry": Equal variances and equal covariances
#' - "autoregressive": Covariance decreases with time distance
#' - "ante_dependence": Flexible for unequal time intervals
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
#'   cov_covariance = "compound_symmetry"
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
		"unstructured",
		"compound_symmetry",
		"autoregressive",
		"ante_dependence",
		"toeplitz",
		"spatial_exponential",
		"spatial_power"
	),
	df_adjustment = c("Kenward-Roger", "Satterthwaite", "Residual"),
	method = c("REML", "ML"),
	control = list()
) {
	# Validate inputs
	if (!is.data.frame(data)) {
		ph_abort("'data' must be a data frame")
	}

	# Check mmrm package availability
	if (!requireNamespace("mmrm", quietly = TRUE)) {
		ph_abort(
			"Package 'mmrm' is required for MMRM analysis. ",
			"Install with: install.packages('mmrm')"
		)
	}

	# Convert to character vectors
	cov_covariance <- match.arg(cov_covariance)
	df_adjustment <- match.arg(df_adjustment)
	method <- match.arg(method)

	# Check required variables exist
	required_vars <- c(response_var, subject_var, trt_var, time_var)
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

	# Build model formula
	fixed_effects <- c(trt_var, time_var)
	if (!is.null(covariates)) {
		fixed_effects <- c(fixed_effects, covariates)
	}

	if (interaction) {
		fixed_effects <- c(fixed_effects, paste0(trt_var, ":", time_var))
	}

	formula_str <- sprintf(
		"%s ~ %s",
		response_var,
		paste(fixed_effects, collapse = " + ")
	)
	formula <- stats::as.formula(formula_str)

	# Build the MMRM model
	tryCatch(
		{
			fit <- mmrm::mmrm(
				formula = formula,
				data = data,
				weights = NULL,
				reml = method == "REML",
				covariance = cov_covariance,
				time = time_var,
				subject = subject_var,
				adjust_method = df_adjustment,
				control = control
			)
		},
		error = function(e) {
			ph_abort(sprintf("MMRM model fitting failed: %s", conditionMessage(e)))
		}
	)

	# Extract model results
	coef_summary <- summary(fit)$coefficients
	sigma <- sqrt(summary(fit)$sigma2)
	loglik <- logLik(fit)
	aic_val <- AIC(fit)
	bic_val <- BIC(fit)

	# Extract components
	estimates <- coef_summary[, "Estimate"]
	ses <- coef_summary[, "Std. Error"]
	dfs <- coef_summary[, "df"]

	# Compute t critical values (vectorized for different df per coefficient)
	t_crit <- qt(0.975, dfs)

	# Build CI matrix
	ci_matrix <- cbind(
		"2.5 %" = estimates - t_crit * ses,
		"97.5 %" = estimates + t_crit * ses
	)

	MMRMResult(
		model = fit,
		coefficients = estimates,
		se = ses,
		ci = ci_matrix,
		p_values = coef_summary[, "Pr(>|t|)"],
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

	data.frame(
		Parameter = names(result@coefficients),
		Estimate = round(result@coefficients, digits),
		`Std. Error` = round(result@se, digits),
		`t-value` = round(result@coefficients / result@se, digits),
		df = round(result@df, digits),
		`P-value` = format_pvalue(result@p_values),
		`2.5 %` = round(result@ci[, 1], digits),
		`97.5 %` = round(result@ci[, 2], digits),
		stringsAsFactors = FALSE
	)
}

#' Create MMRM Table
#'
#' Creates a clinical table from MMRM results.
#'
#' @param result An MMRMResult object
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
#' table <- create_mmrm_table(result, title = "MMRM Results")
#' }
create_mmrm_table <- function(
	result,
	title = "Mixed Model Repeated Measures Analysis",
	subtitle = NULL,
	footnotes = NULL,
	autofit = TRUE
) {
	if (!S7::S7_inherits(result, MMRMResult)) {
		ph_abort("'result' must be an MMRMResult object")
	}

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
		title = title,
		footnotes = meta_footnotes,
		autofit = autofit
	)
}
