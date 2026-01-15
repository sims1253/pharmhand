#' Bias-Adjusted Meta-Analysis Functions
#'
#' Functions for performing meta-analysis with risk of bias adjustments,
#' sensitivity analyses, and selection model approaches.
#'
#' @name meta_bias_adjusted
#' @references
#'   Guyatt GH, et al. (2016). GRADE guidelines: 7. Rating the quality of
#'   evidence--inconsistency. J Clin Epidemiol.
#'
#'   Sterne JAC, et al. (2019). RoB 2: a revised tool for assessing risk
#'   of bias in randomised trials. BMJ, 366, l4898.
#'
#'   McShane BB, et al. (2016). Statistical Tests, P-Values, Confidence
#'   Intervals, and Power: A Guide to Misinterpretations. Am Stat.
NULL


#' Calculate Study Weights Based on Risk of Bias
#'
#' Computes inverse-variance weights adjusted for risk of bias assessments.
#' High risk studies receive reduced weights (default 0), and some concerns
#' receive partial weights (default 0.5). Low risk studies receive full weights.
#'
#' @param meta_result A MetaResult object from meta_analysis().
#' @param rob_results List of RoB2Result or ROBINSIResult objects.
#' @param weight_high Numeric. Weight multiplier for high-risk studies.
#'   Default: 0 (exclude completely).
#' @param weight_concerns Numeric. Weight multiplier for "some concerns"
#'   (RoB 2) or "Moderate" (ROBINS-I) studies. Default: 0.5.
#' @param weight_moderate Numeric. Weight multiplier for "Moderate" risk
#'   (ROBINS-I). Default: 0.75.
#' @param weight_serious Numeric. Weight multiplier for "Serious" risk
#'   (ROBINS-I). Default: 0.25.
#' @param weight_critical Numeric. Weight multiplier for "Critical" risk
#'   (ROBINS-I). Default: 0.
#'
#' @return A list with components:
#' \describe{
#' \item{weights}{Numeric vector of adjusted weights}
#' \item{study_ids}{Character vector of study identifiers}
#' \item{rob_judgments}{Character vector of RoB judgments}
#' \item{multipliers}{Numeric vector of weight multipliers applied}
#' \item{method}{Character string describing the weighting method}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create meta-analysis result
#' meta_res <- meta_analysis(
#'   yi = log(c(0.75, 0.82, 0.68, 0.91, 0.77)),
#'   sei = c(0.12, 0.15, 0.18, 0.14, 0.11),
#'   study_labels = paste("Study", 1:5),
#'   effect_measure = "hr"
#' )
#'
#' # Create RoB 2 assessments
#' rob_results <- list(
#'   assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
#'   assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 5", "Low", "Low", "Low", "Some concerns", "Low")
#' )
#'
#' # Calculate RoB-adjusted weights
#' rob_weights <- calculate_rob_weights(
#'   meta_res,
#'   rob_results,
#'   weight_high = 0,
#'   weight_concerns = 0.5
#' )
#' rob_weights$weights
#' rob_weights$rob_judgments
#' }
calculate_rob_weights <- function(
	meta_result,
	rob_results,
	weight_high = 0,
	weight_concerns = 0.5,
	weight_moderate = 0.75,
	weight_serious = 0.25,
	weight_critical = 0
) {
	# Validate inputs
	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	if (!is.list(rob_results) || length(rob_results) == 0) {
		ph_abort("rob_results must be a non-empty list of RoB results")
	}

	# Extract study data from meta_result
	study_labels <- meta_result@metadata$study_labels
	sei <- meta_result@metadata$sei

	if (is.null(study_labels)) {
		study_labels <- paste("Study", seq_along(meta_result@metadata$yi))
	}

	k <- length(study_labels)

	if (k == 0) {
		ph_abort("meta_result contains no study data")
	}

	# Build study ID to RoB judgment mapping
	rob_map <- list()
	for (i in seq_along(rob_results)) {
		result <- rob_results[[i]]

		if (S7::S7_inherits(result, RoB2Result)) {
			rob_map[[result@study_id]] <- list(
				type = "RoB2",
				judgment = result@overall
			)
		} else if (S7::S7_inherits(result, ROBINSIResult)) {
			rob_map[[result@study_id]] <- list(
				type = "ROBINSI",
				judgment = result@overall
			)
		} else {
			ph_abort(sprintf(
				"Element %d of rob_results must be a RoB2Result or ROBINSIResult object",
				i
			))
		}
	}

	# Calculate base inverse-variance weights
	wi <- 1 / (sei^2)

	# Determine if random-effects weights should be used
	is_random <- meta_result@model == "random"
	if (is_random && !is.null(meta_result@heterogeneity$tau2)) {
		tau2 <- meta_result@heterogeneity$tau2
		if (!is.na(tau2) && tau2 > 0) {
			wi <- 1 / (sei^2 + tau2)
		}
	}

	# Initialize multipliers
	multipliers <- rep(1, k)
	rob_judgments <- rep(NA_character_, k)

	# Apply RoB adjustments
	for (i in seq_len(k)) {
		study_id <- study_labels[i]

		if (study_id %in% names(rob_map)) {
			rob_info <- rob_map[[study_id]]
			rob_judgments[i] <- rob_info$judgment

			if (rob_info$type == "RoB2") {
				# RoB 2 judgments: Low, Some concerns, High
				multipliers[i] <- switch(
					rob_info$judgment,
					"Low" = 1,
					"Some concerns" = weight_concerns,
					"High" = weight_high,
					1 # Default to full weight if unknown
				)
			} else if (rob_info$type == "ROBINSI") {
				# ROBINS-I judgments: Low, Moderate, Serious, Critical, No information
				multipliers[i] <- switch(
					rob_info$judgment,
					"Low" = 1,
					"Moderate" = weight_moderate,
					"Serious" = weight_serious,
					"Critical" = weight_critical,
					"No information" = 0.5, # Default partial weight
					1 # Default to full weight if unknown
				)
			}
		} else {
			# Study not in rob_results - use full weight
			rob_judgments[i] <- "Not assessed"
			multipliers[i] <- 1
		}
	}

	# Calculate adjusted weights
	adjusted_wi <- wi * multipliers
	sum_wi <- sum(adjusted_wi)

	if (sum_wi == 0) {
		warning("All adjusted weights are zero; returning uniform weights")
		adjusted_wi <- rep(1, k)
		sum_wi <- k
	}

	# Normalize weights to sum to 1
	normalized_weights <- adjusted_wi / sum_wi

	list(
		weights = stats::setNames(normalized_weights, study_labels),
		study_ids = study_labels,
		rob_judgments = stats::setNames(rob_judgments, study_labels),
		multipliers = stats::setNames(multipliers, study_labels),
		method = sprintf(
			paste0(
				"RoB-adjusted weights (high=%.2f, concerns=%.2f, ",
				"moderate=%.2f, serious=%.2f)"
			),
			weight_high,
			weight_concerns,
			weight_moderate,
			weight_serious
		)
	)
}


#' Sensitivity Analysis Across Risk of Bias Scenarios
#'
#' Performs sensitivity analyses by re-running meta-analysis under different
#' RoB scenarios: including all studies, low-risk only, low+some concerns,
#' and excluding high-risk studies. This helps assess how RoB affects
#' pooled estimates.
#'
#' @param meta_result A MetaResult object from meta_analysis().
#' @param rob_results List of RoB2Result or ROBINSIResult objects.
#' @param method Character. Tau-squared estimation method: "DL", "REML", "PM".
#'   Default: "REML".
#' @param conf_level Numeric. Confidence level. Default: 0.95.
#'
#' @return A list with components:
#' \describe{
#' \item{results}{Data frame with scenario, estimate, CI, I2, tau2, k}
#' \item{scenarios}{Character vector of scenario names}
#' \item{original_estimate}{Original pooled estimate (all studies)}
#' \item{comparison}{Comparison with original estimate}
#' \item{effect_measure}{Effect measure used}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create meta-analysis result
#' meta_res <- meta_analysis(
#'   yi = log(c(0.75, 0.82, 0.68, 0.91, 0.77)),
#'   sei = c(0.12, 0.15, 0.18, 0.14, 0.11),
#'   effect_measure = "hr"
#' )
#'
#' # Create RoB 2 assessments
#' rob_results <- list(
#'   assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
#'   assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 5", "Low", "Low", " concerns", "LowLow", "Some")
#' )
#'
#' # Perform RoB sensitivity analysis
#' sensitivity <- rob_sensitivity_analysis(meta_res, rob_results)
#' sensitivity$results
#' }
rob_sensitivity_analysis <- function(
	meta_result,
	rob_results,
	method = "REML",
	conf_level = 0.95
) {
	# Validate inputs
	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	if (!is.list(rob_results) || length(rob_results) == 0) {
		ph_abort("rob_results must be a non-empty list of RoB results")
	}

	method <- match.arg(method, choices = c("DL", "REML", "PM"))

	# Extract data
	yi <- meta_result@metadata$yi
	sei <- meta_result@metadata$sei
	study_labels <- meta_result@metadata$study_labels

	if (is.null(study_labels)) {
		study_labels <- paste("Study", seq_along(yi))
	}

	k <- length(yi)

	if (k < 2) {
		ph_abort("At least 2 studies required for sensitivity analysis")
	}

	# Determine RoB tool type
	rob_type <- NA_character_
	for (result in rob_results) {
		if (S7::S7_inherits(result, RoB2Result)) {
			rob_type <- "RoB2"
			break
		} else if (S7::S7_inherits(result, ROBINSIResult)) {
			rob_type <- "ROBINSI"
			break
		}
	}

	if (is.na(rob_type)) {
		ph_abort("No recognized RoB result type found in rob_results")
	}

	# Build study ID to judgment mapping
	rob_map <- list()
	for (result in rob_results) {
		if (S7::S7_inherits(result, RoB2Result)) {
			rob_map[[result@study_id]] <- result@overall
		} else if (S7::S7_inherits(result, ROBINSIResult)) {
			rob_map[[result@study_id]] <- result@overall
		}
	}

	# Helper function to run meta-analysis on subset
	run_meta_subset <- function(subset_idx, scenario_name) {
		if (length(subset_idx) < 2) {
			return(list(
				scenario = scenario_name,
				estimate = NA_real_,
				ci = c(NA_real_, NA_real_),
				I2 = NA_real_,
				tau2 = NA_real_,
				k = length(subset_idx),
				p_value = NA_real_,
				warning = "Insufficient studies"
			))
		}

		yi_sub <- yi[subset_idx]
		sei_sub <- sei[subset_idx]
		labels_sub <- study_labels[subset_idx]

		# Run meta-analysis
		result <- tryCatch(
			{
				meta_res <- meta_analysis(
					yi = yi_sub,
					sei = sei_sub,
					study_labels = labels_sub,
					effect_measure = meta_result@effect_measure,
					model = meta_result@model,
					method = method,
					conf_level = conf_level
				)

				list(
					estimate = if (meta_result@effect_measure %in% c("hr", "or", "rr")) {
						log(meta_res@estimate)
					} else {
						meta_res@estimate
					},
					ci = if (meta_result@effect_measure %in% c("hr", "or", "rr")) {
						log(meta_res@ci)
					} else {
						meta_res@ci
					},
					I2 = meta_res@heterogeneity$I2,
					tau2 = meta_res@heterogeneity$tau2,
					k = meta_res@n,
					p_value = meta_res@p_value,
					warning = NULL
				)
			},
			error = function(e) {
				list(
					estimate = NA_real_,
					ci = c(NA_real_, NA_real_),
					I2 = NA_real_,
					tau2 = NA_real_,
					k = length(subset_idx),
					p_value = NA_real_,
					warning = conditionMessage(e)
				)
			}
		)

		result$scenario <- scenario_name
		result
	}

	# Define inclusion criteria for each scenario
	if (rob_type == "RoB2") {
		scenarios <- list(
			list(
				name = "All studies",
				include = rep(TRUE, k)
			),
			list(
				name = "Low risk only",
				include = sapply(study_labels, function(s) {
					id <- which(names(rob_map) == s)
					if (length(id) > 0) rob_map[[s]] == "Low" else FALSE
				})
			),
			list(
				name = "Low + Some concerns",
				include = sapply(study_labels, function(s) {
					id <- which(names(rob_map) == s)
					if (length(id) > 0) {
						rob_map[[s]] %in% c("Low", "Some concerns")
					} else {
						FALSE
					}
				})
			),
			list(
				name = "Excluding High risk",
				include = sapply(study_labels, function(s) {
					id <- which(names(rob_map) == s)
					if (length(id) > 0) rob_map[[s]] != "High" else TRUE
				})
			)
		)
	} else {
		# ROBINS-I scenarios
		scenarios <- list(
			list(
				name = "All studies",
				include = rep(TRUE, k)
			),
			list(
				name = "Low risk only",
				include = sapply(study_labels, function(s) {
					id <- which(names(rob_map) == s)
					if (length(id) > 0) rob_map[[s]] == "Low" else FALSE
				})
			),
			list(
				name = "Low + Moderate",
				include = sapply(study_labels, function(s) {
					id <- which(names(rob_map) == s)
					if (length(id) > 0) {
						rob_map[[s]] %in% c("Low", "Moderate")
					} else {
						FALSE
					}
				})
			),
			list(
				name = "Excluding Serious/Critical",
				include = sapply(study_labels, function(s) {
					id <- which(names(rob_map) == s)
					if (length(id) > 0) {
						!rob_map[[s]] %in% c("Serious", "Critical")
					} else {
						TRUE
					}
				})
			)
		)
	}

	# Run analyses for each scenario
	results_list <- lapply(scenarios, function(s) {
		run_meta_subset(which(s$include), s$name)
	})

	# Build results data frame
	results_df <- data.frame(
		scenario = vapply(results_list, function(r) r$scenario, character(1)),
		estimate = vapply(results_list, function(r) r$estimate, numeric(1)),
		ci_lower = vapply(results_list, function(r) r$ci[1], numeric(1)),
		ci_upper = vapply(results_list, function(r) r$ci[2], numeric(1)),
		I2 = vapply(results_list, function(r) r$I2, numeric(1)),
		tau2 = vapply(results_list, function(r) r$tau2, numeric(1)),
		k = vapply(results_list, function(r) r$k, integer(1)),
		p_value = vapply(results_list, function(r) r$p_value, numeric(1)),
		warning = vapply(results_list, function(r) r$warning %||% "", character(1)),
		stringsAsFactors = FALSE
	)

	# Transform for display if ratio measure
	is_ratio <- meta_result@effect_measure %in% c("hr", "or", "rr")
	if (is_ratio) {
		results_df$estimate_display <- exp(results_df$estimate)
		results_df$ci_lower_display <- exp(results_df$ci_lower)
		results_df$ci_upper_display <- exp(results_df$ci_upper)
	} else {
		results_df$estimate_display <- results_df$estimate
		results_df$ci_lower_display <- results_df$ci_lower
		results_df$ci_upper_display <- results_df$ci_upper
	}

	# Original estimate (all studies)
	original_estimate <- if (is_ratio) {
		log(meta_result@estimate)
	} else {
		meta_result@estimate
	}

	# Calculate comparison with original
	results_df$pct_change_from_original <- if (!is.na(original_estimate)) {
		100 * (results_df$estimate - original_estimate) / abs(original_estimate)
	} else {
		NA_real_
	}

	# Create summary comparison
	scenarios_run <- results_df$scenario
	estimates <- results_df$estimate

	list(
		results = results_df,
		scenarios = scenarios_run,
		original_estimate = original_estimate,
		comparison = list(
			scenarios = scenarios_run,
			estimates = estimates,
			change_from_original = results_df$pct_change_from_original
		),
		effect_measure = meta_result@effect_measure,
		method = method,
		conf_level = conf_level
	)
}


#' Bias-Adjusted Meta-Analysis
#'
#' Performs meta-analysis with adjustment for risk of bias using one of
#' three methods: weight downgrade, exclusion of high-risk studies, or
#' selection model approach.
#'
#' @param meta_result A MetaResult object from meta_analysis().
#' @param rob_results List of RoB2Result or ROBINSIResult objects.
#' @param method Character. Adjustment method: "weight_downgrade",
#'   "exclude_high", or "selection_model". Default: "weight_downgrade".
#' @param weight_high Numeric. Weight for high-risk studies (weight_downgrade).
#'   Default: 0.
#' @param weight_concerns Numeric. Weight for "some concerns" (RoB 2) or
#'   "Moderate" (ROBINS-I) studies. Default: 0.5.
#' @param weight_moderate Numeric. Weight for "Moderate" risk (ROBINS-I).
#'   Default: 0.75.
#' @param weight_serious Numeric. Weight for "Serious" risk (ROBINS-I).
#'   Default: 0.25.
#' @param exclude_high Logical. If TRUE, exclude high-risk studies (used by
#'   "exclude_high" method). Default: TRUE.
#' @param selection_alpha Numeric. Significance level alpha for selection model
#'   (between 0 and 1). Default: 0.05.
#' @param conf_level Numeric. Confidence level. Default: 0.95.
#' @param ... Additional arguments passed to internal methods.
#'
#' @return A BiasAdjustedMetaResult object (extends MetaResult) with:
#' \describe{
#' \item{estimate}{Adjusted pooled effect estimate}
#' \item{ci}{Confidence interval}
#' \item{p_value}{P-value}
#' \item{n}{Number of studies included}
#' \item{method}{Method description}
#' \item{heterogeneity}{Heterogeneity statistics}
#' \item{adjustment_details}{List with adjustment metadata}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create meta-analysis result
#' meta_res <- meta_analysis(
#'   yi = log(c(0.75, 0.82, 0.68, 0.91, 0.77)),
#'   sei = c(0.12, 0.15, 0.18, 0.14, 0.11),
#'   study_labels = paste("Study", 1:5),
#'   effect_measure = "hr"
#' )
#'
#' # Create RoB 2 assessments
#' rob_results <- list(
#'   assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
#'   assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
#'   assess_rob2("Study 5", "Low", "Low", "Low", "Some concerns", "Low")
#' )
#'
#' # Weight downgrade method
#' adjusted <- bias_adjusted_meta(
#'   meta_res,
#'   rob_results,
#'   method = "weight_downgrade",
#'   weight_high = 0,
#'   weight_concerns = 0.5
#' )
#' adjusted@estimate
#' adjusted@ci
#'
#' # Exclude high risk method
#' adjusted2 <- bias_adjusted_meta(
#'   meta_res,
#'   rob_results,
#'   method = "exclude_high"
#' )
#' adjusted2@estimate
#' adjusted2@ci
#' }
bias_adjusted_meta <- function(
	meta_result,
	rob_results,
	method = c("weight_downgrade", "exclude_high", "selection_model"),
	weight_high = 0,
	weight_concerns = 0.5,
	weight_moderate = 0.75,
	weight_serious = 0.25,
	exclude_high = TRUE,
	selection_alpha = 0.05,
	conf_level = 0.95,
	...
) {
	# Validate inputs - check meta_result first
	if (!S7::S7_inherits(meta_result, MetaResult)) {
		ph_abort("meta_result must be a MetaResult object")
	}

	if (!is.list(rob_results) || length(rob_results) == 0) {
		ph_abort("rob_results must be a non-empty list of RoB results")
	}

	# Validate method parameter
	valid_methods <- c("weight_downgrade", "exclude_high", "selection_model")
	if (length(method) != 1 || !method %in% valid_methods) {
		ph_abort(
			"method must be one of 'weight_downgrade', 'exclude_high', or 'selection_model'"
		)
	}

	# Extract data
	yi <- meta_result@metadata$yi
	sei <- meta_result@metadata$sei
	study_labels <- meta_result@metadata$study_labels

	if (is.null(study_labels)) {
		study_labels <- paste("Study", seq_along(yi))
	}

	k <- length(yi)

	if (k < 2) {
		ph_abort("At least 2 studies required for bias-adjusted meta-analysis")
	}

	# Build study ID to judgment mapping
	rob_map <- list()
	rob_type <- NA_character_

	for (result in rob_results) {
		if (S7::S7_inherits(result, RoB2Result)) {
			rob_map[[result@study_id]] <- list(
				type = "RoB2",
				judgment = result@overall
			)
			rob_type <- "RoB2"
		} else if (S7::S7_inherits(result, ROBINSIResult)) {
			rob_map[[result@study_id]] <- list(
				type = "ROBINSI",
				judgment = result@overall
			)
			rob_type <- "ROBINSI"
		}
	}

	# Determine which studies to include and their weights
	study_weights <- rep(1, k)
	include_study <- rep(TRUE, k)
	rob_judgments <- rep(NA_character_, k)

	for (i in seq_len(k)) {
		study_id <- study_labels[i]

		if (study_id %in% names(rob_map)) {
			rob_info <- rob_map[[study_id]]
			rob_judgments[i] <- rob_info$judgment

			if (rob_info$type == "RoB2") {
				study_weights[i] <- switch(
					rob_info$judgment,
					"Low" = 1,
					"Some concerns" = weight_concerns,
					"High" = weight_high
				)
				if (exclude_high && rob_info$judgment == "High") {
					include_study[i] <- FALSE
				}
			} else if (rob_info$type == "ROBINSI") {
				study_weights[i] <- switch(
					rob_info$judgment,
					"Low" = 1,
					"Moderate" = weight_moderate,
					"Serious" = weight_serious,
					"Critical" = 0,
					"No information" = 0.5
				)
				if (exclude_high && rob_info$judgment %in% c("Critical", "Serious")) {
					include_study[i] <- FALSE
				}
			}
		} else {
			rob_judgments[i] <- "Not assessed"
		}
	}

	# Filter included studies
	included_idx <- which(include_study)

	if (length(included_idx) < 2) {
		ph_abort("Fewer than 2 studies remain after applying RoB filters")
	}

	# Apply method-specific adjustments
	if (method == "weight_downgrade") {
		# Calculate adjusted inverse-variance weights
		adjusted_result <- .bias_adjusted_weight_downgrade(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			study_weights = study_weights,
			include_study = include_study,
			meta_result = meta_result,
			conf_level = conf_level,
			rob_judgments = rob_judgments
		)
	} else if (method == "exclude_high") {
		# Simple exclusion of high-risk studies
		adjusted_result <- .bias_adjusted_exclude_high(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			include_study = include_study,
			meta_result = meta_result,
			conf_level = conf_level,
			rob_judgments = rob_judgments
		)
	} else if (method == "selection_model") {
		# Selection model approach
		adjusted_result <- .bias_adjusted_selection_model(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			study_weights = study_weights,
			include_study = include_study,
			meta_result = meta_result,
			conf_level = conf_level,
			selection_alpha = selection_alpha,
			rob_judgments = rob_judgments
		)
	}

	# Add adjustment metadata to the metadata list
	adjusted_result@metadata <- c(
		adjusted_result@metadata,
		list(
			adjustment_method = method,
			original_k = k,
			adjusted_k = length(included_idx),
			excluded_studies = study_labels[!include_study],
			rob_judgments = stats::setNames(rob_judgments, study_labels),
			weight_high = if (method == "weight_downgrade") weight_high else NULL,
			weight_concerns = if (method == "weight_downgrade") {
				weight_concerns
			} else {
				NULL
			},
			exclude_high = exclude_high,
			selection_alpha = if (method == "selection_model") {
				selection_alpha
			} else {
				NULL
			}
		)
	)

	class(adjusted_result) <- c("BiasAdjustedMetaResult", class(adjusted_result))

	adjusted_result
}


#' Internal: Weight downgrade method
#'
#' @keywords internal
.bias_adjusted_weight_downgrade <- function(
	yi,
	sei,
	study_labels,
	study_weights,
	include_study,
	meta_result,
	conf_level,
	rob_judgments
) {
	k <- length(yi)

	# Filter to included studies
	yi_incl <- yi[include_study]
	sei_incl <- sei[include_study]
	labels_incl <- study_labels[include_study]
	weights_incl <- study_weights[include_study]
	rob_judgments_incl <- rob_judgments[include_study]

	# Calculate inverse-variance weights
	wi <- 1 / (sei_incl^2)

	# Apply RoB weight multipliers
	adjusted_wi <- wi * weights_incl

	# Normalize weights
	sum_wi <- sum(adjusted_wi)
	if (sum_wi == 0) {
		ph_warn("All weights are zero; using uniform weights")
		adjusted_wi <- rep(1, length(yi_incl))
		sum_wi <- length(yi_incl)
	}

	# Use random-effects model if original was random-effects
	is_random <- meta_result@model == "random"

	# Calculate adjusted pooled estimate
	theta_fe <- sum(adjusted_wi * yi_incl) / sum(adjusted_wi)

	if (is_random) {
		# Estimate tau2 using inverse-variance weights (not RoB-adjusted)
		# This gives a more stable tau2 estimate
		wi_tau <- 1 / (sei_incl^2)
		theta_fe <- sum(wi_tau * yi_incl) / sum(wi_tau)
		Q <- sum(wi_tau * (yi_incl - theta_fe)^2)
		df <- length(yi_incl) - 1

		# Handle edge case with few studies
		if (length(yi_incl) < 3 || df < 1) {
			# Not enough studies for reliable tau2 estimation
			if (length(yi_incl) >= 2) {
				c_val <- sum(wi_tau) - sum(wi_tau^2) / sum(wi_tau)
				tau2 <- max(0, (Q - df) / c_val)
			} else {
				tau2 <- 0
			}
		} else {
			tau2 <- estimate_tau2(
				yi_incl,
				sei_incl,
				"REML",
				wi_tau,
				theta_fe,
				Q,
				df
			)
		}

		# Validate tau2 - cap extreme values
		# Cap tau2 at a reasonable multiple of the average variance
		avg_var <- mean(sei_incl^2)
		max_tau2 <- 10 * avg_var # Cap at 10x average variance
		if (is.na(tau2) || tau2 < 0 || !is.finite(tau2) || tau2 > max_tau2) {
			tau2 <- min(max(tau2, 0), max_tau2)
		}

		# Random-effects weights with RoB adjustment
		wi_re <- 1 / (sei_incl^2 + tau2)
		adjusted_wi_re <- wi_re * weights_incl
		sum_wi_re <- sum(adjusted_wi_re)

		# Ensure valid sum of weights
		if (sum_wi_re <= 0 || !is.finite(sum_wi_re) || sum_wi_re < 1e-10) {
			# Fall back to simple weighted mean
			theta_adj <- sum(adjusted_wi * yi_incl) / sum(adjusted_wi)
			se_adj <- sqrt(1 / sum(adjusted_wi))
			tau2 <- 0
		} else {
			theta_adj <- sum(adjusted_wi_re * yi_incl) / sum_wi_re
			se_adj <- sqrt(1 / sum_wi_re)
		}
	} else {
		theta_adj <- sum(adjusted_wi * yi_incl) / sum(adjusted_wi)
		se_adj <- sqrt(1 / sum(adjusted_wi))
		tau2 <- 0
	}

	# Validate se_adj - cap extreme values
	if (!is.finite(se_adj) || se_adj <= 0 || se_adj > 10) {
		# Use a more conservative SE estimate based on original SE
		se_adj <- sqrt(sum(sei_incl^2 * weights_incl) / sum(weights_incl))
		if (!is.finite(se_adj) || se_adj <= 0) {
			se_adj <- mean(sei_incl)
		}
	}

	# Confidence interval
	alpha <- 1 - conf_level
	n_incl <- length(yi_incl)
	if (is_random && n_incl > 2) {
		crit <- stats::qt(1 - alpha / 2, df = n_incl - 1)
	} else {
		crit <- stats::qnorm(1 - alpha / 2)
	}

	ci_lower <- theta_adj - crit * se_adj
	ci_upper <- theta_adj + crit * se_adj

	# P-value
	z <- theta_adj / se_adj
	if (is_random && n_incl > 2) {
		p_value <- 2 * stats::pt(-abs(z), df = n_incl - 1)
	} else {
		p_value <- 2 * stats::pnorm(-abs(z))
	}

	# Transform back if ratio measure
	is_ratio <- meta_result@effect_measure %in% c("hr", "or", "rr")

	if (is_ratio) {
		display_estimate <- exp(theta_adj)
		display_ci <- exp(c(ci_lower, ci_upper))
	} else {
		display_estimate <- theta_adj
		display_ci <- c(ci_lower, ci_upper)
	}

	# I-squared calculation
	if (is_random) {
		Q_adj <- sum(adjusted_wi * (yi_incl - theta_fe)^2)
		I2 <- if (Q_adj > 0) max(0, (Q_adj - df) / Q_adj) * 100 else 0
	} else {
		I2 <- NA_real_
	}

	# Prediction interval
	pred_interval <- NULL
	if (is_random && tau2 > 0 && n_incl > 2) {
		se_pred <- sqrt(se_adj^2 + tau2)
		crit_pred <- stats::qt(1 - alpha / 2, df = n_incl - 2)
		pred_interval <- c(
			theta_adj - crit_pred * se_pred,
			theta_adj + crit_pred * se_pred
		)
		if (is_ratio) {
			pred_interval <- exp(pred_interval)
		}
	}

	# Create individual study results
	study_results <- lapply(seq_along(yi_incl), function(i) {
		if (is_ratio) {
			est <- exp(yi_incl[i])
			ci <- exp(
				yi_incl[i] + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sei_incl[i]
			)
		} else {
			est <- yi_incl[i]
			ci <- yi_incl[i] + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sei_incl[i]
		}
		ComparisonResult(
			estimate = est,
			ci = ci,
			ci_level = conf_level,
			effect_measure = meta_result@effect_measure,
			method = "Individual study"
		)
	})
	names(study_results) <- labels_incl

	# Normalized weights for display
	norm_weights <- adjusted_wi / sum(adjusted_wi)

	# Create result object
	result <- MetaResult(
		estimate = display_estimate,
		ci = display_ci,
		ci_level = conf_level,
		p_value = p_value,
		method = sprintf(
			"RoB-adjusted meta-analysis (%s, weight_downgrade method)",
			if (is_random) "random-effects" else "fixed-effect"
		),
		n = length(yi_incl),
		model = if (is_random) "random" else "fixed",
		effect_measure = meta_result@effect_measure,
		heterogeneity = list(
			Q = if (is_random) Q else NA_real_,
			Q_df = if (is_random) df else NA_integer_,
			Q_pvalue = if (is_random) {
				stats::pchisq(Q, df, lower.tail = FALSE)
			} else {
				NA_real_
			},
			I2 = I2,
			H2 = if (is_random) Q / df else NA_real_,
			tau2 = tau2,
			tau = sqrt(tau2)
		),
		weights = stats::setNames(norm_weights, labels_incl),
		prediction_interval = pred_interval,
		study_results = study_results,
		metadata = list(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			rob_judgments = stats::setNames(rob_judgments, study_labels),
			adjustment_method = "weight_downgrade",
			original_method = meta_result@metadata$method %||% "REML"
		)
	)

	result
}


#' Internal: Exclude high-risk studies method
#'
#' @keywords internal
.bias_adjusted_exclude_high <- function(
	yi,
	sei,
	study_labels,
	include_study,
	meta_result,
	conf_level,
	rob_judgments
) {
	# Filter to included studies
	yi_incl <- yi[include_study]
	sei_incl <- sei[include_study]
	labels_incl <- study_labels[include_study]
	rob_judgments_incl <- rob_judgments[include_study]

	# Use original meta_analysis function with filtered data
	result <- meta_analysis(
		yi = yi_incl,
		sei = sei_incl,
		study_labels = labels_incl,
		effect_measure = meta_result@effect_measure,
		model = meta_result@model,
		method = meta_result@metadata$method %||% "REML",
		conf_level = conf_level,
		prediction = TRUE
	)

	# Update metadata
	result@metadata$yi <- yi
	result@metadata$sei <- sei
	result@metadata$study_labels <- study_labels
	result@metadata$rob_judgments <- stats::setNames(rob_judgments, study_labels)
	result@metadata$adjustment_method <- "exclude_high"

	result
}


#' Internal: Selection model method
#'
#' Implements a simplified selection model approach where studies are
#' weighted based on the probability of being selected given their p-values
#' and risk of bias. This is a sensitivity analysis approach.
#'
#' @keywords internal
.bias_adjusted_selection_model <- function(
	yi,
	sei,
	study_labels,
	study_weights,
	include_study,
	meta_result,
	conf_level,
	selection_alpha,
	rob_judgments
) {
	k <- length(yi)

	# Calculate p-values for each study
	z_scores <- yi / sei
	p_values <- 2 * stats::pnorm(-abs(z_scores))

	# Calculate selection probability based on p-value
	# Studies with p < selection_alpha have higher probability of being selected
	selection_prob <- ifelse(p_values < selection_alpha, 1, 0.5)

	# Combine selection probability with RoB weight
	# Final weight = inverse variance * RoB weight * selection probability
	wi <- 1 / (sei^2)
	combined_weights <- wi * study_weights * selection_prob

	# Apply inclusion filter
	include_idx <- which(include_study)
	yi_incl <- yi[include_idx]
	sei_incl <- sei[include_idx]
	weights_incl <- combined_weights[include_idx]
	labels_incl <- study_labels[include_idx]
	rob_judgments_incl <- rob_judgments[include_idx]

	if (length(yi_incl) < 2) {
		ph_abort("Fewer than 2 studies remain after applying RoB filters")
	}

	# Normalize weights
	sum_wi <- sum(weights_incl)
	if (sum_wi == 0) {
		ph_warn("All weights are zero; using uniform weights")
		weights_incl <- rep(1, length(yi_incl))
		sum_wi <- length(yi_incl)
	}

	# Use random-effects model if original was random-effects
	is_random <- meta_result@model == "random"

	# Calculate adjusted pooled estimate
	theta_fe <- sum(weights_incl * yi_incl) / sum(weights_incl)

	if (is_random) {
		# Estimate tau2 using inverse-variance weights (not selection-adjusted)
		# This gives a more stable tau2 estimate
		wi_tau <- 1 / (sei_incl^2)
		theta_fe <- sum(wi_tau * yi_incl) / sum(wi_tau)
		Q <- sum(wi_tau * (yi_incl - theta_fe)^2)
		df <- length(yi_incl) - 1

		# Handle edge case with few studies
		if (length(yi_incl) < 3 || df < 1) {
			if (length(yi_incl) >= 2) {
				c_val <- sum(wi_tau) - sum(wi_tau^2) / sum(wi_tau)
				tau2 <- max(0, (Q - df) / c_val)
			} else {
				tau2 <- 0
			}
		} else {
			tau2 <- estimate_tau2(
				yi_incl,
				sei_incl,
				"REML",
				wi_tau,
				theta_fe,
				Q,
				df
			)
		}

		# Validate tau2 - cap extreme values
		avg_var <- mean(sei_incl^2)
		max_tau2 <- 10 * avg_var
		if (is.na(tau2) || tau2 < 0 || !is.finite(tau2) || tau2 > max_tau2) {
			tau2 <- min(max(tau2, 0), max_tau2)
		}

		# Random-effects weights with adjustment
		wi_re <- 1 / (sei_incl^2 + tau2)
		adjusted_wi_re <- wi_re *
			study_weights[include_idx] *
			selection_prob[include_idx]
		sum_wi_re <- sum(adjusted_wi_re)

		# Ensure valid sum of weights
		if (sum_wi_re <= 0 || !is.finite(sum_wi_re) || sum_wi_re < 1e-10) {
			theta_adj <- sum(weights_incl * yi_incl) / sum(weights_incl)
			se_adj <- sqrt(1 / sum(weights_incl))
			tau2 <- 0
		} else {
			theta_adj <- sum(adjusted_wi_re * yi_incl) / sum_wi_re
			se_adj <- sqrt(1 / sum_wi_re)
		}
	} else {
		theta_adj <- theta_fe
		se_adj <- sqrt(1 / sum_wi)
		tau2 <- 0
	}

	# Validate se_adj - cap extreme values
	if (!is.finite(se_adj) || se_adj <= 0 || se_adj > 10) {
		se_adj <- sqrt(
			sum(
				sei_incl^2 * study_weights[include_idx] * selection_prob[include_idx]
			) /
				sum(study_weights[include_idx] * selection_prob[include_idx])
		)
		if (!is.finite(se_adj) || se_adj <= 0) {
			se_adj <- mean(sei_incl)
		}
	}

	# Confidence interval
	alpha <- 1 - conf_level
	n_incl <- length(yi_incl)
	if (is_random && n_incl > 2) {
		crit <- stats::qt(1 - alpha / 2, df = n_incl - 1)
	} else {
		crit <- stats::qnorm(1 - alpha / 2)
	}

	ci_lower <- theta_adj - crit * se_adj
	ci_upper <- theta_adj + crit * se_adj

	# P-value
	z <- theta_adj / se_adj
	if (is_random && n_incl > 2) {
		p_value <- 2 * stats::pt(-abs(z), df = n_incl - 1)
	} else {
		p_value <- 2 * stats::pnorm(-abs(z))
	}

	# Transform back if ratio measure
	is_ratio <- meta_result@effect_measure %in% c("hr", "or", "rr")

	if (is_ratio) {
		display_estimate <- exp(theta_adj)
		display_ci <- exp(c(ci_lower, ci_upper))
	} else {
		display_estimate <- theta_adj
		display_ci <- c(ci_lower, ci_upper)
	}

	# I-squared calculation
	if (is_random) {
		I2 <- if (Q > 0) max(0, (Q - df) / Q) * 100 else 0
	} else {
		I2 <- NA_real_
	}

	# Prediction interval
	pred_interval <- NULL
	if (is_random && tau2 > 0 && n_incl > 2) {
		se_pred <- sqrt(se_adj^2 + tau2)
		crit_pred <- stats::qt(1 - alpha / 2, df = n_incl - 2)
		pred_interval <- c(
			theta_adj - crit_pred * se_pred,
			theta_adj + crit_pred * se_pred
		)
		if (is_ratio) {
			pred_interval <- exp(pred_interval)
		}
	}

	# Create individual study results
	study_results <- lapply(seq_along(yi_incl), function(i) {
		if (is_ratio) {
			est <- exp(yi_incl[i])
			ci <- exp(
				yi_incl[i] + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sei_incl[i]
			)
		} else {
			est <- yi_incl[i]
			ci <- yi_incl[i] + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sei_incl[i]
		}
		ComparisonResult(
			estimate = est,
			ci = ci,
			ci_level = conf_level,
			effect_measure = meta_result@effect_measure,
			method = "Individual study"
		)
	})
	names(study_results) <- labels_incl

	# Normalized weights for display
	norm_weights <- weights_incl / sum(weights_incl)

	# Create result object
	result <- MetaResult(
		estimate = display_estimate,
		ci = display_ci,
		ci_level = conf_level,
		p_value = p_value,
		method = sprintf(
			"RoB-adjusted meta-analysis (%s, selection_model, alpha=%.2f)",
			if (is_random) "random-effects" else "fixed-effect",
			selection_alpha
		),
		n = length(yi_incl),
		model = if (is_random) "random" else "fixed",
		effect_measure = meta_result@effect_measure,
		heterogeneity = list(
			Q = if (is_random) Q else NA_real_,
			Q_df = if (is_random) df else NA_integer_,
			Q_pvalue = if (is_random) {
				stats::pchisq(Q, df, lower.tail = FALSE)
			} else {
				NA_real_
			},
			I2 = I2,
			H2 = if (is_random) Q / df else NA_real_,
			tau2 = tau2,
			tau = sqrt(tau2)
		),
		weights = stats::setNames(norm_weights, labels_incl),
		prediction_interval = pred_interval,
		study_results = study_results,
		metadata = list(
			yi = yi,
			sei = sei,
			study_labels = study_labels,
			rob_judgments = stats::setNames(rob_judgments, study_labels),
			adjustment_method = "selection_model",
			selection_alpha = selection_alpha,
			original_method = meta_result@metadata$method %||% "REML"
		)
	)

	result
}


#' Print Method for BiasAdjustedMetaResult
#'
#' @param x A BiasAdjustedMetaResult object.
#' @param ... Additional arguments passed to print.
#' @keywords internal
print.BiasAdjustedMetaResult <- function(x, ...) {
	cat("Bias-Adjusted Meta-Analysis Results\n")
	cat("====================================\n")
	cat(sprintf("Method: %s\n", x@method))
	cat(sprintf("Effect Measure: %s\n", x@effect_measure))
	cat(sprintf("Studies included: %d\n", x@n))
	cat(sprintf("\nPooled Estimate: %.3f\n", x@estimate))
	cat(sprintf("95%% CI: [%.3f, %.3f]\n", x@ci[1], x@ci[2]))
	cat(sprintf("P-value: %.4f\n", x@p_value))

	if (!is.null(x@heterogeneity)) {
		cat("\nHeterogeneity:\n")
		cat(sprintf("  I2: %.1f%%\n", x@heterogeneity$I2))
		cat(sprintf("  Tau2: %.4f\n", x@heterogeneity$tau2))
	}

	if (!is.null(x@metadata$adjustment_method)) {
		cat("\nAdjustment Details:\n")
		cat(sprintf("  Original studies: %d\n", x@metadata$original_k))
		cat(sprintf("  Included studies: %d\n", x@metadata$adjusted_k))
		if (length(x@metadata$excluded_studies) > 0) {
			cat(sprintf(
				"  Excluded: %s\n",
				paste(x@metadata$excluded_studies, collapse = ", ")
			))
		}
	}

	invisible(x)
}


#' Summarize Bias-Adjusted Meta-Analysis Results
#'
#' Creates a formatted summary table of bias-adjusted meta-analysis results
#' including the original and adjusted estimates with interpretation.
#'
#' @param adjusted_result A BiasAdjustedMetaResult object from
#'   bias_adjusted_meta().
#' @param original_result A MetaResult object with original
#'   (unadjusted) results.
#' @param digits Integer. Number of decimal places for display. Default: 3.
#'
#' @return A data frame with comparison of original and adjusted results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # (Using results from bias_adjusted_meta example)
#' # summary_df <- summarize_bias_adjusted(adjusted, original_meta)
#' # print(summary_df)
#' }
summarize_bias_adjusted <- function(
	adjusted_result,
	original_result,
	digits = 3
) {
	if (!inherits(adjusted_result, "BiasAdjustedMetaResult")) {
		ph_abort("adjusted_result must be a BiasAdjustedMetaResult object")
	}

	if (!S7::S7_inherits(original_result, MetaResult)) {
		ph_abort("original_result must be a MetaResult object")
	}

	# Calculate percent change
	pct_change <- 100 *
		(adjusted_result@estimate - original_result@estimate) /
		abs(original_result@estimate)

	# Determine interpretation
	if (abs(pct_change) < 5) {
		interpretation <- "Minimal change"
	} else if (abs(pct_change) < 10) {
		interpretation <- "Small change"
	} else if (abs(pct_change) < 20) {
		interpretation <- "Moderate change"
	} else {
		interpretation <- "Large change"
	}

	# Create summary data frame
	summary_df <- data.frame(
		Parameter = c(
			"Effect Estimate",
			"95% CI Lower",
			"95% CI Upper",
			"Number of Studies",
			"I-squared (%)",
			"Tau-squared",
			"P-value"
		),
		Original = c(
			original_result@estimate,
			original_result@ci[1],
			original_result@ci[2],
			original_result@n,
			original_result@heterogeneity$I2,
			original_result@heterogeneity$tau2,
			original_result@p_value
		),
		Adjusted = c(
			adjusted_result@estimate,
			adjusted_result@ci[1],
			adjusted_result@ci[2],
			adjusted_result@n,
			adjusted_result@heterogeneity$I2,
			adjusted_result@heterogeneity$tau2,
			adjusted_result@p_value
		),
		stringsAsFactors = FALSE
	)

	# Add change column
	summary_df$Change <- NA
	numeric_rows <- c(1, 2, 3, 4, 5, 6)
	for (i in numeric_rows) {
		if (
			is.numeric(summary_df$Original[i]) && is.numeric(summary_df$Adjusted[i])
		) {
			summary_df$Change[i] <- summary_df$Adjusted[i] - summary_df$Original[i]
		}
	}

	# Add interpretation
	attr(summary_df, "interpretation") <- interpretation
	attr(summary_df, "pct_change") <- pct_change
	attr(summary_df, "method") <- adjusted_result@method

	class(summary_df) <- c("BiasAdjustedSummary", class(summary_df))

	summary_df
}


#' Print Method for BiasAdjustedSummary
#'
#' @param x A BiasAdjustedSummary object.
#' @param ... Additional arguments passed to print.
#' @keywords internal
print.BiasAdjustedSummary <- function(x, ...) {
	cat("Bias-Adjusted Meta-Analysis Summary\n")
	cat("====================================\n\n")

	interpretation <- attr(x, "interpretation")
	pct_change <- attr(x, "pct_change")
	method <- attr(x, "method")

	cat(sprintf("Method: %s\n", method))
	cat(sprintf(
		"Interpretation: %s (%.1f%% change from original)\n\n",
		interpretation,
		pct_change
	))

	print(as.data.frame(x), digits = 3)

	invisible(x)
}
