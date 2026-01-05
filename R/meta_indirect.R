#' @title Indirect Comparison Functions
#' @name meta_indirect
#' @description Functions for performing indirect treatment comparisons
#'   using the Bucher method and assessing transitivity.
NULL


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
#' # Bucher indirect comparison: Drug A vs Drug B via Placebo
#' # Drug A vs Placebo: HR = 0.75
#' # Drug B vs Placebo: HR = 0.85
#' result <- indirect_comparison(
#'   effect_ab = log(0.75),
#'   se_ab = 0.12,
#'   effect_bc = log(0.85),
#'   se_bc = 0.10,
#'   effect_measure = "hr",
#'   label_a = "Drug A",
#'   label_b = "Placebo",
#'   label_c = "Drug B"
#' )
#' result@estimate
#' result@ci
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
#' @details
#' Transitivity is a fundamental assumption in network meta-analysis. When
#' treatments A and C have not been directly compared in a randomized trial,
#' we may infer their relative effect through a common comparator B (A vs B and
#' B vs C). This indirect comparison is valid only if the studies comparing
#' A vs B and B vs C are sufficiently similar in terms of population
#' characteristics, study design, and clinical setting. Violations of
#' transitivity can bias indirect comparisons and undermine the credibility
#' of network meta-analysis results.
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
#' # Assess transitivity across treatment comparisons
#' chars <- data.frame(
#'   study_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
#'   treatment = c("A", "B", "B", "C", "A", "C"),
#'   mean_age = c(55, 55, 58, 58, 52, 52),
#'   pct_male = c(60, 60, 65, 65, 55, 55)
#' )
#' result <- assess_transitivity(
#'   study_characteristics = chars,
#'   char_vars = c("mean_age", "pct_male"),
#'   continuous_vars = c("mean_age", "pct_male")
#' )
#' result$overall_assessment
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
