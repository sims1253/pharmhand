#' @title Responder Analysis Tables
#' @name efficacy_responder
#' @description Functions for responder/binary endpoint analysis and
#'   non-inferiority testing.
NULL

#' Create Responder Summary Table
#'
#' Generates a response rate table with confidence intervals and
#' treatment comparisons (odds ratio, risk ratio, or risk difference)
#' for binary endpoints like ORR, CR, PR.
#'
#' @param data ADaMData object or data frame
#' @param response_var Variable containing response values
#' @param response_values Character vector of values indicating response
#'   (default: c("CR", "PR") for tumor response)
#' @param trt_var Treatment variable name (default: "TRT01P")
#' @param ref_group Reference group for comparison. If NULL, uses first
#'   level of treatment variable.
#' @param ci_method Method for CI calculation: "wilson" (recommended),
#'   "exact" (Clopper-Pearson), or "wald"
#' @param comparison_type "OR" for odds ratio, "RR" for risk ratio,
#'   "RD" for risk difference
#' @param conf_level Confidence level (default: 0.95)
#' @param title Table title
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable object with response summary
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic tumor response table
#' resp_table <- create_responder_table(
#'   data = adrs,
#'   response_var = "AVALC",
#'   response_values = c("CR", "PR"),
#'   title = "Best Overall Response"
#' )
#'
#' # With risk ratio instead of odds ratio
#' resp_table <- create_responder_table(
#'   data = adrs,
#'   comparison_type = "RR",
#'   title = "Response Rate Summary"
#' )
#' }
create_responder_table <- function(
	data,
	response_var = "AVALC",
	response_values = c("CR", "PR"),
	trt_var = "TRT01P",
	ref_group = NULL,
	ci_method = c("wilson", "exact", "wald"),
	comparison_type = c("OR", "RR", "RD"),
	conf_level = 0.95,
	title = "Response Rate Summary",
	autofit = TRUE
) {
	ci_method <- match.arg(ci_method)
	comparison_type <- match.arg(comparison_type)

	# Get filtered data
	df <- get_filtered_data(data)
	trt_var_actual <- get_trt_var(data, default = trt_var)

	# Define responder
	df$responder <- as.integer(df[[response_var]] %in% response_values)

	# Ensure treatment is factor
	df[[trt_var_actual]] <- as.factor(df[[trt_var_actual]])
	trt_levels <- levels(df[[trt_var_actual]])

	if (is.null(ref_group)) {
		ref_group <- trt_levels[1]
	}

	# Response rate by treatment
	response_summary <- df |>
		dplyr::group_by(!!rlang::sym(trt_var_actual)) |>
		dplyr::summarise(
			N = dplyr::n(),
			responders = sum(.data$responder, na.rm = TRUE),
			rate = ifelse(.data$N > 0, .data$responders / .data$N, NA_real_),
			.groups = "drop"
		)

	# Filter out empty groups
	response_summary <- response_summary[response_summary$N > 0, , drop = FALSE]

	# Add confidence intervals
	response_summary$ci_lower <- NA_real_
	response_summary$ci_upper <- NA_real_

	for (i in seq_len(nrow(response_summary))) {
		ci <- calculate_proportion_ci(
			response_summary$responders[i],
			response_summary$N[i],
			method = ci_method,
			conf_level = conf_level
		)
		response_summary$ci_lower[i] <- ci$lower
		response_summary$ci_upper[i] <- ci$upper
	}

	# Format for display
	response_summary$`n/N` <- paste0(
		response_summary$responders,
		"/",
		response_summary$N
	)
	response_summary$`Rate (%)` <- sprintf("%.1f", response_summary$rate * 100)
	ci_col_name <- paste0(round(conf_level * 100), "% CI")
	response_summary[[ci_col_name]] <- sprintf(
		"(%.1f, %.1f)",
		response_summary$ci_lower * 100,
		response_summary$ci_upper * 100
	)

	# Calculate treatment comparison (only if >1 treatment)
	if (length(trt_levels) > 1) {
		comparison <- calculate_response_comparison(
			df = df,
			trt_var = trt_var_actual,
			ref_group = ref_group,
			comparison_type = comparison_type,
			conf_level = conf_level
		)

		response_summary[[paste0(comparison_type, " (95% CI)")]] <- NA_character_
		response_summary$`p-value` <- NA_character_

		# Reference group
		ref_idx <- which(response_summary[[trt_var_actual]] == ref_group)
		response_summary[[paste0(comparison_type, " (95% CI)")]][ref_idx] <-
			"Reference"
		response_summary$`p-value`[ref_idx] <- "-"

		# Other groups
		for (trt in names(comparison)) {
			trt_idx <- which(response_summary[[trt_var_actual]] == trt)
			if (length(trt_idx) > 0) {
				comp <- comparison[[trt]]
				if (comparison_type == "RD") {
					response_summary[[paste0(comparison_type, " (95% CI)")]][trt_idx] <-
						sprintf(
							"%.1f (%.1f, %.1f)",
							comp$estimate * 100,
							comp$lcl * 100,
							comp$ucl * 100
						)
				} else {
					response_summary[[paste0(comparison_type, " (95% CI)")]][trt_idx] <-
						sprintf(
							"%.2f (%.2f, %.2f)",
							comp$estimate,
							comp$lcl,
							comp$ucl
						)
				}
				response_summary$`p-value`[trt_idx] <- format_pvalue(comp$pvalue)
			}
		}
	}

	# Select and rename columns for display
	display_cols <- c(trt_var_actual, "n/N", "Rate (%)", ci_col_name)
	if (paste0(comparison_type, " (95% CI)") %in% names(response_summary)) {
		display_cols <- c(
			display_cols,
			paste0(comparison_type, " (95% CI)"),
			"p-value"
		)
	}

	display_df <- response_summary[, display_cols, drop = FALSE]
	names(display_df)[1] <- "Treatment"

	# Create flextable
	ft <- create_hta_table(
		display_df,
		title = title,
		footnotes = c(
			paste("Response defined as:", paste(response_values, collapse = ", ")),
			paste("95% CI calculated using", ci_method, "method"),
			if (length(trt_levels) > 1) {
				paste(comparison_type, "compared to", ref_group)
			} else {
				NULL
			}
		),
		autofit = autofit
	)

	ClinicalTable(
		data = display_df,
		flextable = ft,
		type = "responder",
		title = title,
		metadata = list(
			response_values = response_values,
			ci_method = ci_method,
			comparison_type = comparison_type,
			ref_group = ref_group
		)
	)
}

#' Non-Inferiority Test
#'
#' Tests non-inferiority of treatment vs comparator.
#'
#' @param data Data frame
#' @param outcome_var Character. Outcome variable name
#' @param trt_var Character. Treatment variable name
#' @param ref_group Character. Reference group name
#' @param ni_margin Numeric. Non-inferiority margin (positive value)
#' @param type Character. "continuous" or "binary"
#' @param higher_better Logical. TRUE if higher values are better
#'   (default: TRUE)
#' @param conf_level Numeric. One-sided confidence level
#'   (default: 0.975, equivalent to two-sided 95% CI)
#' @param method Character. Method for binary endpoints only: "wald", "wilson",
#'   "exact" (default: "wilson"). Ignored for continuous endpoints.
#'
#' @return List with:
#'   - estimate: Point estimate of difference (trt - ref)
#'   - ci_lower: Lower bound of one-sided CI
#'   - ci_upper: Upper bound (may be Inf for one-sided)
#'   - ni_margin: The margin used
#'   - non_inferior: Logical. TRUE if non-inferiority is concluded.
#'     When higher_better=TRUE: ci_lower > -ni_margin.
#'     When higher_better=FALSE: ci_upper < ni_margin.
#'   - conclusion: Character summary
#'   - method: Method used
#'
#' @details
#' For continuous endpoints: Tests if (mean_trt - mean_ref) + ni_margin > 0
#' For binary endpoints: Tests if (prop_trt - prop_ref) + ni_margin > 0
#'
#' Non-inferiority is concluded if the lower bound of the one-sided CI
#' for the treatment difference exceeds -ni_margin.
#'
#' The direction of the test depends on higher_better:
#' - If higher_better=TRUE (default): the lower CI bound must exceed -ni_margin
#' - If higher_better=FALSE: the upper CI bound must be below ni_margin
#'
#'
#' For binary endpoints, the Wilson method uses the Newcombe-Wilson
#' hybrid CI approach. The Wald method provides a simpler alternative.
#' For regulatory submissions, verify the CI method aligns with
#' agency preferences.
#'
#' @references
#' IQWiG Methods v8.0, Section 10.3.5, p. 217-218.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Non-inferiority test for continuous endpoint
#' result <- test_non_inferiority(
#'   data = adeff,
#'   outcome_var = "CHG",
#'   trt_var = "TRT01P",
#'   ref_group = "Active Control",
#'   ni_margin = 0.5,
#'   type = "continuous"
#' )
#'
#' # Non-inferiority test for binary endpoint
#' result <- test_non_inferiority(
#'   data = adrs,
#'   outcome_var = "AVALC",
#'   trt_var = "TRT01P",
#'   ref_group = "Active Control",
#'   ni_margin = 0.10,
#'   type = "binary"
#' )
#' }
test_non_inferiority <- function(
	data,
	outcome_var,
	trt_var,
	ref_group,
	ni_margin,
	type = c("continuous", "binary"),
	higher_better = TRUE,
	conf_level = 0.975,
	method = c("wilson", "wald", "exact")
) {
	type <- match.arg(type)
	method <- match.arg(method)

	assert_data_frame(data, "data")
	assert_character_scalar(outcome_var, "outcome_var")
	assert_character_scalar(trt_var, "trt_var")
	assert_character_scalar(ref_group, "ref_group")
	assert_numeric_scalar(ni_margin, "ni_margin")
	assert_numeric_scalar(conf_level, "conf_level")

	if (
		!is.logical(higher_better) ||
			length(higher_better) != 1 ||
			is.na(higher_better)
	) {
		ph_abort("'higher_better' must be TRUE or FALSE")
	}
	if (is.na(ni_margin) || ni_margin <= 0) {
		ph_abort("'ni_margin' must be a positive number")
	}
	if (conf_level <= 0.5 || conf_level >= 1) {
		ph_abort("'conf_level' must be > 0.5 and < 1")
	}

	assert_column_exists(data, outcome_var, "data")
	assert_column_exists(data, trt_var, "data")

	df <- data[, c(outcome_var, trt_var), drop = FALSE]
	df <- df[!is.na(df[[outcome_var]]) & !is.na(df[[trt_var]]), , drop = FALSE]
	if (nrow(df) == 0) {
		ph_abort("No non-missing observations available for analysis")
	}

	df[[trt_var]] <- as.character(df[[trt_var]])
	if (!ref_group %in% df[[trt_var]]) {
		ph_abort(sprintf(
			"Reference group '%s' not found in '%s'",
			ref_group,
			trt_var
		))
	}

	trt_groups <- setdiff(unique(df[[trt_var]]), ref_group)
	if (length(trt_groups) != 1) {
		ph_abort("Exactly one non-reference treatment group is required")
	}
	trt_group <- trt_groups[1]

	trt_vals <- df[df[[trt_var]] == trt_group, outcome_var]
	ref_vals <- df[df[[trt_var]] == ref_group, outcome_var]

	if (length(trt_vals) == 0 || length(ref_vals) == 0) {
		ph_abort("Both treatment and reference groups must have observations")
	}

	if (type == "continuous") {
		if (!is.numeric(trt_vals) || !is.numeric(ref_vals)) {
			ph_abort("'outcome_var' must be numeric for continuous endpoints")
		}
		if (length(trt_vals) < 2 || length(ref_vals) < 2) {
			ph_abort("Each group must have at least 2 observations")
		}

		alternative <- if (isTRUE(higher_better)) "greater" else "less"
		test <- stats::t.test(
			trt_vals,
			ref_vals,
			alternative = alternative,
			conf.level = conf_level
		)
		estimate <- mean(trt_vals) - mean(ref_vals)
		ci_lower <- test$conf.int[1]
		ci_upper <- test$conf.int[2]
		method_used <- "welch-t"
	} else {
		if (!is.logical(trt_vals) && !is.numeric(trt_vals)) {
			ph_abort("'outcome_var' must be logical or numeric for binary endpoints")
		}
		if (!is.logical(ref_vals) && !is.numeric(ref_vals)) {
			ph_abort("'outcome_var' must be logical or numeric for binary endpoints")
		}

		trt_bin <- if (is.logical(trt_vals)) {
			as.integer(trt_vals)
		} else {
			trt_vals
		}
		ref_bin <- if (is.logical(ref_vals)) {
			as.integer(ref_vals)
		} else {
			ref_vals
		}

		# Check for valid binary values (must be exactly 0 or 1, not values like 0.5)
		valid_trt <- all(trt_bin %in% c(0, 1)) && all(trt_bin == floor(trt_bin))
		valid_ref <- all(ref_bin %in% c(0, 1)) && all(ref_bin == floor(ref_bin))

		if (!valid_trt || !valid_ref) {
			ph_abort(
				"Binary endpoints must be coded as integers 0/1 or logical TRUE/FALSE"
			)
		}

		n_trt <- length(trt_bin)
		n_ref <- length(ref_bin)
		x_trt <- sum(trt_bin)
		x_ref <- sum(ref_bin)
		p_trt <- x_trt / n_trt
		p_ref <- x_ref / n_ref
		estimate <- p_trt - p_ref

		if (method == "wald") {
			z <- stats::qnorm(conf_level)
			se <- sqrt(p_trt * (1 - p_trt) / n_trt + p_ref * (1 - p_ref) / n_ref)
			lower <- estimate - z * se
			upper <- estimate + z * se
		} else {
			# Newcombe-Wilson hybrid CI for difference in proportions
			# (Newcombe RG. Statist. Med. 1998; 17:873-890, Method 10)
			conf_level_two_sided <- 2 * conf_level - 1
			trt_ci <- calculate_proportion_ci(
				x_trt,
				n_trt,
				method = method,
				conf_level = conf_level_two_sided
			)
			ref_ci <- calculate_proportion_ci(
				x_ref,
				n_ref,
				method = method,
				conf_level = conf_level_two_sided
			)
			lower <- estimate -
				sqrt(
					(p_trt - trt_ci$lower)^2 + (ref_ci$upper - p_ref)^2
				)
			upper <- estimate +
				sqrt(
					(trt_ci$upper - p_trt)^2 + (p_ref - ref_ci$lower)^2
				)
		}

		lower <- max(-1, lower)
		upper <- min(1, upper)

		if (isTRUE(higher_better)) {
			ci_lower <- lower
			ci_upper <- Inf
		} else {
			ci_lower <- -Inf
			ci_upper <- upper
		}

		method_used <- method
	}

	if (isTRUE(higher_better)) {
		non_inferior <- ci_lower > -ni_margin
	} else {
		non_inferior <- ci_upper < ni_margin
	}

	conclusion <- if (isTRUE(non_inferior)) {
		"Non-inferiority demonstrated"
	} else {
		"Non-inferiority not demonstrated"
	}

	list(
		estimate = estimate,
		ci_lower = ci_lower,
		ci_upper = ci_upper,
		ni_margin = ni_margin,
		non_inferior = non_inferior,
		conclusion = conclusion,
		method = method_used
	)
}

#' ANCOVA Analysis for Continuous Endpoints
#'
#' Performs ANCOVA to estimate treatment effects adjusted for baseline
#' and other covariates.
#'
#' @param data Data frame with outcome and covariates
#' @param outcome_var Character. Post-baseline outcome variable
#' @param trt_var Character. Treatment variable
#' @param baseline_var Character. Baseline value variable
#' @param covariates Character vector. Additional covariates to adjust for
#' @param ref_group Character. Reference group for contrast. If NULL, the first
#'   level of the treatment variable is used as reference.
#' @param conf_level Numeric. Confidence level (default: 0.95)
#'
#' @return List with:
#'   - treatment_effects: Data frame with adjusted differences vs reference
#'   - model: The fitted lm object
#'   - summary: Model summary
#'   - anova: ANOVA table
#'
#' @details
#' Fits model: outcome ~ baseline + trt + covariates
#' Treatment effects are extracted from model coefficients (trt - ref)
#' and confidence intervals are computed with confint.
#' Missing values are handled by lm() via listwise deletion.
#' Specified variables must exist in the data.
#'
#' @references
#' IQWiG Methods v8.0, Section 10.3.6, p. 218-220.
#'
#' @export
#'
#' @examples
#' # Simulated example
#' set.seed(123)
#' n <- 100
#' sim_data <- data.frame(
#'   TRT01P = rep(c("Placebo", "Drug A"), each = n/2),
#'   BASE = rnorm(n, 50, 10),
#'   CHG = rnorm(n, 5, 8),
#'   AGEGR1 = sample(c("<65", ">=65"), n, replace = TRUE),
#'   SEX = sample(c("F", "M"), n, replace = TRUE)
#' )
#'
#' result <- ancova_adjust_continuous(
#'   data = sim_data,
#'   outcome_var = "CHG",
#'   trt_var = "TRT01P",
#'   baseline_var = "BASE",
#'   ref_group = "Placebo",
#'   covariates = c("AGEGR1", "SEX")
#' )
#' print(result$treatment_effects)
ancova_adjust_continuous <- function(
	data,
	outcome_var,
	trt_var,
	baseline_var,
	covariates = NULL,
	ref_group = NULL,
	conf_level = 0.95
) {
	assert_data_frame(data, "data")
	assert_character_scalar(outcome_var, "outcome_var")
	assert_character_scalar(trt_var, "trt_var")
	assert_character_scalar(baseline_var, "baseline_var")

	if (is.null(covariates)) {
		covariates <- character()
	}
	if (!is.character(covariates)) {
		ph_abort("'covariates' must be a character vector")
	}
	if (
		length(covariates) > 0 &&
			any(is.na(covariates) | nchar(trimws(covariates)) == 0)
	) {
		ph_abort("'covariates' must be a non-empty character vector")
	}
	if (!is.null(ref_group)) {
		assert_character_scalar(ref_group, "ref_group")
	}
	assert_numeric_scalar(conf_level, "conf_level")
	assert_in_range(conf_level, 0, 1, "conf_level")

	covariates <- unique(covariates)
	covariates <- setdiff(covariates, c(outcome_var, trt_var, baseline_var))

	required_cols <- unique(c(outcome_var, trt_var, baseline_var, covariates))
	missing_cols <- setdiff(required_cols, names(data))
	if (length(missing_cols) > 0) {
		ph_abort(sprintf(
			"'data' is missing required columns: %s",
			paste(missing_cols, collapse = ", ")
		))
	}

	df <- data[, required_cols, drop = FALSE]
	df <- df[stats::complete.cases(df), , drop = FALSE]
	if (nrow(df) == 0) {
		ph_abort("No complete cases available for ANCOVA")
	}

	if (!is.numeric(df[[outcome_var]])) {
		ph_abort("'outcome_var' must be numeric")
	}
	if (!is.numeric(df[[baseline_var]])) {
		ph_abort("'baseline_var' must be numeric")
	}

	df[[trt_var]] <- as.factor(df[[trt_var]])
	trt_levels <- levels(df[[trt_var]])
	if (length(trt_levels) < 2) {
		ph_abort("At least two treatment groups are required")
	}

	if (is.null(ref_group)) {
		ref_group <- trt_levels[1]
	} else if (!ref_group %in% trt_levels) {
		ph_abort(sprintf(
			"Reference group '%s' not found in '%s'",
			ref_group,
			trt_var
		))
	}

	df[[trt_var]] <- stats::relevel(df[[trt_var]], ref = ref_group)

	predictors <- c(baseline_var, trt_var, covariates)
	formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
	model <- stats::lm(stats::as.formula(formula_str), data = df)
	model_summary <- summary(model)
	anova_table <- stats::anova(model)

	coef_values <- stats::coef(model)
	conf_int <- stats::confint(model, level = conf_level)

	trt_levels <- levels(df[[trt_var]])
	non_ref_levels <- trt_levels[trt_levels != ref_group]
	expected_terms <- paste0(trt_var, make.names(non_ref_levels))
	if (all(expected_terms %in% names(coef_values))) {
		effect_terms <- expected_terms
		effect_labels <- non_ref_levels
	} else {
		effect_terms <- names(coef_values)[
			grepl(paste0("^", trt_var), names(coef_values))
		]
		effect_labels <- gsub(paste0("^", trt_var), "", effect_terms)
	}

	if (length(effect_terms) == 0) {
		ph_abort("No treatment effects could be extracted from the ANCOVA model")
	}

	treatment_effects <- data.frame(
		Treatment = effect_labels,
		estimate = unname(coef_values[effect_terms]),
		ci_lower = conf_int[effect_terms, 1],
		ci_upper = conf_int[effect_terms, 2],
		stringsAsFactors = FALSE
	)

	list(
		treatment_effects = treatment_effects,
		model = model,
		summary = model_summary,
		anova = anova_table
	)
}

#' Calculate Response Comparison (OR, RR, or RD)
#'
#' @param df Data frame with responder column
#' @param trt_var Treatment variable name
#' @param ref_group Reference group name
#' @param comparison_type "OR", "RR", or "RD"
#' @param conf_level Confidence level
#'
#' @return Named list of comparison results per treatment group
#' @keywords internal
calculate_response_comparison <- function(
	df,
	trt_var,
	ref_group,
	comparison_type,
	conf_level
) {
	alpha <- 1 - conf_level
	z <- stats::qnorm(1 - alpha / 2)

	trt_levels <- levels(df[[trt_var]])
	other_trts <- setdiff(trt_levels, ref_group)

	# Reference group stats
	ref_data <- df[df[[trt_var]] == ref_group, ]
	n_ref <- nrow(ref_data)
	x_ref <- sum(ref_data$responder)
	p_ref <- x_ref / n_ref

	results <- list()

	for (trt in other_trts) {
		trt_data <- df[df[[trt_var]] == trt, ]
		n_trt <- nrow(trt_data)
		x_trt <- sum(trt_data$responder)
		p_trt <- x_trt / n_trt

		if (comparison_type == "OR") {
			# Odds Ratio via logistic regression
			df_subset <- df[df[[trt_var]] %in% c(ref_group, trt), ]
			df_subset[[trt_var]] <- stats::relevel(
				factor(df_subset[[trt_var]]),
				ref = ref_group
			)

			glm_fit <- stats::glm(
				stats::reformulate(trt_var, response = "responder"),
				data = df_subset,
				family = stats::binomial()
			)
			coef_idx <- 2
			or <- exp(stats::coef(glm_fit)[coef_idx])
			or_ci <- exp(stats::confint.default(glm_fit, level = conf_level)[
				coef_idx,
			])
			pval <- summary(glm_fit)$coefficients[coef_idx, "Pr(>|z|)"]

			results[[trt]] <- list(
				estimate = or,
				lcl = or_ci[1],
				ucl = or_ci[2],
				pvalue = pval
			)
		} else if (comparison_type == "RR") {
			# Risk Ratio (approximate)
			# Check for extreme rates that require continuity correction
			extreme_rates <- p_ref == 0 || p_ref == 1 || p_trt == 0 || p_trt == 1
			continuity_applied <- FALSE

			if (extreme_rates) {
				# Apply Haldane continuity correction: add 0.5 to counts
				x_trt_adj <- x_trt + 0.5
				x_ref_adj <- x_ref + 0.5
				n_trt_adj <- n_trt + 1
				n_ref_adj <- n_ref + 1
				p_trt_calc <- x_trt_adj / n_trt_adj
				p_ref_calc <- x_ref_adj / n_ref_adj
				continuity_applied <- TRUE
			} else {
				p_trt_calc <- p_trt
				p_ref_calc <- p_ref
			}

			rr <- p_trt_calc / p_ref_calc
			log_rr <- log(rr)
			# Use adjusted sample sizes when continuity correction was applied
			n_trt_use <- if (continuity_applied) n_trt_adj else n_trt
			n_ref_use <- if (continuity_applied) n_ref_adj else n_ref
			se_log_rr <- sqrt(
				(1 - p_trt_calc) /
					(n_trt_use * p_trt_calc) +
					(1 - p_ref_calc) / (n_ref_use * p_ref_calc)
			)
			rr_ci <- exp(log_rr + c(-1, 1) * z * se_log_rr)

			# Chi-square test for p-value (use Fisher's exact for extreme cases)
			cont_table <- matrix(
				c(x_trt, n_trt - x_trt, x_ref, n_ref - x_ref),
				nrow = 2
			)
			if (extreme_rates || any(cont_table < 5)) {
				pval <- stats::fisher.test(cont_table)$p.value
			} else {
				pval <- stats::chisq.test(cont_table)$p.value
			}

			results[[trt]] <- list(
				estimate = rr,
				lcl = rr_ci[1],
				ucl = rr_ci[2],
				pvalue = pval,
				continuity_correction = continuity_applied
			)
		} else {
			# Risk Difference
			rd <- p_trt - p_ref
			se_rd <- sqrt(p_trt * (1 - p_trt) / n_trt + p_ref * (1 - p_ref) / n_ref)
			rd_ci <- rd + c(-1, 1) * z * se_rd

			# Handle zero SE case to avoid NaN/Inf
			if (se_rd < 1e-10) {
				z_stat <- if (abs(rd) < 1e-10) 0 else sign(rd) * Inf
				pval <- if (abs(rd) < 1e-10) 1 else 0
			} else {
				z_stat <- rd / se_rd
				pval <- 2 * stats::pnorm(-abs(z_stat))
			}

			results[[trt]] <- list(
				estimate = rd,
				lcl = rd_ci[1],
				ucl = rd_ci[2],
				pvalue = pval
			)
		}
	}

	results
}

#' Calculate Proportion Confidence Interval
#'
#' @param x Number of successes
#' @param n Total number of trials
#' @param method CI method: "wilson", "exact", or "wald"
#' @param conf_level Confidence level
#'
#' @return List with lower and upper bounds
#' @keywords internal
calculate_proportion_ci <- function(
	x,
	n,
	method = "wilson",
	conf_level = 0.95
) {
	alpha <- 1 - conf_level
	p <- x / n

	if (method == "wilson") {
		# Wilson score interval
		z <- stats::qnorm(1 - alpha / 2)
		denom <- 1 + z^2 / n
		center <- (p + z^2 / (2 * n)) / denom
		margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
		list(lower = max(0, center - margin), upper = min(1, center + margin))
	} else if (method == "exact") {
		# Clopper-Pearson exact
		result <- stats::binom.test(x, n, conf.level = conf_level)
		list(lower = result$conf.int[1], upper = result$conf.int[2])
	} else {
		# Wald interval
		se <- sqrt(p * (1 - p) / n)
		z <- stats::qnorm(1 - alpha / 2)
		list(lower = max(0, p - z * se), upper = min(1, p + z * se))
	}
}
