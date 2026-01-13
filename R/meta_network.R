#' @title Network Meta-Analysis Functions
#' @name meta_network
#' @description Functions for performing network meta-analysis, including
#'   rankings and league tables.
NULL

.extract_pairwise_data <- function(df, t1, t2) {
	idx <- (df$treat1 == t1 & df$treat2 == t2) |
		(df$treat1 == t2 & df$treat2 == t1)
	if (!any(idx, na.rm = TRUE)) {
		return(NULL)
	}
	studies <- df[idx, , drop = FALSE]
	effects <- ifelse(studies$treat1 == t1, studies$effect, -studies$effect)
	list(
		effects = effects,
		ses = studies$se,
		n_studies = nrow(studies)
	)
}

.pool_pairwise_effects <- function(effects, ses, model = c("fixed", "random")) {
	model <- match.arg(model)
	if (length(effects) != length(ses)) {
		ph_abort("effects and ses must have the same length")
	}
	keep <- is.finite(effects) &
		!is.na(effects) &
		is.finite(ses) &
		!is.na(ses) &
		ses > 0
	if (!all(keep)) {
		ph_warn(sprintf(
			"Dropping %d row(s) with missing/invalid effect or SE",
			sum(!keep)
		))
	}
	effects <- effects[keep]
	ses <- ses[keep]
	if (length(effects) == 0) {
		ph_abort("No valid effect/SE values available")
	}
	if (length(effects) == 1) {
		return(list(estimate = effects[1], se = ses[1], tau2 = 0))
	}

	wi_fe <- 1 / (ses^2)
	theta_fe <- sum(wi_fe * effects) / sum(wi_fe)
	se_fe <- sqrt(1 / sum(wi_fe))

	if (model == "fixed") {
		return(list(estimate = theta_fe, se = se_fe, tau2 = 0))
	}

	Q <- sum(wi_fe * (effects - theta_fe)^2)
	df <- length(effects) - 1
	tau2 <- estimate_tau2(effects, ses, "DL", wi_fe, theta_fe, Q, df)
	wi_re <- 1 / (ses^2 + tau2)
	theta_re <- sum(wi_re * effects) / sum(wi_re)
	se_re <- sqrt(1 / sum(wi_re))

	list(estimate = theta_re, se = se_re, tau2 = tau2)
}

.check_network_connectivity <- function(edges, treatments) {
	if (length(treatments) == 0) {
		return(FALSE)
	}

	adj <- stats::setNames(vector("list", length(treatments)), treatments)
	for (i in seq_len(nrow(edges))) {
		a <- as.character(edges$treat1[i])
		b <- as.character(edges$treat2[i])
		adj[[a]] <- unique(c(adj[[a]], b))
		adj[[b]] <- unique(c(adj[[b]], a))
	}

	start <- treatments[1]
	visited <- stats::setNames(rep(FALSE, length(treatments)), treatments)
	queue <- start
	visited[[start]] <- TRUE
	while (length(queue) > 0) {
		v <- queue[1]
		queue <- queue[-1]
		neighbors <- adj[[v]]
		for (n in neighbors) {
			if (!isTRUE(visited[[n]])) {
				visited[[n]] <- TRUE
				queue <- c(queue, n)
			}
		}
	}

	all(visited)
}

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
#'   "graph" (not yet implemented, defaults to bucher)
#' @param conf_level Numeric. Confidence level. Default: 0.95
#'
#' @details
#' For ratio measures ("hr", "or", "rr"), effect estimates must be on
#' the **log scale**. For difference measures ("rd", "md", "smd"), effect
#' estimates must be on the **raw scale**.
#'
#' Indirect effects are estimated using a single-step Bucher chain via one
#' intermediate treatment (chosen to minimize the indirect standard error).
#' Multiple indirect paths are not combined.
#'
#' @return List with relative effects, rankings, and network structure
#' @export
#'
#' @examples
#' # Network meta-analysis of 4 studies
#' nma_data <- data.frame(
#'   study = c("S1", "S2", "S3", "S4"),
#'   treat1 = c("A", "B", "A", "B"),
#'   treat2 = c("B", "C", "C", "D"),
#'   effect = log(c(0.75, 0.90, 0.80, 0.85)),
#'   se = c(0.12, 0.15, 0.18, 0.14)
#' )
#' result <- network_meta(nma_data, effect_measure = "hr")
#' result@comparisons
#' result@network$treatments
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

	resolved_method <- if (method == "bucher") {
		"bucher_chain"
	} else {
		ph_warn("method='graph' not yet implemented; using method='bucher'")
		"bucher_chain"
	}

	admiraldev::assert_data_frame(data)

	# Validate required columns
	required_cols <- c(study_var, treat1_var, treat2_var, effect_var, se_var)
	missing_cols <- setdiff(required_cols, names(data))
	if (length(missing_cols) > 0) {
		ph_abort(sprintf(
			"Missing required columns: %s",
			paste(missing_cols, collapse = ", ")
		))
	}

	# Check for self-comparisons
	self_comp <- data[[treat1_var]] == data[[treat2_var]]
	if (any(self_comp, na.rm = TRUE)) {
		ph_abort("Self-comparisons (treat1 == treat2) are not allowed")
	}

	# Rename columns for internal use
	df <- tibble::tibble(
		study = data[[study_var]],
		treat1 = data[[treat1_var]],
		treat2 = data[[treat2_var]],
		effect = data[[effect_var]],
		se = data[[se_var]]
	)

	# Validate numeric columns
	if (!is.numeric(df$effect)) {
		ph_abort(sprintf("Column '%s' must be numeric", effect_var))
	}
	if (!is.numeric(df$se)) {
		ph_abort(sprintf("Column '%s' must be numeric", se_var))
	}

	# Get all treatments
	treatments <- sort(unique(c(df$treat1, df$treat2)))
	n_treatments <- length(treatments)

	if (is.null(reference)) {
		reference <- treatments[1]
	}

	if (!reference %in% treatments) {
		ph_abort(sprintf("Reference treatment '%s' not in network", reference))
	}

	# Build network structure (treatments are undirected edges)
	edges <- tibble::tibble(
		treat1 = pmin(df$treat1, df$treat2),
		treat2 = pmax(df$treat1, df$treat2)
	)
	edges <- unique(edges)
	edges$n_studies <- purrr::map_int(seq_len(nrow(edges)), function(i) {
		a <- edges$treat1[i]
		b <- edges$treat2[i]
		sum(
			(df$treat1 == a & df$treat2 == b) |
				(df$treat1 == b & df$treat2 == a)
		)
	})

	if (!.check_network_connectivity(edges, treatments)) {
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

			# Warn if NAs detected
			if (anyNA(effects) || anyNA(ses)) {
				ph_warn(sprintf(
					"NA values detected in comparison %s vs %s",
					reference,
					trt
				))
			}

			pooled <- .pool_pairwise_effects(effects, ses, model = model)
			est <- pooled$estimate
			se <- pooled$se

			direct_results[[trt]] <- list(
				estimate = est,
				se = se,
				n_studies = nrow(studies),
				evidence = "direct",
				tau2 = pooled$tau2
			)
		}
	}

	# For treatments without direct comparison to reference, use indirect
	indirect_results <- list()
	for (trt in setdiff(treatments, c(reference, names(direct_results)))) {
		# Try to find indirect path through another treatment. Use the bridge
		# with the smallest indirect SE (single-step Bucher chain).
		best <- NULL
		for (bridge in names(direct_results)) {
			bridge_studies <- df[
				(df$treat1 == bridge & df$treat2 == trt) |
					(df$treat1 == trt & df$treat2 == bridge),
			]
			if (nrow(bridge_studies) == 0) {
				next
			}

			# Effect of bridge vs trt
			effects <- ifelse(
				bridge_studies$treat1 == bridge,
				bridge_studies$effect,
				-bridge_studies$effect
			)
			ses <- bridge_studies$se
			pooled_bridge <- .pool_pairwise_effects(effects, ses, model = model)
			bridge_est <- pooled_bridge$estimate
			bridge_se <- pooled_bridge$se

			# Indirect: ref vs trt = (ref vs bridge) + (bridge vs trt)
			ref_bridge <- direct_results[[bridge]]
			ind_est <- ref_bridge$estimate + bridge_est
			ind_se <- sqrt(ref_bridge$se^2 + bridge_se^2)

			candidate <- list(
				estimate = ind_est,
				se = ind_se,
				n_studies = ref_bridge$n_studies + nrow(bridge_studies),
				evidence = "indirect",
				via = bridge
			)
			if (is.null(best) || candidate$se < best$se) {
				best <- candidate
			}
		}
		if (!is.null(best)) {
			indirect_results[[trt]] <- best
		}
	}

	# Combine all results
	all_results <- c(direct_results, indirect_results)

	# Create comparison table
	is_ratio <- effect_measure %in% c("hr", "or", "rr")
	alpha <- 1 - conf_level
	z <- stats::qnorm(1 - alpha / 2)

	comparison_table <- tibble::tibble(
		treatment = names(all_results),
		vs = reference,
		estimate = purrr::map_dbl(all_results, function(x) {
			if (is_ratio) exp(x$estimate) else x$estimate
		}),
		ci_lower = purrr::map_dbl(all_results, function(x) {
			val <- x$estimate - z * x$se
			if (is_ratio) exp(val) else val
		}),
		ci_upper = purrr::map_dbl(all_results, function(x) {
			val <- x$estimate + z * x$se
			if (is_ratio) exp(val) else val
		}),
		se = purrr::map_dbl(all_results, function(x) x$se),
		n_studies = purrr::map_int(all_results, function(x) x$n_studies),
		evidence = purrr::map_chr(all_results, function(x) x$evidence)
	)

	# Lower is typically better (HR/OR/RR < 1, or negative differences)
	# For outcomes where higher is better, users should interpret accordingly
	comparison_table$rank <- rank(comparison_table$estimate)

	# Add reference
	ref_row <- tibble::tibble(
		treatment = reference,
		vs = reference,
		estimate = if (is_ratio) 1 else 0,
		ci_lower = if (is_ratio) 1 else 0,
		ci_upper = if (is_ratio) 1 else 0,
		se = 0,
		n_studies = NA_integer_,
		evidence = "reference",
		rank = NA_integer_
	)
	comparison_table <- dplyr::bind_rows(ref_row, comparison_table)

	NMAResult(
		comparisons = comparison_table,
		network = list(
			treatments = treatments,
			n_treatments = n_treatments,
			edges = edges,
			reference = reference
		),
		model = model,
		effect_measure = effect_measure,
		conf_level = conf_level,
		method = resolved_method,
		n_studies = nrow(df),
		metadata = list(
			data = df,
			input = list(
				study_var = study_var,
				treat1_var = treat1_var,
				treat2_var = treat2_var,
				effect_var = effect_var,
				se_var = se_var
			),
			method_requested = method,
			method_resolved = resolved_method
		)
	)
}

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
#' @examples
#' # Node-splitting for inconsistency testing
#' nma_data <- data.frame(
#'   study = c("S1", "S2", "S3"),
#'   treat1 = c("A", "B", "A"),
#'   treat2 = c("B", "C", "C"),
#'   effect = log(c(0.75, 0.90, 0.80)),
#'   se = c(0.12, 0.15, 0.18)
#' )
#' nma_result <- network_meta(nma_data, effect_measure = "hr")
#' ns_result <- node_splitting(nma_result)
#' ns_result$note
node_splitting <- function(
	nma_result,
	data = NULL,
	conf_level = 0.95
) {
	if (!S7::S7_inherits(nma_result, NMAResult)) {
		ph_abort("nma_result must be an NMAResult object from network_meta()")
	}

	network <- nma_result@network
	edges <- network$edges
	reference <- network$reference
	effect_measure <- nma_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")
	model <- nma_result@model

	if (is.null(data)) {
		data <- nma_result@metadata$data
	}
	if (is.null(data) || !is.data.frame(data)) {
		ph_abort(paste0(
			"node_splitting requires the original NMA data: pass `data=` ",
			"or use an NMAResult created by network_meta()"
		))
	}
	if (!all(c("study", "treat1", "treat2", "effect", "se") %in% names(data))) {
		ph_abort("data must contain columns: study, treat1, treat2, effect, se")
	}

	df <- tibble::as_tibble(data)
	alpha <- 1 - conf_level
	z <- stats::qnorm(1 - alpha / 2)

	# For each comparison with both direct and indirect evidence
	results <- list()

	for (i in seq_len(nrow(edges))) {
		t1 <- edges$treat1[i]
		t2 <- edges$treat2[i]
		n_direct <- edges$n_studies[i]

		if (n_direct < 1) {
			next
		}

		pair_direct <- .extract_pairwise_data(df, t1, t2)
		if (is.null(pair_direct)) {
			next
		}
		direct_pooled <- .pool_pairwise_effects(
			pair_direct$effects,
			pair_direct$ses,
			model = model
		)
		direct_est <- direct_pooled$estimate
		direct_se <- direct_pooled$se

		best_ind <- NULL
		for (bridge in setdiff(network$treatments, c(t1, t2))) {
			pair_1 <- .extract_pairwise_data(df, t1, bridge)
			pair_2 <- .extract_pairwise_data(df, bridge, t2)
			if (is.null(pair_1) || is.null(pair_2)) {
				next
			}
			p1 <- .pool_pairwise_effects(pair_1$effects, pair_1$ses, model = model)
			p2 <- .pool_pairwise_effects(pair_2$effects, pair_2$ses, model = model)
			ind_est <- p1$estimate + p2$estimate
			ind_se <- sqrt(p1$se^2 + p2$se^2)
			if (is.null(best_ind) || ind_se < best_ind$se) {
				best_ind <- list(
					estimate = ind_est,
					se = ind_se,
					bridge = bridge,
					n_indirect = pair_1$n_studies + pair_2$n_studies
				)
			}
		}

		if (!is.null(best_ind)) {
			direct_ci <- direct_est + c(-1, 1) * z * direct_se
			ind_ci <- best_ind$estimate + c(-1, 1) * z * best_ind$se
			if (is_ratio) {
				direct_est_disp <- exp(direct_est)
				direct_ci_disp <- exp(direct_ci)
				ind_est_disp <- exp(best_ind$estimate)
				ind_ci_disp <- exp(ind_ci)
			} else {
				direct_est_disp <- direct_est
				direct_ci_disp <- direct_ci
				ind_est_disp <- best_ind$estimate
				ind_ci_disp <- ind_ci
			}

			z_incon <- (direct_est - best_ind$estimate) /
				sqrt(direct_se^2 + best_ind$se^2)
			p_incon <- 2 * stats::pnorm(-abs(z_incon))

			results[[paste(t1, t2, sep = "_vs_")]] <- tibble::tibble(
				comparison = paste(t1, "vs", t2),
				reference = reference,
				direct_estimate = direct_est_disp,
				direct_se = direct_se,
				direct_ci_lower = direct_ci_disp[1],
				direct_ci_upper = direct_ci_disp[2],
				n_direct = n_direct,
				indirect_estimate = ind_est_disp,
				indirect_se = best_ind$se,
				indirect_ci_lower = ind_ci_disp[1],
				indirect_ci_upper = ind_ci_disp[2],
				n_indirect = best_ind$n_indirect,
				bridge = best_ind$bridge,
				inconsistency_z = z_incon,
				inconsistency_p = p_incon,
				model = model,
				conf_level = conf_level
			)
		}
	}

	if (length(results) == 0) {
		return(list(
			results = tibble::tibble(
				comparison = character(),
				direct_estimate = numeric()
			),
			effect_measure = effect_measure,
			note = "No comparisons with both direct and indirect evidence"
		))
	}

	result_df <- dplyr::bind_rows(results)

	list(
		results = result_df,
		effect_measure = effect_measure,
		note = paste0(
			"Full node-splitting requires re-analysis excluding ",
			"direct evidence. Results shown are simplified."
		)
	)
}

#'
#' Calculates ranking probabilities and SUCRA (Surface Under Cumulative
#' Ranking curve) or P-scores for treatments in network meta-analysis.
#'
#' @param nma_result Result from network_meta()
#' @param lower_better Logical. Is lower estimate better?
#'   Default: TRUE for ratios
#' @param n_sim Integer. Number of simulations for ranking. Default: 1000
#' @param seed Integer or NULL. Random seed for reproducibility. Default: 42.
#'   Set to NULL for non-deterministic results.
#'
#' @details
#' The reference treatment is treated as the baseline in the output and is
#' assigned an SE of 0. For any non-reference treatments with missing/invalid
#' standard errors, the SE is imputed using the median of valid SE values.
#' This imputation can affect ranking probabilities and SUCRA values.
#'
#' @return List with rankings, SUCRA/P-scores, and rankogram data. When
#'   `n_treatments == 1`, the returned `sucra` value is set to `1.0` (the single
#'   treatment is trivially best).
#' @note RNG state is isolated via `withr::with_seed` during simulation,
#'   so calling this function does not alter the global RNG state.
#' @export
#' @examples
#' # Calculate SUCRA rankings
#' nma_data <- data.frame(
#'   study = c("S1", "S2", "S3"),
#'   treat1 = c("A", "B", "A"),
#'   treat2 = c("B", "C", "C"),
#'   effect = log(c(0.75, 0.90, 0.80)),
#'   se = c(0.12, 0.15, 0.18)
#' )
#' nma_result <- network_meta(nma_data, effect_measure = "hr")
#' sucra <- calculate_sucra(nma_result)
#' sucra$ranking
#' sucra$interpretation
calculate_sucra <- function(
	nma_result,
	lower_better = NULL,
	n_sim = 1000,
	seed = 42
) {
	if (!S7::S7_inherits(nma_result, NMAResult)) {
		ph_abort("nma_result must be an NMAResult object from network_meta()")
	}

	comparisons <- nma_result@comparisons
	effect_measure <- nma_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")

	# Default direction
	if (is.null(lower_better)) {
		lower_better <- is_ratio # Lower HR/OR/RR is typically better
	}

	treatments <- comparisons$treatment
	n_treat <- length(treatments)
	estimates <- comparisons$estimate
	ses <- comparisons$se

	# Handle missing/invalid SEs.
	# Keep the reference treatment SE at 0 (it is the baseline in our output);
	# for any other treatments with missing/zero SE, impute from observed SEs.
	ref_trt <- nma_result@network$reference
	ref_idx <- which(treatments == ref_trt)
	if (length(ref_idx) == 1) {
		ses[ref_idx] <- 0
	}

	bad_idx <- setdiff(
		which(is.na(ses) | !is.finite(ses) | ses <= 0),
		ref_idx
	)
	if (length(bad_idx) > 0) {
		pos_ses <- ses[ses > 0 & is.finite(ses)]
		# Fallback when pos_ses is empty: use a small, finite epsilon.
		imputed_se <- if (length(pos_ses) > 0) {
			stats::median(pos_ses)
		} else {
			sqrt(.Machine$double.eps)
		}
		ses[bad_idx] <- imputed_se
		ph_warn(sprintf(
			"Imputed missing/invalid SE(s) with %.6g for: %s",
			imputed_se,
			paste(treatments[bad_idx], collapse = ", ")
		))
	}

	# Simulate rankings
	rank_matrix <- matrix(0, nrow = n_sim, ncol = n_treat)
	colnames(rank_matrix) <- treatments

	simulate_ranks <- function() {
		rank_matrix_local <- rank_matrix
		for (sim in seq_len(n_sim)) {
			# Sample from normal distribution
			sampled <- stats::rnorm(n_treat, mean = estimates, sd = ses)

			# Rank (1 = best)
			if (lower_better) {
				ranks <- rank(sampled)
			} else {
				ranks <- rank(-sampled)
			}

			rank_matrix_local[sim, ] <- ranks
		}
		rank_matrix_local
	}

	rank_matrix <- if (!is.null(seed)) {
		withr::with_seed(seed, simulate_ranks())
	} else {
		simulate_ranks()
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

	# Handle degenerate case of single treatment
	if (n_treat == 1) {
		sucra <- 1.0 # Single treatment is trivially best
	} else {
		for (i in seq_len(n_treat)) {
			cum_probs <- cumsum(rank_probs[i, 1:(n_treat - 1)])
			sucra[i] <- mean(cum_probs)
		}
	}

	# Mean rank
	mean_rank <- colMeans(rank_matrix)

	# Create ranking summary
	# Note: ranking is ordinal based on estimate magnitude.
	# For ratio measures (HR/OR/RR), lower is typically better.
	# For difference measures, interpretation depends on outcome direction.
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

#'
#' Generates a league table showing all pairwise treatment comparisons
#' from a network meta-analysis.
#'
#' @param nma_result Result from network_meta()
#' @param digits Integer. Decimal places for estimates. Default: 2
#' @param show_ci Logical. Show confidence intervals. Default: TRUE
#' @param highlight_sig Logical. Highlight significant comparisons.
#'   Default: TRUE
#' @param conf_level Numeric. Confidence level. Default: 0.95
#'
#' @return ClinicalTable with league table matrix
#' @export
#' @examples
#' # Create league table for NMA
#' nma_data <- data.frame(
#'   study = c("S1", "S2", "S3"),
#'   treat1 = c("A", "B", "A"),
#'   treat2 = c("B", "C", "C"),
#'   effect = log(c(0.75, 0.90, 0.80)),
#'   se = c(0.12, 0.15, 0.18)
#' )
#' nma_result <- network_meta(nma_data, effect_measure = "hr")
#' table <- create_league_table(nma_result)
#' table@type
create_league_table <- function(
	nma_result,
	digits = 2,
	show_ci = TRUE,
	highlight_sig = TRUE,
	conf_level = 0.95
) {
	if (!S7::S7_inherits(nma_result, NMAResult)) {
		ph_abort("nma_result must be an NMAResult object from network_meta()")
	}

	# Use NMA result's confidence level if available
	if (!is.null(nma_result@conf_level)) {
		conf_level <- nma_result@conf_level
	}

	comparisons <- nma_result@comparisons
	treatments <- nma_result@network$treatments
	n_treat <- length(treatments)
	effect_measure <- nma_result@effect_measure
	is_ratio <- effect_measure %in% c("hr", "or", "rr")
	null_value <- if (is_ratio) 1 else 0

	# Calculate z-score for confidence level
	z <- stats::qnorm(1 - (1 - conf_level) / 2)

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

	ref <- nma_result@network$reference
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

	# Calculate dynamic SE floor based on data
	positives <- se_vs_ref[se_vs_ref > 0 & is.finite(se_vs_ref)]
	min_se <- if (length(positives) > 0) min(positives) else NA_real_
	se_floor <- if (!is.na(min_se)) min_se / 100 else sqrt(.Machine$double.eps)

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
				se_row <- if (is.na(se_row) || se_row == 0) se_floor else se_row
				se_col <- if (is.na(se_col) || se_col == 0) se_floor else se_col
				se_diff <- sqrt(se_row^2 + se_col^2)

				ci_lower <- exp(log_diff - z * se_diff)
				ci_upper <- exp(log_diff + z * se_diff)
			} else {
				est_row <- est_vs_ref[t_row]
				est_col <- est_vs_ref[t_col]
				estimate <- est_row - est_col

				se_row <- se_vs_ref[t_row]
				se_col <- se_vs_ref[t_col]
				se_row <- if (is.na(se_row) || se_row == 0) se_floor else se_row
				se_col <- if (is.na(se_col) || se_col == 0) se_floor else se_col
				se_diff <- sqrt(se_row^2 + se_col^2)

				ci_lower <- estimate - z * se_diff
				ci_upper <- estimate + z * se_diff
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
	league_df <- tibble::as_tibble(league_matrix, rownames = "Treatment")

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
