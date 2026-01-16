# Tests for bias-adjusted meta-analysis functions (R/meta_bias_adjusted.R)

# =============================================================================
# calculate_rob_weights function tests
# =============================================================================

test_that("calculate_rob_weights returns list with required components", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 5", "Low", "Low", "Low", "Some concerns", "Low")
	)

	rob_weights <- calculate_rob_weights(meta_res, rob_results)

	expect_true(is.list(rob_weights))
	expect_true("weights" %in% names(rob_weights))
	expect_true("study_ids" %in% names(rob_weights))
	expect_true("rob_judgments" %in% names(rob_weights))
	expect_true("multipliers" %in% names(rob_weights))
	expect_true("method" %in% names(rob_weights))
})

test_that("calculate_rob_weights applies correct multipliers for RoB 2 Low", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	rob_weights <- calculate_rob_weights(meta_res, rob_results)

	expect_true(rob_weights$multipliers["Study 1"] == 1)
})

test_that("calculate_rob_weights applies multipliers for RoB 2 concerns", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 2", "Low", "Low", "Low", "Some concerns", "Low")
	)

	rob_weights <- calculate_rob_weights(
		meta_res,
		rob_results,
		weight_concerns = 0.5
	)

	expect_true(rob_weights$multipliers["Study 2"] == 0.5)
})

test_that("calculate_rob_weights applies correct multipliers for RoB 2 High", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low")
	)

	rob_weights <- calculate_rob_weights(meta_res, rob_results, weight_high = 0)

	expect_true(rob_weights$multipliers["Study 3"] == 0)
})

test_that("calculate_rob_weights applies correct multipliers for ROBINS-I", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_robins_i("Study 1", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i(
			"Study 2",
			"Moderate",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i(
			"Study 3",
			"Serious",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i(
			"Study 4",
			"Critical",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i("Study 5", "Low", "Low", "Low", "Low", "Low", "Low", "Low")
	)

	rob_weights <- calculate_rob_weights(
		meta_res,
		rob_results,
		weight_moderate = 0.75,
		weight_serious = 0.25,
		weight_critical = 0
	)

	expect_true(rob_weights$multipliers["Study 1"] == 1)
	expect_true(rob_weights$multipliers["Study 2"] == 0.75)
	expect_true(rob_weights$multipliers["Study 3"] == 0.25)
	expect_true(rob_weights$multipliers["Study 4"] == 0)
})

test_that(
	paste0(
		"calculate_rob_weights returns correct multipliers ",
		"for ROBINS-I judgments"
	),
	{
		meta_res <- meta_analysis(
			yi = .meta_yi_hr,
			sei = .meta_sei_hr,
			study_labels = paste("Study", 1:5),
			effect_measure = "hr"
		)

		rob_results <- list(
			assess_robins_i(
				"Study 1",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low"
			),
			assess_robins_i(
				"Study 2",
				"Moderate",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low"
			),
			assess_robins_i(
				"Study 3",
				"Serious",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low"
			),
			assess_robins_i(
				"Study 4",
				"Critical",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low"
			),
			assess_robins_i(
				"Study 5",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low",
				"Low"
			)
		)

		rob_weights <- calculate_rob_weights(
			meta_res,
			rob_results,
			weight_moderate = 0.75,
			weight_serious = 0.25,
			weight_critical = 0
		)

		expect_true(rob_weights$multipliers["Study 1"] == 1)
		expect_true(rob_weights$multipliers["Study 2"] == 0.75)
		expect_true(rob_weights$multipliers["Study 3"] == 0.25)
		expect_true(rob_weights$multipliers["Study 4"] == 0)
	}
)

test_that("calculate_rob_weights normalizes ROB2 weights to sum to 1", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 5", "Low", "Low", "Low", "Low", "Low")
	)

	rob_weights <- calculate_rob_weights(meta_res, rob_results)

	expect_equal(sum(rob_weights$weights), 1, tolerance = 0.001)
})

test_that("calculate_rob_weights handles unassessed studies", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low")
	)

	rob_weights <- calculate_rob_weights(meta_res, rob_results)

	expect_equal(unname(rob_weights$rob_judgments["Study 1"]), "Low")
	expect_equal(unname(rob_weights$rob_judgments["Study 2"]), "Low")
	expect_equal(unname(rob_weights$rob_judgments["Study 3"]), "Not assessed")
	expect_equal(unname(rob_weights$multipliers["Study 3"]), 1)
})

test_that("calculate_rob_weights warns when all weights are zero", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 1", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 4", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 5", "High", "Low", "Low", "Low", "Low")
	)

	expect_warning(
		calculate_rob_weights(
			meta_res,
			rob_results,
			weight_high = 0,
			weight_concerns = 0
		),
		"All adjusted weights"
	)
})

test_that("calculate_rob_weights errors with invalid meta_result", {
	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	expect_error(
		calculate_rob_weights(list(), rob_results),
		"meta_result must be a MetaResult object"
	)
})

test_that("calculate_rob_weights errors with empty rob_results", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		effect_measure = "hr"
	)

	expect_error(
		calculate_rob_weights(meta_res, list()),
		"non-empty list"
	)
})

test_that("calculate_rob_weights errors with invalid RoB result", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		list(study_id = "Study 1", overall = "Low")
	)

	expect_error(
		calculate_rob_weights(meta_res, rob_results),
		"RoB2Result or ROBINSIResult"
	)
})

# =============================================================================
# rob_sensitivity_analysis function tests
# =============================================================================

test_that("rob_sensitivity_analysis returns list with required components", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 5", "Low", "Low", "Low", "Some concerns", "Low")
	)

	sensitivity <- rob_sensitivity_analysis(meta_res, rob_results)

	expect_true("All studies" %in% sensitivity$results$scenario)
})

test_that("rob_sensitivity_analysis runs Low risk only scenario for RoB 2", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low")
	)

	sensitivity <- rob_sensitivity_analysis(meta_res, rob_results)

	expect_true("Low risk only" %in% sensitivity$results$scenario)
})

test_that("rob_sensitivity_analysis runs Low + Some concerns scenario", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Some concerns", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low")
	)

	sensitivity <- rob_sensitivity_analysis(meta_res, rob_results)

	expect_true("Low + Some concerns" %in% sensitivity$results$scenario)
})

test_that("rob_sensitivity_analysis runs Excluding High risk scenario", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	sensitivity <- rob_sensitivity_analysis(meta_res, rob_results)

	expect_true("Excluding High risk" %in% sensitivity$results$scenario)
})

test_that("rob_sensitivity_analysis works with ROBINS-I assessments", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_robins_i("Study 1", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i(
			"Study 2",
			"Moderate",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i(
			"Study 3",
			"Serious",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i("Study 4", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i("Study 5", "Low", "Low", "Low", "Low", "Low", "Low", "Low")
	)

	expect_warning(
		sensitivity <- rob_sensitivity_analysis(meta_res, rob_results),
		"very low degrees of freedom"
	)

	expect_true("All studies" %in% sensitivity$results$scenario)
	expect_true("Low risk only" %in% sensitivity$results$scenario)
	expect_true("Low + Moderate" %in% sensitivity$results$scenario)
	expect_true("Excluding Serious/Critical" %in% sensitivity$results$scenario)
})

test_that("rob_sensitivity_analysis calculates percent change from original", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	expect_warning(
		sensitivity <- rob_sensitivity_analysis(meta_res, rob_results),
		"very low degrees of freedom"
	)

	expect_true("pct_change_from_original" %in% names(sensitivity$results))
})

test_that("rob_sensitivity_analysis handles insufficient studies", {
	meta_res <- meta_analysis(
		yi = c(0.5, 0.8),
		sei = c(0.1, 0.2),
		effect_measure = "md"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	sensitivity <- rob_sensitivity_analysis(meta_res, rob_results)

	expect_true("Low risk only" %in% sensitivity$results$scenario)
	expect_true(is.na(sensitivity$results$estimate[
		sensitivity$results$scenario == "Low risk only"
	]))
	expect_true("Insufficient studies" %in% sensitivity$results$warning)
})

test_that("rob_sensitivity_analysis errors with invalid meta_result", {
	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	expect_error(
		rob_sensitivity_analysis(list(), rob_results),
		"meta_result must be a MetaResult object"
	)
})

test_that("rob_sensitivity_analysis errors with empty rob_results", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		effect_measure = "hr"
	)

	expect_error(
		rob_sensitivity_analysis(meta_res, list()),
		"non-empty list"
	)
})

# =============================================================================
# bias_adjusted_meta function tests
# =============================================================================

test_that("bias_adjusted_meta returns BiasAdjustedMetaResult", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 5", "Low", "Low", "Low", "Some concerns", "Low")
	)

	adjusted <- bias_adjusted_meta(
		meta_res,
		rob_results,
		method = "weight_downgrade"
	)

	expect_true(inherits(adjusted, "BiasAdjustedMetaResult"))
	# Also verify S7 base class
	expect_s7_class(adjusted, MetaResult)
})

test_that("bias_adjusted_meta with weight_downgrade adjusts weights", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Some concerns", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(
		meta_res,
		rob_results,
		method = "weight_downgrade",
		weight_high = 0,
		weight_concerns = 0.5
	)

	expect_true(adjusted@metadata$adjustment_method == "weight_downgrade")
	expect_true("Low" %in% adjusted@metadata$rob_judgments)
})

test_that("bias_adjusted_meta with exclude_high removes high risk studies", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(
		meta_res,
		rob_results,
		method = "exclude_high"
	)

	expect_true(adjusted@metadata$adjustment_method == "exclude_high")
	expect_true("Study 2" %in% adjusted@metadata$excluded_studies)
})

test_that("bias_adjusted_meta with selection_model adjusts weights", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Some concerns", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(
		meta_res,
		rob_results,
		method = "selection_model",
		selection_alpha = 0.05
	)

	expect_true(adjusted@metadata$adjustment_method == "selection_model")
	expect_true(!is.null(adjusted@metadata$selection_alpha))
})

test_that("bias_adjusted_meta works with ROBINS-I assessments", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_robins_i("Study 1", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i(
			"Study 2",
			"Moderate",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i(
			"Study 3",
			"Serious",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i("Study 4", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i("Study 5", "Low", "Low", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(
		meta_res,
		rob_results,
		method = "weight_downgrade",
		weight_moderate = 0.75,
		weight_serious = 0.25
	)

	expect_true(adjusted@metadata$adjustment_method == "weight_downgrade")
	# With exclude_high=TRUE (default), Serious/Critical studies are excluded
	expect_true(adjusted@n < meta_res@n)
	# Verify Serious study was excluded
	expect_true("Study 3" %in% adjusted@metadata$excluded_studies)
})

test_that("bias_adjusted_meta stores adjustment metadata", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(meta_res, rob_results, method = "exclude_high")

	expect_true("original_k" %in% names(adjusted@metadata))
	expect_true("adjusted_k" %in% names(adjusted@metadata))
	expect_true("excluded_studies" %in% names(adjusted@metadata))
	expect_true("rob_judgments" %in% names(adjusted@metadata))
})

test_that("bias_adjusted_meta errors with insufficient studies", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 4", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 5", "Low", "Low", "Low", "Low", "Low")
	)

	expect_error(
		bias_adjusted_meta(meta_res, rob_results, method = "exclude_high"),
		"Fewer than 2 studies"
	)
})

test_that("bias_adjusted_meta errors with invalid meta_result", {
	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	expect_error(
		bias_adjusted_meta(list(), rob_results),
		"meta_result must be a MetaResult object"
	)
})

test_that("bias_adjusted_meta errors with empty rob_results", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		effect_measure = "hr"
	)

	expect_error(
		bias_adjusted_meta(meta_res, list()),
		"non-empty list"
	)
})

test_that("bias_adjusted_meta errors with mismatched method", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	expect_error(
		bias_adjusted_meta(meta_res, rob_results, method = "invalid_method"),
		"'arg' should be one of"
	)
})

# =============================================================================
# summarize_bias_adjusted function tests
# =============================================================================

test_that("summarize_bias_adjusted returns data frame with comparison", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(meta_res, rob_results, method = "exclude_high")
	summary_df <- summarize_bias_adjusted(adjusted, meta_res)

	expect_true(is.data.frame(summary_df))
	expect_true("Parameter" %in% names(summary_df))
	expect_true("Original" %in% names(summary_df))
	expect_true("Adjusted" %in% names(summary_df))
})

test_that("summarize_bias_adjusted calculates percent change", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(meta_res, rob_results, method = "exclude_high")
	summary_df <- summarize_bias_adjusted(adjusted, meta_res)

	expect_true("Change" %in% names(summary_df))
})

test_that("summarize_bias_adjusted provides interpretation", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(meta_res, rob_results, method = "exclude_high")
	summary_df <- summarize_bias_adjusted(adjusted, meta_res)

	expect_true(!is.null(attr(summary_df, "interpretation")))
})

test_that("summarize_bias_adjusted errors with invalid adjusted_result", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		effect_measure = "hr"
	)

	expect_error(
		summarize_bias_adjusted(list(), meta_res),
		"BiasAdjustedMetaResult"
	)
})

test_that("summarize_bias_adjusted errors with invalid original_result", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(meta_res, rob_results, method = "exclude_high")

	expect_error(
		summarize_bias_adjusted(adjusted, list()),
		"MetaResult"
	)
})

# =============================================================================
# Integration tests with RoB2Result and ROBINSIResult
# =============================================================================

test_that("calculate_rob_weights integrates with RoB2Result", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Some concerns", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low")
	)

	rob_weights <- calculate_rob_weights(meta_res, rob_results)

	expect_true(S7::S7_inherits(rob_results[[1]], RoB2Result))
	expect_true(length(rob_weights$weights) == 5)
})

test_that("calculate_rob_weights integrates with ROBINSIResult", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_robins_i("Study 1", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i(
			"Study 2",
			"Moderate",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		),
		assess_robins_i(
			"Study 3",
			"Serious",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low",
			"Low"
		)
	)

	rob_weights <- calculate_rob_weights(meta_res, rob_results)

	expect_true(S7::S7_inherits(rob_results[[1]], ROBINSIResult))
	expect_true(length(rob_weights$weights) == 5)
})

test_that("bias_adjusted_meta works with mixed RoB results", {
	meta_res <- meta_analysis(
		yi = .meta_yi_hr,
		sei = .meta_sei_hr,
		study_labels = paste("Study", 1:5),
		effect_measure = "hr",
		model = "random"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i("Study 3", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_robins_i("Study 4", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 5", "Low", "Low", "Low", "Low", "Low")
	)

	adjusted <- bias_adjusted_meta(
		meta_res,
		rob_results,
		method = "weight_downgrade"
	)

	expect_true(adjusted@metadata$adjustment_method == "weight_downgrade")
})

# =============================================================================
# Regression Tests for Fixed Bugs
# =============================================================================

test_that(
	paste0(
		"vapply handles empty study_labels without type error ",
		"(issue: sapply)"
	),
	{
		# Create meta result with explicit empty labels
		meta_res <- meta_analysis(
			yi = c(0.5, 0.6, 0.7),
			sei = c(0.1, 0.15, 0.12),
			study_labels = c("S1", "S2", "S3"),
			effect_measure = "hr"
		)

		rob_results <- list(
			assess_rob2("S1", "Low", "Low", "Low", "Low", "Low"),
			assess_rob2("S2", "High", "Low", "Low", "Low", "Low"),
			assess_rob2("S3", "Low", "Low", "Low", "Low", "Low")
		)

		# Should complete without type errors
		expect_no_error(
			rob_sensitivity_analysis(meta_res, rob_results)
		)
	}
)

test_that("rejects invalid weight parameters", {
	meta_res <- meta_analysis(
		yi = c(0.5, 0.6, 0.7),
		sei = c(0.1, 0.15, 0.12),
		effect_measure = "hr"
	)

	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "High", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 3", "Low", "Low", "Low", "Low", "Low")
	)

	expect_error(
		calculate_rob_weights(meta_res, rob_results, weight_high = -1),
		"between 0 and 1"
	)
	expect_error(
		calculate_rob_weights(meta_res, rob_results, weight_concerns = 2),
		"between 0 and 1"
	)
})
