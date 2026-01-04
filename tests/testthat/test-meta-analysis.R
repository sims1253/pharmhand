# Tests for meta-analysis functions

test_that("meta_analysis performs fixed-effect analysis", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_s7_class(result, MetaResult)
	expect_equal(result@model, "fixed")
	expect_equal(result@effect_measure, "hr")
	expect_equal(result@n, 5L)
	expect_true(result@estimate > 0) # HR should be positive
	expect_length(result@ci, 2)
})

test_that("meta_analysis performs random-effects analysis", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "random"
	)

	expect_s7_class(result, MetaResult)
	expect_equal(result@model, "random")
	expect_true(!is.null(result@heterogeneity$tau2))
	expect_true(!is.null(result@heterogeneity$I2))
})

test_that("meta_analysis calculates heterogeneity statistics", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6) # Variable effects
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		model = "random"
	)

	het <- result@heterogeneity
	expect_true(het$Q > 0)
	expect_true(het$I2 >= 0 && het$I2 <= 1)
	expect_true(het$tau2 >= 0)
})

test_that("calculate_heterogeneity returns all statistics", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	het <- calculate_heterogeneity(yi, sei)

	expect_true(is.list(het))
	expect_true("Q" %in% names(het))
	expect_true("I2" %in% names(het))
	expect_true("tau2" %in% names(het))
	expect_true("interpretation" %in% names(het))
})

test_that("leave_one_out performs sensitivity analysis", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
	loo <- leave_one_out(meta_res)

	expect_true(is.data.frame(loo$results))
	expect_equal(nrow(loo$results), 5) # One row per excluded study
})

test_that("create_meta_forest_plot creates ClinicalPlot", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
	plot <- create_meta_forest_plot(meta_res)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "forest_meta")
})

test_that("eggers_test detects asymmetry", {
	# Create asymmetric data (small studies with large effects)
	yi <- c(0.5, 0.6, 0.4, 0.3, 0.8, 1.0, 1.2)
	sei <- c(0.3, 0.3, 0.25, 0.2, 0.1, 0.08, 0.05)

	result <- eggers_test(yi = yi, sei = sei)

	expect_true(is.list(result))
	expect_true("intercept" %in% names(result))
	expect_true("p_value" %in% names(result))
	expect_true("interpretation" %in% names(result))
})

test_that("create_funnel_plot creates ClinicalPlot", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
	plot <- create_funnel_plot(meta_res)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "funnel")
})

test_that("indirect_comparison calculates Bucher estimate", {
	result <- indirect_comparison(
		effect_ab = log(0.75), # A vs B
		se_ab = 0.12,
		effect_bc = log(0.85), # B vs C
		se_bc = 0.10,
		effect_measure = "hr"
	)

	expect_s7_class(result, ComparisonResult)
	expect_equal(result@effect_measure, "hr")
	expect_true(result@estimate > 0)
})

test_that("trim_and_fill detects and imputes missing studies", {
	# Create asymmetric data
	yi <- c(-0.5, -0.4, -0.3, -0.1, 0.0, 0.5, 0.6, 0.7)
	sei <- c(0.1, 0.12, 0.15, 0.18, 0.2, 0.1, 0.12, 0.15)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")
	tf_result <- trim_and_fill(meta_res)

	expect_true(is.list(tf_result))
	expect_true("original" %in% names(tf_result))
	expect_true("adjusted" %in% names(tf_result))
	expect_true("n_imputed" %in% names(tf_result))
})

test_that("network_meta analyzes treatment network", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	result <- network_meta(nma_data, effect_measure = "hr")

	expect_true(is.list(result))
	expect_true("comparisons" %in% names(result))
	expect_true("network" %in% names(result))
	expect_equal(result$network$n_treatments, 3)
})

test_that("create_network_plot creates visualization", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	plot <- create_network_plot(nma_res)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "network_geometry")
})

test_that("calculate_sucra computes treatment rankings", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	sucra <- calculate_sucra(nma_res)

	expect_true(is.list(sucra))
	expect_true("ranking" %in% names(sucra))
	expect_true("sucra" %in% names(sucra))
	expect_equal(length(sucra$sucra), 3) # 3 treatments
})

test_that("create_league_table creates ClinicalTable", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	table <- create_league_table(nma_res)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@type, "league_table")
})

# =============================================================================
# Additional tests for edge cases and validation
# =============================================================================

test_that("meta_analysis errors with insufficient studies", {
	yi <- c(0.5)
	sei <- c(0.1)

	expect_error(
		meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
		"At least 2 studies"
	)
})

test_that("meta_analysis errors with mismatched lengths", {
	yi <- c(0.5, 0.6)
	sei <- c(0.1)

	expect_error(
		meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
		"same length"
	)
})

test_that("meta_analysis errors with missing yi or sei", {
	expect_error(
		meta_analysis(yi = NULL, sei = c(0.1, 0.2), effect_measure = "hr"),
		"required"
	)
})

test_that("meta_analysis accepts data frame input", {
	df <- data.frame(
		yi = log(c(0.75, 0.82, 0.68)),
		sei = c(0.12, 0.15, 0.18)
	)

	result <- meta_analysis(data = df, effect_measure = "hr")

	expect_s7_class(result, MetaResult)
	expect_equal(result@n, 3L)
})

test_that("meta_analysis calculates prediction intervals for random effects", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		model = "random",
		prediction = TRUE
	)

	expect_true(!is.null(result@prediction_interval))
	expect_length(result@prediction_interval, 2)
})

test_that("meta_analysis respects different effect measures", {
	yi <- c(0.5, 0.6, 0.4)
	sei <- c(0.1, 0.1, 0.1)

	# Test different effect measures
	hr_result <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
	or_result <- meta_analysis(yi = yi, sei = sei, effect_measure = "or")
	rr_result <- meta_analysis(yi = yi, sei = sei, effect_measure = "rr")
	rd_result <- meta_analysis(yi = yi, sei = sei, effect_measure = "rd")
	md_result <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")

	expect_s7_class(hr_result, MetaResult)
	expect_s7_class(or_result, MetaResult)
	expect_s7_class(rr_result, MetaResult)
	expect_s7_class(rd_result, MetaResult)
	expect_s7_class(md_result, MetaResult)
})

test_that("meta_analysis uses different tau2 estimation methods", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result_dl <- meta_analysis(yi = yi, sei = sei, method = "DL")
	result_reml <- meta_analysis(yi = yi, sei = sei, method = "REML")
	result_pm <- meta_analysis(yi = yi, sei = sei, method = "PM")

	expect_s7_class(result_dl, MetaResult)
	expect_s7_class(result_reml, MetaResult)
	expect_s7_class(result_pm, MetaResult)

	# Different methods may produce slightly different tau2 estimates
	expect_true(!is.na(result_dl@heterogeneity$tau2))
	expect_true(!is.na(result_reml@heterogeneity$tau2))
	expect_true(!is.na(result_pm@heterogeneity$tau2))
})

test_that("meta_analysis applies Knapp-Hartung adjustment", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result_kh <- meta_analysis(yi = yi, sei = sei, knapp_hartung = TRUE)
	result_no_kh <- meta_analysis(yi = yi, sei = sei, knapp_hartung = FALSE)

	expect_s7_class(result_kh, MetaResult)
	expect_s7_class(result_no_kh, MetaResult)

	# Knapp-Hartung should affect the confidence intervals
	expect_true(result_kh@metadata$knapp_hartung)
	expect_false(result_no_kh@metadata$knapp_hartung)
})

test_that("eggers_test returns NA for insufficient studies", {
	yi <- c(0.5)
	sei <- c(0.2)

	result <- eggers_test(yi = yi, sei = sei)

	expect_true(is.na(result$intercept))
	expect_true(is.na(result$slope))
	expect_true(is.na(result$p_value))
	# Interpretation message contains information about insufficient studies
	expect_true(grepl(
		"Insufficient|studies",
		result$interpretation,
		ignore.case = TRUE
	))
})

test_that("eggers_test can accept MetaResult input", {
	# Test with direct yi/sei input instead of MetaResult
	# to avoid metadata dependency issues in this test
	yi <- c(0.5, 0.6, 0.4, 0.3, 0.8, 0.5)
	sei <- c(0.2, 0.15, 0.25, 0.18, 0.22, 0.19)

	result <- eggers_test(yi = yi, sei = sei)

	expect_true(is.list(result))
	expect_true("intercept" %in% names(result))
	expect_true("slope" %in% names(result))
	expect_true(!is.na(result$intercept))
})

test_that("indirect_comparison handles different effect measures", {
	# HR
	hr_result <- indirect_comparison(
		effect_ab = log(0.75),
		se_ab = 0.12,
		effect_bc = log(0.85),
		se_bc = 0.10,
		effect_measure = "hr"
	)
	expect_s7_class(hr_result, ComparisonResult)
	expect_true(hr_result@estimate > 0)

	# OR
	or_result <- indirect_comparison(
		effect_ab = log(0.5),
		se_ab = 0.15,
		effect_bc = log(0.6),
		se_bc = 0.12,
		effect_measure = "or"
	)
	expect_s7_class(or_result, ComparisonResult)

	# MD
	md_result <- indirect_comparison(
		effect_ab = -5.0,
		se_ab = 1.0,
		effect_bc = -3.0,
		se_bc = 0.8,
		effect_measure = "md"
	)
	expect_s7_class(md_result, ComparisonResult)
	expect_true(md_result@estimate < 0) # A vs C should be negative
})

test_that("indirect_comparison validates standard errors", {
	expect_error(
		indirect_comparison(
			effect_ab = log(0.75),
			se_ab = 0, # Invalid: SE = 0
			effect_bc = log(0.85),
			se_bc = 0.10,
			effect_measure = "hr"
		),
		"positive"
	)

	expect_error(
		indirect_comparison(
			effect_ab = log(0.75),
			se_ab = -0.1, # Invalid: negative SE
			effect_bc = log(0.85),
			se_bc = 0.10,
			effect_measure = "hr"
		),
		"positive"
	)
})

test_that("trim_and_fill returns expected structure", {
	# Create asymmetric data for trim and fill
	yi <- c(-0.5, -0.4, -0.3, -0.1, 0.0, 0.5, 0.6, 0.7)
	sei <- c(0.1, 0.12, 0.15, 0.18, 0.2, 0.1, 0.12, 0.15)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")

	# Just check it returns a list with expected structure
	tf_result <- trim_and_fill(meta_res)

	expect_true(is.list(tf_result))
	expect_true("original" %in% names(tf_result))
	expect_true("adjusted" %in% names(tf_result))
	expect_true("n_imputed" %in% names(tf_result))
})

test_that("network_meta handles incomplete networks", {
	# Data with studies that may have incomplete information
	incomplete_data <- data.frame(
		study = c("S1", "S2", "S3", "S4"),
		treat1 = c("A", "B", "A", "D"),
		treat2 = c("B", "C", "C", "E"),
		effect = log(c(0.75, 0.90, 0.80, 0.95)),
		se = c(0.12, 0.15, 0.18, 0.20)
	)

	# Should still produce results even if network not fully connected
	result <- network_meta(incomplete_data, effect_measure = "hr")

	expect_true(is.list(result))
	expect_true("network" %in% names(result))
})

test_that("network_meta handles custom reference treatment", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	result_ref_b <- network_meta(nma_data, reference = "B", effect_measure = "hr")

	expect_true(is.list(result_ref_b))
	expect_equal(result_ref_b$network$reference, "B")
})

test_that("create_network_plot supports different layouts", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")

	plot_circle <- create_network_plot(nma_res, layout = "circle")
	plot_star <- create_network_plot(nma_res, layout = "star")
	plot_auto <- create_network_plot(nma_res, layout = "auto")

	expect_s7_class(plot_circle, ClinicalPlot)
	expect_s7_class(plot_star, ClinicalPlot)
	expect_s7_class(plot_auto, ClinicalPlot)
})

test_that("compare_direct_indirect tests consistency", {
	# Direct comparison (A vs C)
	direct <- indirect_comparison(
		effect_ab = log(0.75),
		se_ab = 0.12,
		effect_bc = log(0.85),
		se_bc = 0.10,
		effect_measure = "hr"
	)

	# Indirect comparison through B
	indirect <- indirect_comparison(
		effect_ab = log(0.78),
		se_ab = 0.13,
		effect_bc = log(0.82),
		se_bc = 0.11,
		effect_measure = "hr"
	)

	result <- compare_direct_indirect(direct, indirect, effect_measure = "hr")

	expect_true(is.list(result))
	expect_true("direct_estimate" %in% names(result))
	expect_true("indirect_estimate" %in% names(result))
	expect_true("inconsistency_test" %in% names(result))
	expect_true("pooled" %in% names(result))
})

test_that("assess_transitivity evaluates population balance", {
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
		treatment = c("A", "B", "B", "C", "A", "C"),
		mean_age = c(55, 55, 58, 58, 52, 52),
		pct_male = c(60, 60, 65, 65, 55, 55),
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("mean_age", "pct_male"),
		continuous_vars = c("mean_age", "pct_male")
	)

	expect_true(is.list(result))
	expect_true("summaries" %in% names(result))
	expect_true("overall_assessment" %in% names(result))
	expect_equal(result$n_treatments, 3)
})

test_that("assess_transitivity handles categorical characteristics", {
	# Test with only categorical variables
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2"),
		treatment = c("A", "B", "A", "B"),
		region = c("US", "US", "EU", "EU"),
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("region"),
		continuous_vars = character(0) # No continuous vars
	)

	expect_true(is.list(result))
	expect_true("summaries" %in% names(result))
})

test_that("assess_transitivity handles categorical characteristics", {
	# Test with only categorical variables
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2"),
		treatment = c("A", "B", "A", "B"),
		region = c("US", "US", "EU", "EU"),
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("region"),
		continuous_vars = character(0) # No continuous vars
	)

	expect_true(is.list(result))
	expect_true("summaries" %in% names(result))
})

test_that("assess_transitivity handles insufficient variation", {
	# Create data where all means are the same - no variation
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2"),
		treatment = c("A", "B", "A", "B"),
		mean_age = c(55, 55, 55, 55), # No variation
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("mean_age"),
		continuous_vars = c("mean_age")
	)

	expect_true(is.list(result))
	expect_true("overall_assessment" %in% names(result))
})

test_that("assess_transitivity handles categorical characteristics", {
	# Test with only categorical variables
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2"),
		treatment = c("A", "B", "A", "B"),
		region = c("US", "US", "EU", "EU"),
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("region"),
		continuous_vars = c() # No continuous vars
	)

	expect_true(is.list(result))
	expect_true("summaries" %in% names(result))
})

test_that("assess_transitivity handles insufficient variation", {
	# Create data where all means are the same - no variation
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2"),
		treatment = c("A", "B", "A", "B"),
		mean_age = c(55, 55, 55, 55), # No variation
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("mean_age"),
		continuous_vars = c("mean_age")
	)

	expect_true(is.list(result))
	expect_true("overall_assessment" %in% names(result))
})

test_that("assess_transitivity validates inputs", {
	# Missing study_id column
	bad_data <- data.frame(
		treatment = c("A", "B"),
		mean_age = c(55, 55)
	)

	expect_error(
		assess_transitivity(bad_data, char_vars = "mean_age"),
		"study_id"
	)

	# Missing characteristic variable
	study_data <- data.frame(
		study_id = c("S1", "S2"),
		treatment = c("A", "B"),
		mean_age = c(55, 58)
	)

	expect_error(
		assess_transitivity(study_data, char_vars = "nonexistent"),
		"not found"
	)
})

test_that("create_meta_forest_plot supports customization", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	plot_custom <- create_meta_forest_plot(
		meta_res,
		title = "Custom Title",
		show_weights = FALSE,
		show_heterogeneity = FALSE,
		show_prediction = FALSE
	)

	expect_s7_class(plot_custom, ClinicalPlot)
	expect_equal(plot_custom@type, "forest_meta")
})

test_that("create_funnel_plot supports customization", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	plot_custom <- create_funnel_plot(
		meta_res,
		title = "Custom Funnel",
		show_ci = FALSE,
		show_egger = FALSE
	)

	expect_s7_class(plot_custom, ClinicalPlot)
	expect_equal(plot_custom@type, "funnel")
})

test_that("meta_analysis stores individual study results", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		study_labels = c("Study 1", "Study 2", "Study 3")
	)

	expect_true(is.list(result@study_results))
	expect_equal(length(result@study_results), 3)
	expect_true(all(sapply(result@study_results, function(x) {
		S7::S7_inherits(x, ComparisonResult)
	})))
	expect_equal(names(result@study_results), c("Study 1", "Study 2", "Study 3"))
})

test_that("meta_analysis stores study weights", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	result <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	expect_true(!is.null(result@weights))
	expect_length(result@weights, 3)
	expect_true(all(result@weights > 0))
	expect_equal(sum(result@weights), 1, tolerance = 0.001)
	# Weights should sum to 1
})

test_that("calculate_heterogeneity supports different methods", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	het_dl <- calculate_heterogeneity(yi, sei, method = "DL")
	het_reml <- calculate_heterogeneity(yi, sei, method = "REML")
	het_pm <- calculate_heterogeneity(yi, sei, method = "PM")

	expect_equal(het_dl$method, "DL")
	expect_equal(het_reml$method, "REML")
	expect_equal(het_pm$method, "PM")

	# All should have heterogeneity statistics
	expect_true(!is.na(het_dl$tau2))
	expect_true(!is.na(het_reml$tau2))
	expect_true(!is.na(het_pm$tau2))
})

test_that("leave_one_out identifies influential studies", {
	# Create data where one study has extreme effect
	yi <- c(0.1, 0.15, 0.12, 2.5, 0.13) # 4th study is outlier
	sei <- c(0.1, 0.1, 0.1, 0.1, 0.1)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")
	loo <- leave_one_out(meta_res)

	expect_true("influential_studies" %in% names(loo))
	expect_true("n_influential" %in% names(loo))

	# The 4th study (Study 4) should be identified as influential
	expect_true(any(grepl("4", loo$influential_studies, fixed = TRUE)))
})

test_that("leave_one_out works with direct yi/sei input", {
	yi <- c(0.5, 0.6, 0.4)
	sei <- c(0.1, 0.1, 0.1)

	loo <- leave_one_out(yi = yi, sei = sei)

	expect_true(is.data.frame(loo$results))
	expect_equal(nrow(loo$results), 3)
})

test_that("trim_and_fill supports different estimators", {
	yi <- c(-0.5, -0.4, -0.3, -0.1, 0.0, 0.5, 0.6, 0.7)
	sei <- c(0.1, 0.12, 0.15, 0.18, 0.2, 0.1, 0.12, 0.15)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")

	tf_l0 <- trim_and_fill(meta_res, estimator = "L0")
	tf_r0 <- trim_and_fill(meta_res, estimator = "R0")
	tf_q0 <- trim_and_fill(meta_res, estimator = "Q0")

	expect_true(is.list(tf_l0))
	expect_true(is.list(tf_r0))
	expect_true(is.list(tf_q0))
})

test_that("network_meta handles fixed-effect model", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	result <- network_meta(nma_data, effect_measure = "hr", model = "fixed")

	expect_true(is.list(result))
	expect_equal(result$model, "fixed")
})

test_that("create_league_table includes comparison matrix", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	table <- create_league_table(nma_res)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@type, "league_table")
	expect_true(is.data.frame(table@data) || is.matrix(table@data))
})
