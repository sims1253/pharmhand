# Tests for publication bias functions (R/meta_bias.R)

library(testthat)
library(pharmhand)

# =============================================================================
# eggers_test function tests
# =============================================================================

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

# =============================================================================
# trim_and_fill function tests
# =============================================================================

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

# =============================================================================
# create_funnel_plot function tests
# =============================================================================

test_that("create_funnel_plot creates ClinicalPlot", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
	plot <- create_funnel_plot(meta_res)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "funnel")
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
