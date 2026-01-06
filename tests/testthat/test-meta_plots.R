# Tests for meta-analysis plotting functions (R/meta_plots.R)

library(testthat)
library(pharmhand)

# =============================================================================
# create_meta_forest_plot function tests
# =============================================================================

test_that("create_meta_forest_plot creates ClinicalPlot", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
	plot <- create_meta_forest_plot(meta_res)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "forest_meta")
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
