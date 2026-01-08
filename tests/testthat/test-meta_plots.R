# Tests for meta-analysis plotting functions (R/meta_plots.R)

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
	expect_equal(plot_custom@title, "Custom Title")
	expect_equal(plot_custom@plot$labels$title, "Custom Title")

	# The size guide is removed when show_weights=FALSE
	expect_false("size" %in% names(plot_custom@plot$guides))

	# Heterogeneity stats are added as caption when show_heterogeneity=TRUE
	expect_false("caption" %in% names(plot_custom@plot$labels))

	# Prediction interval is added via geom_segment when show_prediction=TRUE
	layer_types <- sapply(plot_custom@plot$layers, function(l) class(l$geom)[1])
	expect_false(any(grepl("GeomSegment", layer_types, fixed = TRUE)))
})

test_that("create_meta_forest_plot shows elements when enabled", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	# Create with prediction interval for testing
	meta_res@prediction_interval <- c(0.5, 1.2)

	plot_full <- create_meta_forest_plot(
		meta_res,
		title = "Full Feature Plot",
		show_weights = TRUE,
		show_heterogeneity = TRUE,
		show_prediction = TRUE
	)

	# Verify all elements are present when enabled
	expect_equal(plot_full@title, "Full Feature Plot")
	expect_equal(plot_full@plot$labels$title, "Full Feature Plot")

	# Size guide should be present when show_weights=TRUE
	expect_true("size" %in% names(plot_full@plot$guides))

	# Caption should be present when show_heterogeneity=TRUE
	expect_true("caption" %in% names(plot_full@plot$labels))
	expect_true(grepl("I2", plot_full@plot$labels$caption, fixed = TRUE))

	# Prediction interval segment should be present
	layer_types <- sapply(plot_full@plot$layers, function(l) class(l$geom)[1])
	expect_true(any(grepl("GeomSegment", layer_types, fixed = TRUE)))
})
