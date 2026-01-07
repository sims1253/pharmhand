# Tests for Forest Plot Function
# Tests for create_forest_plot
# Source: R/plotting_forest.R

library(testthat)
library(pharmhand)

test_that("create_forest_plot works with TTE data", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- create_mock_forest_tte(n = 30)

	p <- create_forest_plot(
		df,
		subgroups = list(SEX = "Sex", AGEGR1 = "Age Group"),
		endpoint_type = "tte"
	)

	expect_s7_class(p, ClinicalPlot)
	expect_equal(p@type, "forest")
	expect_true(ggplot2::is_ggplot(p@plot))
	expect_true("subgroups" %in% names(p@metadata))
})

test_that("create_forest_plot works with binary data", {
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVALC = sample(c("CR", "PR", "SD", "PD"), 60, replace = TRUE),
		SEX = rep(c("M", "F"), 30),
		stringsAsFactors = FALSE
	)

	p <- create_forest_plot(
		df,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "binary",
		response_values = c("CR", "PR")
	)

	expect_s7_class(p, ClinicalPlot)
	expect_equal(p@type, "forest")
	expect_equal(p@metadata$endpoint_type, "binary")
})

test_that("create_forest_plot includes overall estimate", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- create_mock_forest_tte(n = 30)

	p <- create_forest_plot(
		df,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte"
	)

	# Check that data includes "Overall" row
	expect_true(any(p@data$display_label == "Overall"))
})

test_that("create_forest_plot calculates interaction p-values", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- create_mock_forest_tte(n = 30)

	p <- create_forest_plot(
		df,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte",
		show_interaction = TRUE
	)

	# interaction_p should be calculated
	expect_true("interaction_p" %in% names(p@data))
})

test_that("create_forest_plot warns for missing subgroup variables", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- create_mock_forest_tte(n = 20)

	expect_warning(
		create_forest_plot(
			df,
			subgroups = list(NONEXISTENT = "Missing Variable"),
			endpoint_type = "tte"
		),
		"not found"
	)
})

test_that("create_forest_plot respects log_scale parameter", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- create_mock_forest_tte(n = 30)

	p_log <- create_forest_plot(
		df,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte",
		log_scale = TRUE
	)

	p_linear <- create_forest_plot(
		df,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte",
		log_scale = FALSE
	)

	expect_s7_class(p_log, ClinicalPlot)
	expect_s7_class(p_linear, ClinicalPlot)
})

test_that("create_forest_plot works with ADaMData", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- create_mock_forest_tte(n = 30)
	df$SAFFL <- "Y"

	adam <- ADaMData(data = df, population = "SAF")
	p <- create_forest_plot(
		adam,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte"
	)

	expect_s7_class(p, ClinicalPlot)
	expect_equal(p@type, "forest")
})
