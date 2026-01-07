# Tests for Efficacy Plotting Functions
# Tests for create_mean_plot, create_spider_plot
# Source: R/plotting_efficacy.R

library(testthat)
library(pharmhand)

test_that("create_mean_plot handles missing group_var", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(1:50, each = 5),
		AVISITN = rep(0:4, 50),
		AVAL = rnorm(250, mean = 50, sd = 10)
	)

	plot <- create_mean_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "AVAL"
	)

	expect_true(S7::S7_inherits(plot, ClinicalPlot))
	expect_equal(plot@type, "mean_plot")
})

test_that("create_mean_plot with custom confidence level", {
	set.seed(123)
	data <- data.frame(
		visit = rep(c("V1", "V2"), each = 100),
		value = c(rnorm(100, 10, 2), rnorm(100, 12, 2))
	)

	plot_90 <- create_mean_plot(
		data = data,
		x_var = "visit",
		y_var = "value",
		ci_level = 0.90
	)

	plot_99 <- create_mean_plot(
		data = data,
		x_var = "visit",
		y_var = "value",
		ci_level = 0.99
	)

	expect_s7_class(plot_90, ClinicalPlot)
	expect_s7_class(plot_99, ClinicalPlot)
})

test_that("create_mean_plot with show_points", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(1:20, each = 2),
		visit = rep(c("V1", "V2"), 20),
		value = rnorm(40, 10, 2),
		group = rep(c("A", "B"), 20)
	)

	plot <- create_mean_plot(
		data = data,
		x_var = "visit",
		y_var = "value",
		group_var = "group",
		show_points = TRUE
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_mean_plot without show_n", {
	set.seed(123)
	data <- data.frame(
		visit = rep(c("V1", "V2"), each = 100),
		value = c(rnorm(100, 10, 2), rnorm(100, 12, 2))
	)

	plot <- create_mean_plot(
		data = data,
		x_var = "visit",
		y_var = "value",
		show_n = FALSE
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_mean_plot with custom parameters", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(1:20, each = 2),
		visit = rep(c("V1", "V2"), 20),
		value = rnorm(40, 10, 2),
		group = rep(c("A", "B"), 20)
	)

	plot <- create_mean_plot(
		data = data,
		x_var = "visit",
		y_var = "value",
		group_var = "group",
		title = "Custom Title",
		x_label = "Visit",
		y_label = "Value",
		base_size = 14,
		line_size = 1.5,
		point_size = 4,
		dodge_width = 0.3,
		error_bar_width = 0.2
	)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@title, "Custom Title")
})

test_that("create_spider_plot handles missing group_var", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:20), each = 6),
		AVISITN = rep(0:5, 20),
		PCHG = c(replicate(20, cumsum(c(0, rnorm(5, mean = -5, sd = 15))))),
		stringsAsFactors = FALSE
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG"
	)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "spider_plot")
})

test_that("create_spider_plot highlights subjects", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:20), each = 4),
		AVISITN = rep(0:3, 20),
		PCHG = rnorm(80, 0, 20),
		TRT01P = rep(c("Treatment", "Placebo"), each = 40)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		group_var = "TRT01P",
		highlight_subjects = c("SUBJ001", "SUBJ005")
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_spider_plot with only reference line", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = rnorm(40, 0, 20)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		reference_line = 0
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_spider_plot with custom alpha", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = rnorm(40, 0, 20)
	)

	plot_low <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		alpha = 0.3
	)

	plot_high <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		alpha = 0.9
	)

	expect_s7_class(plot_low, ClinicalPlot)
	expect_s7_class(plot_high, ClinicalPlot)
})

test_that("create_spider_plot with custom line_size", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = rnorm(40, 0, 20)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		line_size = 1.0
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_spider_plot handles missing values", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = c(rnorm(35, 0, 20), rep(NA, 5))
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG"
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_spider_plot with custom labels", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = rnorm(40, 0, 20)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		title = "My Spider Plot",
		x_label = "Visit",
		y_label = "Percent Change"
	)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@title, "My Spider Plot")
})

test_that("create_spider_plot with multiple threshold lines", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = rnorm(40, 0, 20)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		reference_line = 0,
		threshold_lines = c(-30, -20, 20, 30)
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_spider_plot with groups and colors", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:20), each = 4),
		AVISITN = rep(0:3, 20),
		PCHG = rnorm(80, 0, 20),
		TRT = rep(c("A", "B"), 40)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		group_var = "TRT",
		palette = c("A" = "red", "B" = "blue")
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_spider_plot with custom base_size", {
	set.seed(123)
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = rnorm(40, 0, 20)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		base_size = 14
	)

	expect_s7_class(plot, ClinicalPlot)
})
