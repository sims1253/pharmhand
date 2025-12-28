test_that("create_km_plot works with valid data", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	# Mock data
	set.seed(123)
	df <- data.frame(
		time = rexp(20, 0.1),
		event = sample(0:1, 20, replace = TRUE),
		trt = rep(c("A", "B"), each = 10)
	)

	p <- create_km_plot(df, "time", "event", "trt")

	expect_s7_class(p, ClinicalPlot)
	expect_true(ggplot2::is_ggplot(p@plot))
	expect_equal(p@title, "Kaplan-Meier Plot")
})

test_that("create_km_plot handles CNSR inversion", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(123)
	# ADaM format: CNSR = 0 means event, CNSR = 1 means censored
	df <- data.frame(
		AVAL = rexp(20, 0.1),
		CNSR = sample(0:1, 20, replace = TRUE),
		TRT01P = rep(c("Placebo", "Active"), each = 10)
	)

	p <- create_km_plot(
		df,
		time_var = "AVAL",
		event_var = "CNSR",
		trt_var = "TRT01P"
	)

	expect_s7_class(p, ClinicalPlot)
	expect_true(ggplot2::is_ggplot(p@plot))
})

test_that("create_km_plot shows median lines", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- data.frame(
		AVAL = rexp(40, 0.05),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		TRT01P = rep(c("Placebo", "Active"), each = 20)
	)

	p <- create_km_plot(df, show_median = TRUE)

	expect_s7_class(p, ClinicalPlot)
	# Check that geom_segment layers exist (for median lines)
	layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
	expect_true(any(grepl("Segment", layer_classes, fixed = TRUE)))
})

test_that("create_km_plot shows confidence bands", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- data.frame(
		AVAL = rexp(40, 0.05),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		TRT01P = rep(c("Placebo", "Active"), each = 20)
	)

	p <- create_km_plot(df, show_ci = TRUE)

	expect_s7_class(p, ClinicalPlot)
	# Check that ribbon layer exists
	layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
	expect_true(any(grepl("Ribbon", layer_classes, fixed = TRUE)))
})

test_that("create_km_plot shows landmarks", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- data.frame(
		AVAL = rexp(40, 0.05),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		TRT01P = rep(c("Placebo", "Active"), each = 20)
	)

	p <- create_km_plot(df, landmarks = c(12, 24))

	expect_s7_class(p, ClinicalPlot)
	# Check that vline layers exist (for landmark lines)
	layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
	expect_true(any(grepl("Vline", layer_classes, fixed = TRUE)))
})

test_that("create_km_plot works with ADaMData", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		AVAL = rexp(40, 0.05),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		SAFFL = "Y"
	)

	adam <- ADaMData(data = df, population = "SAF")
	p <- create_km_plot(adam)

	expect_s7_class(p, ClinicalPlot)
	expect_true(ggplot2::is_ggplot(p@plot))
})

test_that("create_km_plot applies custom palette", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- data.frame(
		AVAL = rexp(40, 0.05),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		TRT01P = rep(c("Placebo", "Active"), each = 20)
	)

	custom_colors <- c("Placebo" = "red", "Active" = "blue")
	p <- create_km_plot(df, palette = custom_colors)

	expect_s7_class(p, ClinicalPlot)
})

# ===========================================================================
# Tests for Forest Plot
# ===========================================================================

test_that("create_forest_plot works with TTE data", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(42)
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		AGEGR1 = sample(c("<65", ">=65"), 60, replace = TRUE),
		stringsAsFactors = FALSE
	)

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
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		stringsAsFactors = FALSE
	)

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
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		stringsAsFactors = FALSE
	)

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
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

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
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		stringsAsFactors = FALSE
	)

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
	df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:60),
		TRT01P = rep(c("Placebo", "Active"), each = 30),
		AVAL = c(rexp(30, 0.05), rexp(30, 0.03)),
		CNSR = sample(0:1, 60, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), 30),
		SAFFL = "Y",
		stringsAsFactors = FALSE
	)

	adam <- ADaMData(data = df, population = "SAF")
	p <- create_forest_plot(
		adam,
		subgroups = list(SEX = "Sex"),
		endpoint_type = "tte"
	)

	expect_s7_class(p, ClinicalPlot)
	expect_equal(p@type, "forest")
})
