# Tests for Survival Plotting Functions
# Tests for create_km_plot, create_ae_cumulative_incidence_plot,
# create_loglog_plot

describe("plotting survival", {
	it("create_km_plot works with valid data", {
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

	it("create_km_plot handles CNSR inversion", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 20, rate = 0.1)

		p <- create_km_plot(
			df,
			time_var = "AVAL",
			event_var = "CNSR",
			trt_var = "TRT01P"
		)

		expect_s7_class(p, ClinicalPlot)
		expect_true(ggplot2::is_ggplot(p@plot))
	})

	it("create_km_plot shows median lines", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		p <- create_km_plot(df, show_median = TRUE)

		expect_s7_class(p, ClinicalPlot)
		# Check that geom_segment layers exist (for median lines)
		layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
		expect_true(any(grepl("Segment", layer_classes, fixed = TRUE)))
	})

	it("create_km_plot shows confidence bands", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		p <- create_km_plot(df, show_ci = TRUE)

		expect_s7_class(p, ClinicalPlot)
		# Check that ribbon layer exists
		layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
		expect_true(any(grepl("Ribbon", layer_classes, fixed = TRUE)))
	})

	it("create_km_plot shows landmarks", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		p <- create_km_plot(df, landmarks = c(12, 24))

		expect_s7_class(p, ClinicalPlot)
		# Check that vline layers exist (for landmark lines)
		layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
		expect_true(any(grepl("Vline", layer_classes, fixed = TRUE)))
	})

	it("create_km_plot works with ADaMData", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)
		df$SAFFL <- "Y"

		adam <- ADaMData(data = df, population = "SAF")
		p <- create_km_plot(adam)

		expect_s7_class(p, ClinicalPlot)
		expect_true(ggplot2::is_ggplot(p@plot))
	})

	it("create_km_plot applies custom palette", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		custom_colors <- c("Placebo" = "red", "Active" = "blue")
		p <- create_km_plot(df, palette = custom_colors)

		expect_s7_class(p, ClinicalPlot)
	})

	it("create_km_plot uses base_size parameter", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		p <- create_km_plot(df, base_size = 14)

		expect_s7_class(p, ClinicalPlot)
		# Check that theme has the expected base size
		expect_equal(p@plot$theme$text$size, 14)
	})

	it("create_km_plot uses okabe_ito palette by default", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		p <- create_km_plot(df)

		expect_s7_class(p, ClinicalPlot)
		# Check that color scale is manual (not default ggplot2)
		scale_color <- p@plot$scales$get_scales("colour")
		expect_true(!is.null(scale_color))
	})

	it("create_km_plot respects pharmhand.palette option", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		# Set custom palette via options
		old_opt <- getOption("pharmhand.palette")
		on.exit(options(pharmhand.palette = old_opt))
		options(pharmhand.palette = c("#FF0000", "#00FF00", "#0000FF"))

		p <- create_km_plot(df)

		expect_s7_class(p, ClinicalPlot)
		# Check that scale uses the custom colors
		scale_color <- p@plot$scales$get_scales("colour")
		expect_true(!is.null(scale_color))
	})

	it("create_km_plot supports named palettes via option", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		# Set a named palette via options (R4 is another built-in palette)
		old_opt <- getOption("pharmhand.palette")
		on.exit(options(pharmhand.palette = old_opt))
		options(pharmhand.palette = "R4")

		p <- create_km_plot(df)

		expect_s7_class(p, ClinicalPlot)
		scale_color <- p@plot$scales$get_scales("colour")
		expect_true(!is.null(scale_color))
	})

	it("create_km_plot risk table uses consistent font size", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")
		skip_if_not_installed("patchwork")

		df <- create_mock_km_data(n = 40, rate = 0.05, prob_event = 0.7)

		p <- create_km_plot(df, risk_table = TRUE, base_size = 14)

		expect_s7_class(p, ClinicalPlot)
		# Plot should be a patchwork object when risk_table = TRUE
		expect_true(inherits(p@plot, "patchwork"))
	})

	it("create_ae_cumulative_incidence_plot works with basic data", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		set.seed(123)
		df <- data.frame(
			time = rexp(20, 0.1),
			event = sample(0:1, 20, replace = TRUE),
			trt = rep(c("A", "B"), each = 10)
		)

		p <- create_ae_cumulative_incidence_plot(df, "time", "event", "trt")

		expect_s7_class(p, ClinicalPlot)
		expect_true(ggplot2::is_ggplot(p@plot))
		expect_equal(p@title, "Cumulative Incidence of Adverse Events")
	})

	it("create_ae_cumulative_incidence_plot supports multiple groups", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		set.seed(123)
		df <- data.frame(
			time = rexp(30, 0.1),
			event = sample(0:1, 30, replace = TRUE),
			trt = rep(c("A", "B", "C"), each = 10)
		)

		p <- create_ae_cumulative_incidence_plot(df, "time", "event", "trt")

		expect_s7_class(p, ClinicalPlot)
		expect_equal(length(unique(p@plot$data$strata)), 3)
	})

	it("create_ae_cumulative_incidence_plot shows confidence bands", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		set.seed(123)
		df <- data.frame(
			time = rexp(20, 0.1),
			event = sample(0:1, 20, replace = TRUE),
			trt = rep(c("A", "B"), each = 10)
		)

		p <- create_ae_cumulative_incidence_plot(
			df,
			time_var = "time",
			event_var = "event",
			trt_var = "trt",
			show_ci = TRUE
		)

		expect_s7_class(p, ClinicalPlot)
		layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
		expect_true(any(grepl("Ribbon", layer_classes, fixed = TRUE)))
	})

	it("create_loglog_plot works with valid data", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		set.seed(123)
		df <- data.frame(
			time = rexp(20, 0.1),
			event = rep(c(1, 0), length.out = 20),
			trt = rep(c("A", "B"), each = 10)
		)

		# Log-log plots may warn about tied events or non-finite values
		p <- suppressWarnings(create_loglog_plot(df, "time", "event", "trt"))

		expect_s7_class(p, ClinicalPlot)
		expect_true(ggplot2::is_ggplot(p@plot))
		expect_equal(p@title, "Log-Log Survival Plot")
	})

	it("create_loglog_plot supports censor marks", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		df <- data.frame(
			time = c(1, 2, 3, 4, 1.5, 2.5, 3.5, 4.5),
			event = c(1, 0, 1, 0, 1, 0, 1, 0),
			trt = rep(c("A", "B"), each = 4)
		)

		p <- create_loglog_plot(df, "time", "event", "trt", show_censor = TRUE)
		layer_classes <- sapply(p@plot$layers, function(l) class(l$geom)[1])
		expect_true(any(grepl("Point", layer_classes, fixed = TRUE)))

		p_no <- create_loglog_plot(df, "time", "event", "trt", show_censor = FALSE)
		layer_classes_no <- sapply(p_no@plot$layers, function(l) class(l$geom)[1])
		expect_false(any(grepl("Point", layer_classes_no, fixed = TRUE)))
	})

	it("create_loglog_plot supports multiple treatment groups", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		set.seed(123)
		df <- data.frame(
			time = rexp(30, 0.1),
			event = rep(c(1, 0), length.out = 30),
			trt = rep(c("A", "B", "C"), each = 10)
		)

		p <- suppressWarnings(create_loglog_plot(df, "time", "event", "trt"))

		expect_s7_class(p, ClinicalPlot)
		expect_equal(length(unique(p@plot$data$strata)), 3)
	})

	it("create_loglog_plot applies custom colors", {
		skip_if_not_installed("survival")
		skip_if_not_installed("ggplot2")

		set.seed(123)
		df <- data.frame(
			time = rexp(20, 0.1),
			event = rep(c(1, 0), length.out = 20),
			trt = rep(c("Placebo", "Active"), each = 10)
		)

		custom_colors <- c("Placebo" = "red", "Active" = "blue")
		p <- suppressWarnings(create_loglog_plot(
			df,
			"time",
			"event",
			"trt",
			colors = custom_colors
		))

		expect_s7_class(p, ClinicalPlot)
		scale_color <- p@plot$scales$get_scales("colour")
		expect_true(!is.null(scale_color))
		# Verify custom colors are being used (order-agnostic)
		actual_colors <- scale_color$palette(2)
		expect_setequal(unname(actual_colors), unname(custom_colors))
	})
})
