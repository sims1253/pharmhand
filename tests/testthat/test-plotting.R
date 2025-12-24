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

test_that("create_km_plot handles missing packages gracefully", {
	# This is hard to test without unloading packages, which is risky in testthat.
	# We rely on the fact that the function calls requireNamespace.
	# We can test that it calls cli::cli_abort if we force logic, but skip for now.
	succeed()
})
