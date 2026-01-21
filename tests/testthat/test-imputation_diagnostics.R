# Tests for imputation diagnostic plots (R/imputation_diagnostics.R)
# Issue #175: Diagnostic plots for imputation

# =============================================================================
# plot_imputation_convergence tests
# =============================================================================

describe("plot_imputation_convergence", {
	it("returns a ClinicalPlot object", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 123)
		imp <- perform_multiple_imputation(data, m = 3, maxit = 5)

		plot <- plot_imputation_convergence(imp)

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
		expect_equal(plot@type, "imputation_convergence")
	})

	it("validates ImputationResult input", {
		skip_if_not_installed("mice")

		expect_error(
			plot_imputation_convergence("not_valid"),
			"ImputationResult"
		)
	})

	it("accepts variable selection", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 456)
		imp <- perform_multiple_imputation(data, m = 3, maxit = 5)

		plot <- plot_imputation_convergence(imp, vars = "x")

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
	})
})

# =============================================================================
# plot_imputation_distributions tests
# =============================================================================

describe("plot_imputation_distributions", {
	it("returns a ClinicalPlot object", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 789)
		imp <- perform_multiple_imputation(data, m = 3, maxit = 3)

		plot <- plot_imputation_distributions(imp)

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
		expect_equal(plot@type, "imputation_distributions")
	})

	it("validates ImputationResult input", {
		skip_if_not_installed("mice")

		expect_error(
			plot_imputation_distributions("not_valid"),
			"ImputationResult"
		)
	})

	it("accepts variable selection", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 111)
		imp <- perform_multiple_imputation(data, m = 3, maxit = 3)

		plot <- plot_imputation_distributions(imp, vars = "x")

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
	})
})

# =============================================================================
# plot_missing_pattern tests
# =============================================================================

describe("plot_missing_pattern", {
	it("returns a ClinicalPlot object for data frame", {
		data <- data.frame(
			a = c(1, NA, 3, NA, 5),
			b = c(NA, 2, NA, 4, 5),
			c = c(1, 2, 3, 4, 5)
		)

		plot <- plot_missing_pattern(data)

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
		expect_equal(plot@type, "missing_pattern")
	})

	it("returns a ClinicalPlot object for ImputationResult", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 222)
		imp <- perform_multiple_imputation(data, m = 3, maxit = 2)

		plot <- plot_missing_pattern(imp)

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
	})

	it("validates input type", {
		expect_error(
			plot_missing_pattern("not_valid"),
			"data frame|ImputationResult"
		)
	})

	it("handles ImputationResult with no missing data in plot_missing_pattern", {
		skip_if_not_installed("mice")
		data <- data.frame(x = 1:10, y = 1:10)
		result <- suppressWarnings(perform_multiple_imputation(data))
		p <- plot_missing_pattern(result)
		expect_true(S7::S7_inherits(p, ClinicalPlot))
	})
})

# =============================================================================
# create_imputation_report tests
# =============================================================================

describe("create_imputation_report", {
	it("returns a list with diagnostic components", {
		skip_if_not_installed("mice")

		data <- create_imputation_test_data(n = 30, seed = 333)
		imp <- perform_multiple_imputation(data, m = 3, maxit = 3)

		report <- create_imputation_report(imp)

		expect_true(is.list(report))
		expect_true("summary" %in% names(report))
		expect_true("convergence_plot" %in% names(report))
		expect_true("distribution_plot" %in% names(report))
		expect_true("missing_pattern_plot" %in% names(report))
	})
})
