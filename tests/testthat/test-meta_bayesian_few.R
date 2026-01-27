# Tests for Bayesian meta-analysis for few studies (R/meta_bayesian_few.R)
# Issue #157: Bayesian meta-analysis for few studies

# =============================================================================
# bayesian_meta_analysis_few tests
# =============================================================================

describe("bayesian_meta_analysis_few", {
	it("returns a BayesianMetaFewResult object", {
		skip_if_brms_unavailable()

		# Use 3 studies - few studies scenario
		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		expect_warning(
			result <- bayesian_meta_analysis_few(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 1,
				iter = 400,
				warmup = 200,
				prior_sensitivity = FALSE
			),
			"Very few studies",
			fixed = TRUE
		)

		expect_true(S7::S7_inherits(result, BayesianMetaFewResult))
	})

	it("handles very few studies (2 studies)", {
		skip_if_brms_unavailable()

		# Only 2 studies - minimum for meta-analysis
		yi <- log(c(0.75, 0.82))
		sei <- c(0.12, 0.15)

		expect_warning(
			result <- bayesian_meta_analysis_few(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 1,
				iter = 400,
				warmup = 200,
				prior_sensitivity = FALSE
			),
			"Very few studies",
			fixed = TRUE
		)

		expect_true(S7::S7_inherits(result, BayesianMetaFewResult))
	})

	it("uses appropriate priors for few studies", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		expect_warning(
			result <- bayesian_meta_analysis_few(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 1,
				iter = 400,
				warmup = 200,
				prior_sensitivity = FALSE
			),
			"Very few studies",
			fixed = TRUE
		)

		expect_true(!is.null(result))
	})

	it("generates posterior distributions and credible intervals", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		expect_warning(
			result <- bayesian_meta_analysis_few(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 1,
				iter = 400,
				warmup = 200,
				prior_sensitivity = FALSE
			),
			"Very few studies",
			fixed = TRUE
		)

		expect_named(result@posterior_summary)
	})

	it("performs prior sensitivity analysis", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		expect_warning(
			result <- bayesian_meta_analysis_few(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				prior_sensitivity = TRUE,
				chains = 1,
				iter = 400,
				warmup = 200
			),
			"Very few studies",
			fixed = TRUE
		)

		expect_true("prior_sensitivity" %in% names(S7::props(result)))
	})

	it("handles different effect measures", {
		skip_if_brms_unavailable()

		# Test with mean difference
		yi_md <- c(0.5, 0.8, 0.3)
		sei_md <- c(0.2, 0.2, 0.2)

		expect_warning(
			result_md <- bayesian_meta_analysis_few(
				yi = yi_md,
				sei = sei_md,
				effect_measure = "md",
				chains = 1,
				iter = 400,
				warmup = 200,
				prior_sensitivity = FALSE
			),
			"Very few studies",
			fixed = TRUE
		)

		expect_true(S7::S7_inherits(result_md, BayesianMetaFewResult))
	})

	it("provides heterogeneity assessment", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		expect_warning(
			result <- bayesian_meta_analysis_few(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 1,
				iter = 400,
				warmup = 200,
				prior_sensitivity = FALSE
			),
			"Very few studies",
			fixed = TRUE
		)

		expect_true(is.list(result@heterogeneity))
	})

	it("errors when too few studies for analysis", {
		skip_if_brms_unavailable()

		# Single study - too few
		yi <- log(c(0.75))
		sei <- c(0.12)

		expect_error(
			bayesian_meta_analysis_few(
				yi = yi,
				sei = sei,
				effect_measure = "hr"
			),
			"At least 2 studies are required for meta-analysis"
		)
	})
})

# =============================================================================
# BayesianMetaFewResult class tests
# =============================================================================

describe("BayesianMetaFewResult class", {
	it("has expected properties", {
		result <- suppressWarnings(get_shared_bayesian_few_result())

		# Check all expected properties exist
		props <- names(S7::props(result))
		expect_true("posterior_summary" %in% props)
		expect_true("credible_intervals" %in% props)
		expect_true("prior_summary" %in% props)
		expect_true("few_studies_adjustment" %in% props)
		expect_true("prob_positive" %in% props)
		expect_true("prob_negative" %in% props)
		expect_true("heterogeneity" %in% props)
		expect_true("prior_sensitivity" %in% props)
	})

	it("contains valid posterior summary", {
		result <- suppressWarnings(get_shared_bayesian_few_result())

		summary <- result@posterior_summary
		expect_s3_class(summary, "data.frame")
		expect_true(nrow(summary) >= 2) # Overall effect and tau
		expect_true("parameter" %in% names(summary))
		expect_true("mean" %in% names(summary))
	})

	it("contains valid credible intervals", {
		result <- suppressWarnings(get_shared_bayesian_few_result())

		ci <- result@credible_intervals
		expect_s3_class(ci, "data.frame")
		expect_true(nrow(ci) >= 2)
		expect_true("ci_lower" %in% names(ci))
		expect_true("ci_upper" %in% names(ci))
	})

	it("contains probability values", {
		result <- suppressWarnings(get_shared_bayesian_few_result())

		expect_true(result@prob_positive >= 0 && result@prob_positive <= 1)
		expect_true(result@prob_negative >= 0 && result@prob_negative <= 1)
		expect_true(abs(result@prob_positive + result@prob_negative - 1) < 0.01)
	})

	it("stores heterogeneity metrics", {
		result <- suppressWarnings(get_shared_bayesian_few_result())

		het <- result@heterogeneity
		expect_true(is.list(het))
		expect_true("tau2_mean" %in% names(het))
		expect_true("i2_mean" %in% names(het))
	})
})

# =============================================================================
# summary_bayesian_few tests
# =============================================================================

describe("summary_bayesian_few", {
	it("returns a summary list with posterior data", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		summ <- summary_bayesian_few(result)

		expect_true(is.list(summ))
		expect_true("posterior" %in% names(summ))
		expect_s3_class(summ$posterior, "data.frame")
	})

	it("formats values with specified digits", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		summ_3 <- summary_bayesian_few(result, digits = 3)
		summ_2 <- summary_bayesian_few(result, digits = 2)

		# Check that digits parameter affects output with specific assertions
		# Find the Posterior Mean column and check formatting
		expect_true(!identical(summ_3, summ_2))

		# Assert that summ_3 contains a value formatted with three decimal places
		# while summ_2 contains the same value formatted with two decimal places
		posterior_vals_3 <- summ_3$posterior$`Posterior Mean`
		posterior_vals_2 <- summ_2$posterior$`Posterior Mean`

		# Convert to character to check decimal places
		char_vals_3 <- as.character(posterior_vals_3)
		char_vals_2 <- as.character(posterior_vals_2)

		# At least one value should have 3 decimal places in summ_3
		expect_true(any(grepl("\\.[0-9]{3}\\b", char_vals_3)))

		# The corresponding value in summ_2 should have 2 decimal places
		expect_true(any(grepl("\\.[0-9]{2}\\b", char_vals_2)))
	})

	it("includes credible interval column", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		summ <- summary_bayesian_few(result)

		expect_true("95% Credible Interval" %in% names(summ$posterior))
	})

	it("includes required summary statistics", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		summ <- summary_bayesian_few(result)

		post <- summ$posterior
		expect_true("Parameter" %in% names(post))
		expect_true("Posterior Mean" %in% names(post))
		expect_true("Posterior Median" %in% names(post))
		expect_true("Posterior SD" %in% names(post))
	})
})

# =============================================================================
# plot_bayesian_few tests
# =============================================================================

describe("plot_bayesian_few", {
	it("returns a ClinicalPlot object", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		p <- plot_bayesian_few(result)

		expect_true(S7::S7_inherits(p, ClinicalPlot))
		expect_true(!is.null(p@plot))
	})

	it("creates valid ggplot object", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		p <- plot_bayesian_few(result)

		expect_s3_class(p@plot, "ggplot")
	})
})

# =============================================================================
# create_bayesian_few_table tests
# =============================================================================

describe("create_bayesian_few_table", {
	it("returns a ClinicalTable object", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		tab <- create_bayesian_few_table(result)

		expect_true(S7::S7_inherits(tab, ClinicalTable))
		expect_true(!is.null(tab@flextable))
	})

	it("includes custom footnotes", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		custom_fn <- c("Custom footnote 1", "Custom footnote 2")
		tab <- create_bayesian_few_table(result, footnotes = custom_fn)

		# Check that table has custom footnotes
		expect_true(S7::S7_inherits(tab, ClinicalTable))

		# Extract footnotes from the table
		footnotes <- tab@footnotes

		# Assert that each custom footnote appears in the footnotes
		expect_true(length(footnotes) >= length(custom_fn))
		for (fn in custom_fn) {
			expect_true(any(grepl(fn, footnotes, fixed = TRUE)))
		}
	})

	it("uses custom title", {
		result <- suppressWarnings(get_shared_bayesian_few_result())
		custom_title <- "Custom Bayesian Analysis Table"
		tab <- create_bayesian_few_table(result, title = custom_title)

		expect_equal(tab@title, custom_title)
	})
})
