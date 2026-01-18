# Tests for Bayesian meta-analysis for few studies (R/meta_bayesian_few.R)
# Issue #157: Bayesian meta-analysis for few studies

# =============================================================================
# bayesian_meta_analysis_few studies tests
# =============================================================================

describe("bayesian_meta_analysis_few", {
	it("returns a BayesianMetaFewResult object", {
		skip_if_brms_unavailable()

		# Use 3 studies - few studies scenario
		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		result <- bayesian_meta_analysis_few(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 1,
			iter = 400,
			warmup = 200
		)

		expect_true("pharmhand::BayesianMetaFewResult" %in% class(result))
	})

	it("handles very few studies (2 studies)", {
		skip_if_brms_unavailable()

		# Only 2 studies - minimum for meta-analysis
		yi <- log(c(0.75, 0.82))
		sei <- c(0.12, 0.15)

		result <- bayesian_meta_analysis_few(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 1,
			iter = 400,
			warmup = 200
		)

		expect_true("pharmhand::BayesianMetaFewResult" %in% class(result))
	})

	it("uses appropriate priors for few studies", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		result <- bayesian_meta_analysis_few(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 1,
			iter = 400,
			warmup = 200
		)

		# Should have conservative priors for few studies
		expect_true("prior_summary" %in% names(S7::props(result)))
		expect_true("few_studies_adjustment" %in% names(S7::props(result)))
	})

	it("generates posterior distributions and credible intervals", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		result <- bayesian_meta_analysis_few(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 1,
			iter = 400,
			warmup = 200
		)

		expect_true("posterior_summary" %in% names(S7::props(result)))
		expect_true("credible_intervals" %in% names(S7::props(result)))
		expect_true("prob_positive" %in% names(S7::props(result)))
		expect_true("prob_negative" %in% names(S7::props(result)))
	})

	it("performs prior sensitivity analysis", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		result <- bayesian_meta_analysis_few(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			prior_sensitivity = TRUE,
			chains = 1,
			iter = 400,
			warmup = 200
		)

		expect_true("prior_sensitivity" %in% names(S7::props(result)))
	})

	it("handles different effect measures", {
		skip_if_brms_unavailable()

		# Test with mean difference
		yi_md <- c(0.5, 0.8, 0.3)
		sei_md <- c(0.2, 0.2, 0.2)

		result_md <- bayesian_meta_analysis_few(
			yi = yi_md,
			sei = sei_md,
			effect_measure = "md",
			chains = 1,
			iter = 400,
			warmup = 200
		)

		expect_true("pharmhand::BayesianMetaFewResult" %in% class(result_md))
	})

	it("provides heterogeneity assessment", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		result <- bayesian_meta_analysis_few(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 1,
			iter = 400,
			warmup = 200
		)

		expect_true("heterogeneity" %in% names(S7::props(result)))
		expect_true("tau_summary" %in% names(S7::props(result)))
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
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)

		result <- bayesian_meta_analysis_few(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 1,
			iter = 400,
			warmup = 200
		)

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
})
