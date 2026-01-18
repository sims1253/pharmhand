# Tests for MCMC diagnostics (R/mcmc_diagnostics.R)
# Issue #171: MCMC diagnostics (trace, density, Gelman-Rubin, ESS)

# =============================================================================
# plot_mcmc_trace tests
# =============================================================================

describe("plot_mcmc_trace", {
	it("returns a ClinicalPlot object", {
		skip_if_brms_unavailable()

		# Create a simple Bayesian model
		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		plot <- plot_mcmc_trace(result$fit)

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
		expect_equal(plot@type, "mcmc_trace")
	})

	it("validates brmsfit input", {
		expect_error(
			plot_mcmc_trace("not_a_brmsfit"),
			"brmsfit"
		)
	})

	it("accepts parameter selection", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		plot <- plot_mcmc_trace(result$fit, parameters = "b_Intercept")

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
	})

	it("accepts chain selection", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		plot <- plot_mcmc_trace(result$fit, chains = 1)

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
	})
})

# =============================================================================
# plot_mcmc_density tests
# =============================================================================

describe("plot_mcmc_density", {
	it("returns a ClinicalPlot object", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		plot <- plot_mcmc_density(result$fit)

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
		expect_equal(plot@type, "mcmc_density")
	})

	it("validates brmsfit input", {
		expect_error(
			plot_mcmc_density("not_a_brmsfit"),
			"brmsfit"
		)
	})

	it("accepts parameter selection", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		plot <- plot_mcmc_density(result$fit, parameters = "b_Intercept")

		expect_true(S7::S7_inherits(plot, ClinicalPlot))
	})
})

# =============================================================================
# calculate_gelman_rubin tests
# =============================================================================

describe("calculate_gelman_rubin", {
	it("returns Gelman-Rubin diagnostic values", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		rhat_values <- calculate_gelman_rubin(result$fit)

		expect_true(is.numeric(rhat_values))
		expect_true(all(rhat_values > 0))
		expect_true(all(rhat_values < 5)) # Reasonable upper bound
	})

	it("validates brmsfit input", {
		expect_error(
			calculate_gelman_rubin("not_a_brmsfit"),
			"brmsfit"
		)
	})

	it("returns R-hat values close to 1 for converged chains", {
		skip_if_brms_unavailable()

		# Use more iterations for better convergence
		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 2000,
			warmup = 1000,
			seed = 123
		)

		rhat_values <- calculate_gelman_rubin(result$fit)

		# R-hat should be close to 1 for converged chains
		expect_true(all(rhat_values < 1.1)) # Conservative threshold
	})
})

# =============================================================================
# calculate_effective_sample_size tests
# =============================================================================

describe("calculate_effective_sample_size", {
	it("returns effective sample size values", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		ess_values <- calculate_effective_sample_size(result$fit)

		expect_true(is.numeric(ess_values))
		expect_true(all(ess_values > 0))
	})

	it("validates brmsfit input", {
		expect_error(
			calculate_effective_sample_size("not_a_brmsfit"),
			"brmsfit"
		)
	})

	it("returns ESS values less than total samples", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		ess_values <- calculate_effective_sample_size(result$fit)
		total_samples <- (1000 - 500) * 2 # (iter - warmup) * chains

		# ESS should be less than or equal to total samples
		expect_true(all(ess_values <= total_samples))
	})
})

# =============================================================================
# assess_mcmc_convergence tests
# =============================================================================

describe("assess_mcmc_convergence", {
	it("returns convergence assessment summary", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		assessment <- assess_mcmc_convergence(result$fit)

		expect_true(is.list(assessment))
		expect_true("convergence_summary" %in% names(assessment))
		expect_true("rhat_values" %in% names(assessment))
		expect_true("ess_values" %in% names(assessment))
		expect_true("recommendations" %in% names(assessment))
	})

	it("validates brmsfit input", {
		expect_error(
			assess_mcmc_convergence("not_a_brmsfit"),
			"brmsfit"
		)
	})

	it("provides convergence recommendations", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		assessment <- assess_mcmc_convergence(result$fit)

		expect_true(is.character(assessment$recommendations))
		expect_true(nchar(assessment$recommendations) > 0)
	})
})

# =============================================================================
# create_mcmc_diagnostics_report tests
# =============================================================================

describe("create_mcmc_diagnostics_report", {
	it("returns comprehensive diagnostics report", {
		skip_if_brms_unavailable()

		yi <- log(c(0.75, 0.82, 0.68, 0.91))
		sei <- c(0.12, 0.15, 0.18, 0.14)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			iter = 1000,
			warmup = 500
		)

		report <- create_mcmc_diagnostics_report(result$fit)

		expect_true(is.list(report))
		expect_true("trace_plots" %in% names(report))
		expect_true("density_plots" %in% names(report))
		expect_true("convergence_assessment" %in% names(report))
		expect_true("diagnostic_summary" %in% names(report))
	})

	it("validates brmsfit input", {
		expect_error(
			create_mcmc_diagnostics_report("not_a_brmsfit"),
			"brmsfit"
		)
	})
})

# =============================================================================
# Helper test functions
# =============================================================================

# Custom skip function for brms availability
skip_if_brms_unavailable <- function() {
	if (!requireNamespace("brms", quietly = TRUE)) {
		skip("brms package not available")
	}
}
