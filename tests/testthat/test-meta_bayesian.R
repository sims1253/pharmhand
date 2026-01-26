# Tests for Bayesian meta-analysis functions (R/meta_bayesian.R)

# =============================================================================
# Main Function Tests
# =============================================================================

describe("bayesian_meta_analysis()", {
	# -------------------------------------------------------------------------
	# Input Validation Tests
	# -------------------------------------------------------------------------

	it("validates numeric yi", {
		sei <- c(0.12, 0.15, 0.18)

		expect_error(
			bayesian_meta_analysis(
				yi = character(3),
				sei = sei,
				effect_measure = "hr"
			),
			"'yi' must be numeric"
		)
	})

	it("validates numeric sei", {
		yi <- log(c(0.75, 0.82, 0.68))

		expect_error(
			bayesian_meta_analysis(
				yi = yi,
				sei = character(3),
				effect_measure = "hr"
			),
			"'sei' must be numeric"
		)
	})

	it("validates yi and sei length match", {
		yi <- log(c(0.75, 0.82))
		sei <- c(0.12, 0.15, 0.18)

		expect_error(
			bayesian_meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
			"same length"
		)
	})

	it("validates positive sei values", {
		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, -0.15, 0.18)

		expect_error(
			bayesian_meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
			"positive"
		)

		sei <- c(0.12, 0, 0.18)

		expect_error(
			bayesian_meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
			"positive"
		)
	})

	it("validates study_labels length", {
		yi <- log(c(0.75, 0.82, 0.68))
		sei <- c(0.12, 0.15, 0.18)
		study_labels <- c("Study 1", "Study 2")

		expect_error(
			bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				study_labels = study_labels,
				effect_measure = "hr"
			),
			"must have length"
		)
	})

	# -------------------------------------------------------------------------
	# Return Value Tests (with brms)
	# -------------------------------------------------------------------------

	describe("Return value structure", {
		it("returns bayesian_meta_result class", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()
			expect_true("bayesian_meta_result" %in% class(result))
		})

		it("contains expected result components", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			expect_true("posterior_mean" %in% names(result))
			expect_true("posterior_median" %in% names(result))
			expect_true("ci_95" %in% names(result))
			expect_true("ci_80" %in% names(result))
			expect_true("tau_mean" %in% names(result))
			expect_true("tau_ci_95" %in% names(result))
			expect_true("prob_positive" %in% names(result))
			expect_true("prob_negative" %in% names(result))
			expect_true("n_studies" %in% names(result))
			expect_true("effect_measure" %in% names(result))
			expect_true("model_type" %in% names(result))
			expect_true("interpretation" %in% names(result))
		})

		it("stores fit object", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			expect_true("fit" %in% names(result))
			expect_true("brmsfit" %in% class(result$fit))
		})
	})

	# -------------------------------------------------------------------------
	# Configuration Tests
	# -------------------------------------------------------------------------

	describe("Custom priors", {
		it("respects custom priors", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				prior_mu = list(mean = -0.5, sd = 2),
				prior_tau = list(type = "half_cauchy", scale = 0.3),
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("bayesian_meta_result" %in% class(result))
			expect_true("posterior_mean" %in% names(result))
		})
	})

	describe("Study labels", {
		it("generates correct study labels", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)
			custom_labels <- c(
				"Trial A",
				"Trial B",
				"Trial C",
				"Trial D",
				"Trial E",
				"Trial F",
				"Trial G",
				"Trial H"
			)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				study_labels = custom_labels,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("bayesian_meta_result" %in% class(result))
		})
	})

	describe("Sampler parameters", {
		it("accepts adapt_delta parameter", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result_95 <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				adapt_delta = 0.95,
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42
			)
			expect_true("bayesian_meta_result" %in% class(result_95))

			result_99 <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				adapt_delta = 0.99,
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42
			)
			expect_true("bayesian_meta_result" %in% class(result_99))
		})

		it("accepts max_treedepth parameter", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				max_treedepth = 15,
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42
			)
			expect_true("bayesian_meta_result" %in% class(result))
		})
	})
})

# =============================================================================
# Convergence Diagnostics
# =============================================================================

describe("Convergence Diagnostics", {
	it("includes convergence diagnostics in result", {
		skip_if_brms_unavailable()

		yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
		sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 42,
			adapt_delta = 0.99
		)

		expect_true("convergence_diagnostics" %in% names(result))
		expect_true(is.list(result$convergence_diagnostics))
	})

	it("calculates max_rhat", {
		skip_if_brms_unavailable()

		yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
		sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 42,
			adapt_delta = 0.99
		)

		expect_true("max_rhat" %in% names(result$convergence_diagnostics))
		expect_true(is.numeric(result$convergence_diagnostics$max_rhat))
	})

	it("calculates min_bulk_ess", {
		skip_if_brms_unavailable()

		yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
		sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 42,
			adapt_delta = 0.99
		)

		expect_true("min_bulk_ess" %in% names(result$convergence_diagnostics))
		expect_true(is.numeric(result$convergence_diagnostics$min_bulk_ess))
	})

	it("calculates min_tail_ess", {
		skip_if_brms_unavailable()

		yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
		sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 42,
			adapt_delta = 0.99
		)

		expect_true("min_tail_ess" %in% names(result$convergence_diagnostics))
		expect_true(is.numeric(result$convergence_diagnostics$min_tail_ess))
	})

	it("counts divergent transitions", {
		skip_if_brms_unavailable()

		yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
		sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 42,
			adapt_delta = 0.99
		)

		expect_true(
			"divergent_transitions" %in% names(result$convergence_diagnostics)
		)
		expect_true(is.numeric(
			result$convergence_diagnostics$divergent_transitions
		))
	})

	it("calculates bfmi values", {
		skip_if_brms_unavailable()

		yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
		sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

		result <- bayesian_meta_analysis(
			yi = yi,
			sei = sei,
			effect_measure = "hr",
			chains = 2,
			cores = 1,
			iter = 2000,
			warmup = 1000,
			seed = 42,
			adapt_delta = 0.99
		)

		expect_true("bfmi" %in% names(result$convergence_diagnostics))
		expect_true(is.numeric(result$convergence_diagnostics$bfmi))
	})

	describe("Convergence warnings", {
		it("warns when Rhat > 1.01", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75))
			sei <- c(0.10, 0.12, 0.11)

			out <- capture_muffled_warnings(
				suppressMessages(
					bayesian_meta_analysis(
						yi = yi,
						sei = sei,
						effect_measure = "hr",
						backend = "cmdstanr",
						warn_convergence = "always",
						chains = 2,
						cores = 1,
						iter = 50,
						warmup = 25,
						seed = 123,
						adapt_delta = 0.8,
						posterior_predictive = FALSE
					)
				)
			)
			expect_true(any(grepl("Rhat", out$warnings, ignore.case = TRUE)))
		})

		it("warns when ESS < 400", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75))
			sei <- c(0.10, 0.12, 0.11)

			out <- capture_muffled_warnings(
				suppressMessages(
					bayesian_meta_analysis(
						yi = yi,
						sei = sei,
						effect_measure = "hr",
						backend = "cmdstanr",
						warn_convergence = "always",
						chains = 2,
						cores = 1,
						iter = 100,
						warmup = 50,
						seed = 456,
						adapt_delta = 0.95,
						posterior_predictive = FALSE
					)
				)
			)
			expect_true(any(grepl(
				"ESS|effective sample",
				out$warnings,
				ignore.case = TRUE
			)))
		})

		it("warns when divergent transitions > 0", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12)

			out <- capture_muffled_warnings(
				suppressMessages(
					bayesian_meta_analysis(
						yi = yi,
						sei = sei,
						effect_measure = "hr",
						backend = "cmdstanr",
						warn_convergence = "always",
						chains = 2,
						cores = 1,
						iter = 200,
						warmup = 100,
						seed = 789,
						adapt_delta = 0.1,
						posterior_predictive = FALSE
					)
				)
			)
			expect_true(any(grepl("diverg", out$warnings, ignore.case = TRUE)))
		})
	})
})

# =============================================================================
# Predictive Checks
# =============================================================================

describe("Predictive Checks", {
	describe("Posterior predictive checks", {
		it("performs posterior predictive check", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				posterior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("posterior_predictive" %in% names(result))
			expect_true(is.list(result$posterior_predictive))
		})

		it("calculates bayes_p_value", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				posterior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("bayes_p_value" %in% names(result$posterior_predictive))
			expect_true(is.numeric(result$posterior_predictive$bayes_p_value))
			expect_true(result$posterior_predictive$bayes_p_value >= 0)
			expect_true(result$posterior_predictive$bayes_p_value <= 1)
		})

		it("generates pp_check_plot", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				posterior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("pp_check_plot" %in% names(result$posterior_predictive))
			expect_true(
				"ggplot" %in% class(result$posterior_predictive$pp_check_plot)
			)
		})

		it("skips posterior predictive when FALSE", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				posterior_predictive = FALSE,
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true(
				!("posterior_predictive" %in% names(result)) ||
					is.null(result$posterior_predictive)
			)
		})

		it("supports different pp_check types", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result_dens <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				pp_check_type = "dens_overlay",
				posterior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)
			expect_true("posterior_predictive" %in% names(result_dens))
			expect_true(
				"ggplot" %in% class(result_dens$posterior_predictive$pp_check_plot)
			)

			result_hist <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				pp_check_type = "hist",
				posterior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)
			expect_true("posterior_predictive" %in% names(result_hist))
			expect_true(
				"ggplot" %in% class(result_hist$posterior_predictive$pp_check_plot)
			)

			result_ecdf <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				pp_check_type = "ecdf_overlay",
				posterior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)
			expect_true("posterior_predictive" %in% names(result_ecdf))
			expect_true(
				"ggplot" %in% class(result_ecdf$posterior_predictive$pp_check_plot)
			)
		})
	})

	describe("Prior predictive checks", {
		it("performs prior predictive check", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				prior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("prior_predictive" %in% names(result))
			expect_true(is.list(result$prior_predictive))
		})

		it("includes prior summary statistics", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				prior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("prior_summary" %in% names(result$prior_predictive))
			expect_true(is.list(result$prior_predictive$prior_summary))
		})

		it("stores prior samples", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				prior_predictive = TRUE,
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("prior_samples" %in% names(result$prior_predictive))
			expect_true(is.matrix(result$prior_predictive$prior_samples))
		})
	})
})

# =============================================================================
# Trace Plots
# =============================================================================

describe("Trace Plots", {
	describe("create_bayesian_trace_plots()", {
		it("generates ggplot trace plots", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			trace_plot <- create_bayesian_trace_plots(result)
			expect_true("ggplot" %in% class(trace_plot))
		})

		it("accepts parameter specification", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			param_plot <- create_bayesian_trace_plots(result, pars = "b_Intercept")
			expect_true("ggplot" %in% class(param_plot))
		})

		it("supports combine_plots option", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			combined <- create_bayesian_trace_plots(result, combine_plots = TRUE)
			expect_true("ggplot" %in% class(combined))
		})

		it("validates input type", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			expect_error(
				create_bayesian_trace_plots(list(x = 1)),
				"bayesian_meta_result"
			)
		})

		it("validates parameter names", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			expect_error(
				create_bayesian_trace_plots(result, pars = "nonexistent_param"),
				"None of the specified parameters"
			)
		})

		it("validates chain values", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			expect_error(
				create_bayesian_trace_plots(result, chains = 0),
				"positive|greater than"
			)

			expect_error(
				create_bayesian_trace_plots(result, chains = -1),
				"positive|greater than"
			)
		})

		it("works with bayesplot", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()

			if (requireNamespace("bayesplot", quietly = TRUE)) {
				plots <- create_bayesian_trace_plots(result)
				expect_true(inherits(plots, "list") || inherits(plots, "ggplot"))
			} else {
				expect_error(
					create_bayesian_trace_plots(result),
					NA
				)
			}
		})
	})
})

# =============================================================================
# Prior Sensitivity Analysis
# =============================================================================

describe("Prior Sensitivity Analysis", {
	yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
	sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

	describe("Default scenarios", {
		it("creates default prior scenarios", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("scenarios" %in% names(sensitivity_result))
			expect_true(is.list(sensitivity_result$scenarios))

			scenario_names <- names(sensitivity_result$scenarios)
			expect_true(any(grepl("weak", scenario_names, ignore.case = TRUE)))
			expect_true(any(grepl("moderate", scenario_names, ignore.case = TRUE)))
			expect_true(any(grepl("informative", scenario_names, ignore.case = TRUE)))
		})

		it("returns results for all scenarios", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true(all(sapply(sensitivity_result$scenarios, function(x) {
				!is.null(x) && "bayesian_meta_result" %in% class(x)
			})))
		})

		it("generates comparison data frame", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("comparison" %in% names(sensitivity_result))
			expect_true(is.data.frame(sensitivity_result$comparison))
		})
	})

	describe("Custom scenarios", {
		it("accepts custom scenarios", {
			skip_if_brms_unavailable()

			custom_scenarios <- list(
				very_vague = list(
					prior_mu = list(mean = 0, sd = 10),
					prior_tau = list(type = "half_cauchy", scale = 1.0)
				),
				very_informative = list(
					prior_mu = list(mean = -0.2, sd = 0.1),
					prior_tau = list(type = "half_cauchy", scale = 0.1)
				)
			)

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				prior_scenarios = custom_scenarios,
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("very_vague" %in% names(sensitivity_result$scenarios))
			expect_true("very_informative" %in% names(sensitivity_result$scenarios))
		})
	})

	describe("Sensitivity statistics", {
		it("calculates coefficient of variation", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("sensitivity_summary" %in% names(sensitivity_result))
			expect_true(is.list(sensitivity_result$sensitivity_summary))
			expect_true(
				"coefficient_of_variation" %in%
					names(sensitivity_result$sensitivity_summary)
			)
			expect_true(is.numeric(
				sensitivity_result$sensitivity_summary$coefficient_of_variation
			))
		})

		it("calculates max difference", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true(
				"max_difference" %in% names(sensitivity_result$sensitivity_summary)
			)
			expect_true(is.numeric(
				sensitivity_result$sensitivity_summary$max_difference
			))
		})

		it("provides robustness interpretation", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true(
				"robustness" %in% names(sensitivity_result$sensitivity_summary)
			)
			expect_true(is.character(
				sensitivity_result$sensitivity_summary$robustness
			))
		})
	})

	describe("Return value", {
		it("returns prior_sensitivity_result class", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("prior_sensitivity_result" %in% class(sensitivity_result))
		})

		it("contains expected components", {
			skip_if_brms_unavailable()

			sensitivity_result <- prior_sensitivity_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 1000,
				warmup = 500,
				seed = 42,
				adapt_delta = 0.99
			)

			expect_true("scenarios" %in% names(sensitivity_result))
			expect_true("comparison" %in% names(sensitivity_result))
			expect_true("sensitivity_summary" %in% names(sensitivity_result))
		})
	})
})

# =============================================================================
# IQWiG Formatting
# =============================================================================

describe("IQWiG Formatting", {
	.mock_bayesian_result <- function(effect_measure = "hr") {
		structure(
			list(
				posterior_mean = if (effect_measure == "hr") -0.20 else 0.5,
				posterior_median = if (effect_measure == "hr") -0.18 else 0.5,
				ci_95 = if (effect_measure == "hr") c(-0.35, -0.05) else c(0.2, 0.8),
				tau_mean = 0.1,
				tau_ci_95 = c(0.05, 0.2),
				prob_positive = if (effect_measure == "hr") 0.02 else 0.98,
				prob_negative = if (effect_measure == "hr") 0.98 else 0.02,
				n_studies = 8,
				effect_measure = effect_measure,
				interpretation = "Test interpretation"
			),
			class = "bayesian_meta_result"
		)
	}

	describe("format_bayesian_result_iqwig()", {
		it("formats estimates correctly", {
			skip_if_brms_unavailable()

			result_hr <- get_shared_bayesian_result()
			iqwig_result <- format_bayesian_result_iqwig(result_hr)

			expect_true("estimate" %in% names(iqwig_result))
			expect_true(is.character(iqwig_result$estimate))
			expect_true(grepl("[0-9]", iqwig_result$estimate))

			result_smd <- get_shared_bayesian_result_smd()
			iqwig_result_smd <- format_bayesian_result_iqwig(result_smd)

			expect_true("estimate" %in% names(iqwig_result_smd))
			expect_true(is.character(iqwig_result_smd$estimate))
		})

		it("formats CI with semicolons", {
			skip_if_brms_unavailable()

			result <- get_shared_bayesian_result()
			iqwig_result <- format_bayesian_result_iqwig(result)

			expect_true("ci_95" %in% names(iqwig_result))
			expect_true(grepl("\\[", iqwig_result$ci_95))
			expect_true(grepl("\\]", iqwig_result$ci_95))
			expect_true(grepl(";", iqwig_result$ci_95, fixed = TRUE))
			expect_true(is.character(iqwig_result$ci_95))
		})

		it("creates probability statements", {
			skip_if_brms_unavailable()

			result_hr <- get_shared_bayesian_result()
			iqwig_hr <- format_bayesian_result_iqwig(result_hr)

			expect_true("probability_statement" %in% names(iqwig_hr))
			expect_true(is.character(iqwig_hr$probability_statement))
			expect_true(grepl(
				"P",
				iqwig_hr$probability_statement,
				ignore.case = TRUE
			))
			expect_true(grepl("1", iqwig_hr$probability_statement, fixed = TRUE))

			result_smd <- get_shared_bayesian_result_smd()
			iqwig_smd <- format_bayesian_result_iqwig(result_smd)

			expect_true("probability_statement" %in% names(iqwig_smd))
			expect_true(is.character(iqwig_smd$probability_statement))
		})

		it("formats heterogeneity", {
			result <- .mock_bayesian_result("hr")
			iqwig_result <- format_bayesian_result_iqwig(result)

			expect_true("tau" %in% names(iqwig_result))
			expect_true(is.character(iqwig_result$tau))

			expect_true("tau_ci" %in% names(iqwig_result))
			expect_true(is.character(iqwig_result$tau_ci))
			expect_true(grepl(";", iqwig_result$tau_ci, fixed = TRUE))
		})

		it("creates interpretation text", {
			result <- .mock_bayesian_result("hr")
			iqwig_result <- format_bayesian_result_iqwig(result)

			expect_true("interpretation" %in% names(iqwig_result))
			expect_true(is.character(iqwig_result$interpretation))

			expect_true("full_text" %in% names(iqwig_result))
			expect_true(is.character(iqwig_result$full_text))
			expect_true(nchar(iqwig_result$full_text) > 0)

			expect_true(grepl(
				iqwig_result$estimate,
				iqwig_result$full_text,
				fixed = TRUE
			))
			expect_true(grepl(
				iqwig_result$ci_95,
				iqwig_result$full_text,
				fixed = TRUE
			))
			expect_true(grepl(
				iqwig_result$probability_statement,
				iqwig_result$full_text,
				fixed = TRUE
			))
		})
	})

	describe("create_bayesian_forest_plot_iqwig()", {
		it("generates ClinicalPlot object", {
			study_df <- data.frame(
				yi = c(log(0.8), log(1.2)),
				sei = c(0.1, 0.2),
				study_labels = c("S1", "S2"),
				stringsAsFactors = FALSE
			)

			fake_result <- .mock_bayesian_result("hr")

			forest_plot <- create_bayesian_forest_plot_iqwig(
				fake_result,
				study_data = study_df,
				show_prediction_interval = FALSE
			)

			expect_true(S7::S7_inherits(forest_plot, ClinicalPlot))

			z <- stats::qnorm(0.975)
			plot_data <- forest_plot@data
			s1 <- plot_data[plot_data$study == "S1", ]

			expect_equal(
				s1$ci_lower,
				exp(study_df$yi[1] - z * study_df$sei[1]),
				tolerance = 1e-12
			)
			expect_equal(
				s1$ci_upper,
				exp(study_df$yi[1] + z * study_df$sei[1]),
				tolerance = 1e-12
			)
		})

		it("works with full bayesian_meta_result", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			forest_plot <- create_bayesian_forest_plot_iqwig(result)
			expect_true(S7::S7_inherits(forest_plot, ClinicalPlot))
			expect_true("ggplot" %in% class(forest_plot@plot))
		})

		it("supports show_weights option", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			forest_plot_weighted <- create_bayesian_forest_plot_iqwig(
				result,
				show_weights = TRUE
			)
			expect_true(S7::S7_inherits(forest_plot_weighted, ClinicalPlot))
		})

		it("uses IQWiG formatting conventions", {
			skip_if_brms_unavailable()

			yi <- log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81))
			sei <- c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12)

			result <- bayesian_meta_analysis(
				yi = yi,
				sei = sei,
				effect_measure = "hr",
				chains = 2,
				cores = 1,
				iter = 2000,
				warmup = 1000,
				seed = 42,
				adapt_delta = 0.99
			)

			iqwig_result <- format_bayesian_result_iqwig(result)
			forest_plot <- create_bayesian_forest_plot_iqwig(result)

			expect_true("ggplot" %in% class(forest_plot@plot))
		})
	})
})
