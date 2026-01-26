# Tests for prior specification interface (R/prior_specification.R)
# Issue #174: Prior specification interface

# =============================================================================
# create_prior_specification tests
# =============================================================================

describe("create_prior_specification", {
	it("returns a PriorSpecification object", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10)
		)

		expect_true(S7::S7_inherits(prior, PriorSpecification))
	})

	it("validates distribution type", {
		expect_error(
			create_prior_specification(
				distribution = "invalid_dist",
				parameters = list(mean = 0, sd = 1)
			),
			"distribution"
		)
	})

	it("validates parameters for normal distribution", {
		expect_error(
			create_prior_specification(
				distribution = "normal",
				parameters = list(mean = 0) # Missing sd
			),
			"parameters"
		)
	})

	it("supports normal distribution", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 5)
		)

		expect_equal(prior@distribution, "normal")
		expect_equal(prior@parameters$mean, 0)
		expect_equal(prior@parameters$sd, 5)
	})

	it("supports beta distribution", {
		prior <- create_prior_specification(
			distribution = "beta",
			parameters = list(shape1 = 2, shape2 = 2)
		)

		expect_equal(prior@distribution, "beta")
		expect_equal(prior@parameters$shape1, 2)
		expect_equal(prior@parameters$shape2, 2)
	})

	it("supports gamma distribution", {
		prior <- create_prior_specification(
			distribution = "gamma",
			parameters = list(shape = 2, rate = 1)
		)

		expect_equal(prior@distribution, "gamma")
		expect_equal(prior@parameters$shape, 2)
		expect_equal(prior@parameters$rate, 1)
	})

	it("supports half-cauchy distribution", {
		prior <- create_prior_specification(
			distribution = "half_cauchy",
			parameters = list(location = 0, scale = 0.5)
		)

		expect_equal(prior@distribution, "half_cauchy")
		expect_equal(prior@parameters$scale, 0.5)
	})

	it("supports half-normal distribution", {
		prior <- create_prior_specification(
			distribution = "half_normal",
			parameters = list(sd = 1)
		)

		expect_equal(prior@distribution, "half_normal")
		expect_equal(prior@parameters$sd, 1)
	})

	it("supports exponential distribution", {
		prior <- create_prior_specification(
			distribution = "exponential",
			parameters = list(rate = 1)
		)

		expect_equal(prior@distribution, "exponential")
		expect_equal(prior@parameters$rate, 1)
	})

	it("supports uniform distribution", {
		prior <- create_prior_specification(
			distribution = "uniform",
			parameters = list(min = 0, max = 1)
		)

		expect_equal(prior@distribution, "uniform")
		expect_equal(prior@parameters$min, 0)
		expect_equal(prior@parameters$max, 1)
	})

	it("handles cauchy distribution", {
		prior <- create_prior_specification("cauchy", list(location = 0, scale = 1))
		expect_equal(prior@distribution, "cauchy")
	})

	it("handles student_t distribution", {
		prior <- create_prior_specification(
			"student_t",
			list(df = 3, location = 0, scale = 1)
		)
		expect_equal(prior@distribution, "student_t")
	})

	it("accepts description parameter", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10),
			description = "Vague prior for overall effect"
		)

		expect_equal(prior@description, "Vague prior for overall effect")
	})

	it("accepts domain parameter", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10),
			domain = "overall_effect"
		)

		expect_equal(prior@domain, "overall_effect")
	})

	it("accepts reference parameter", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10),
			reference = "Spiegelhalter et al. 2004"
		)

		expect_equal(prior@reference, "Spiegelhalter et al. 2004")
	})
})

# =============================================================================
# get_default_prior tests
# =============================================================================

describe("get_default_prior", {
	it("returns a PriorSpecification object", {
		prior <- get_default_prior("overall_effect")

		expect_true(S7::S7_inherits(prior, PriorSpecification))
	})

	it("supports overall_effect domain", {
		prior <- get_default_prior("overall_effect")

		expect_equal(prior@distribution, "normal")
		expect_equal(prior@parameters$mean, 0)
		expect_equal(prior@parameters$sd, 10)
	})

	it("supports heterogeneity domain", {
		prior <- get_default_prior("heterogeneity")

		expect_equal(prior@distribution, "half_cauchy")
		expect_equal(prior@parameters$scale, 0.5)
	})

	it("supports treatment_effect domain", {
		prior <- get_default_prior("treatment_effect")

		expect_equal(prior@distribution, "normal")
		expect_equal(prior@parameters$mean, 0)
		expect_equal(prior@parameters$sd, 2)
	})

	it("supports baseline_risk domain", {
		prior <- get_default_prior("baseline_risk")

		expect_equal(prior@distribution, "beta")
		expect_equal(prior@parameters$shape1, 1)
		expect_equal(prior@parameters$shape2, 1)
	})

	it("supports probability domain", {
		prior <- get_default_prior("probability")

		expect_equal(prior@distribution, "beta")
		expect_equal(prior@parameters$shape1, 1)
		expect_equal(prior@parameters$shape2, 1)
	})

	it("supports variance domain", {
		prior <- get_default_prior("variance")

		expect_equal(prior@distribution, "half_cauchy")
		expect_equal(prior@parameters$scale, 1)
	})

	it("supports correlation domain", {
		prior <- get_default_prior("correlation")

		expect_equal(prior@distribution, "uniform")
		expect_equal(prior@parameters$min, -1)
		expect_equal(prior@parameters$max, 1)
	})

	it("supports few_studies domain", {
		prior <- get_default_prior("few_studies")

		expect_equal(prior@distribution, "half_cauchy")
		expect_equal(prior@parameters$scale, 0.25)
	})

	it("returns NULL for unknown domain", {
		expect_warning(
			prior <- get_default_prior("unknown_domain"),
			"Unrecognized domain",
			fixed = TRUE
		)

		expect_null(prior)
	})
})

# =============================================================================
# validate_prior_parameters tests
# =============================================================================

describe("validate_prior_parameters", {
	it("validates normal distribution - valid parameters", {
		result <- validate_prior_parameters("normal", list(mean = 0, sd = 1))
		expect_null(result)
	})

	it("validates normal distribution - missing parameters", {
		result <- validate_prior_parameters("normal", list(mean = 0))
		expect_true(is.character(result))
	})

	it("validates normal distribution - invalid parameters", {
		result <- validate_prior_parameters("normal", list(mean = 0, sd = -1))
		expect_true(is.character(result))
	})

	it("validates beta distribution - valid parameters", {
		result <- validate_prior_parameters("beta", list(shape1 = 2, shape2 = 2))
		expect_null(result)
	})

	it("validates beta distribution - missing parameters", {
		result <- validate_prior_parameters("beta", list(shape1 = 2))
		expect_true(is.character(result))
	})

	it("validates beta distribution - invalid parameters", {
		result <- validate_prior_parameters("beta", list(shape1 = 0, shape2 = 2))
		expect_true(is.character(result))
	})

	it("returns NULL for unknown distribution", {
		result <- validate_prior_parameters("unknown", list(param = 1))
		expect_null(result)
	})
})

# =============================================================================
# summarize_prior_specification tests
# =============================================================================

describe("summarize_prior_specification", {
	it("returns summary for normal prior", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10),
			description = "Vague prior"
		)

		summary <- summarize_prior_specification(prior)

		expect_true(is.data.frame(summary))
		expect_equal(summary$distribution, "normal")
		expect_equal(summary$parameters, "mean=0, sd=10")
		expect_equal(summary$description, "Vague prior")
	})

	it("works without description", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10)
		)

		summary <- summarize_prior_specification(prior)

		expect_true(is.data.frame(summary))
		expect_equal(summary$description, "")
	})
})

# =============================================================================
# create_prior_specification_set tests
# =============================================================================

describe("create_prior_specification_set", {
	it("returns a list of PriorSpecification objects", {
		priors <- create_prior_specification_set(
			list(
				overall = get_default_prior("overall_effect"),
				heterogeneity = get_default_prior("heterogeneity")
			)
		)

		expect_true(is.list(priors))
		expect_equal(length(priors), 2)
		expect_true(S7::S7_inherits(priors$overall, PriorSpecification))
		expect_true(S7::S7_inherits(priors$heterogeneity, PriorSpecification))
	})

	it("validates that all elements are PriorSpecification objects", {
		expect_error(
			create_prior_specification_set(
				list(
					overall = get_default_prior("overall_effect"),
					invalid = "not_a_prior"
				)
			),
			"PriorSpecification"
		)
	})
})

# =============================================================================
# PriorSpecification class tests
# =============================================================================

describe("PriorSpecification class", {
	it("has expected properties", {
		prior <- create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10),
			description = "Test prior",
			domain = "test",
			reference = "Test ref"
		)

		# Check all expected properties exist
		expect_true("distribution" %in% names(S7::props(prior)))
		expect_true("parameters" %in% names(S7::props(prior)))
		expect_true("description" %in% names(S7::props(prior)))
		expect_true("domain" %in% names(S7::props(prior)))
		expect_true("reference" %in% names(S7::props(prior)))
		expect_true("metadata" %in% names(S7::props(prior)))
	})
})
