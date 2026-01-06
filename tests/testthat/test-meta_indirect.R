# Tests for indirect comparison functions (R/meta_indirect.R)

library(testthat)
library(pharmhand)

# =============================================================================
# indirect_comparison function tests
# =============================================================================

test_that("indirect_comparison calculates Bucher estimate", {
	result <- indirect_comparison(
		effect_ab = log(0.75), # A vs B
		se_ab = 0.12,
		effect_bc = log(0.85), # B vs C
		se_bc = 0.10,
		effect_measure = "hr"
	)

	expect_s7_class(result, ComparisonResult)
	expect_equal(result@effect_measure, "hr")
	expect_true(result@estimate > 0)
})

test_that("indirect_comparison handles different effect measures", {
	# HR
	hr_result <- indirect_comparison(
		effect_ab = log(0.75),
		se_ab = 0.12,
		effect_bc = log(0.85),
		se_bc = 0.10,
		effect_measure = "hr"
	)
	expect_s7_class(hr_result, ComparisonResult)
	expect_true(hr_result@estimate > 0)

	# OR
	or_result <- indirect_comparison(
		effect_ab = log(0.5),
		se_ab = 0.15,
		effect_bc = log(0.6),
		se_bc = 0.12,
		effect_measure = "or"
	)
	expect_s7_class(or_result, ComparisonResult)

	# MD
	md_result <- indirect_comparison(
		effect_ab = -5.0,
		se_ab = 1.0,
		effect_bc = -3.0,
		se_bc = 0.8,
		effect_measure = "md"
	)
	expect_s7_class(md_result, ComparisonResult)
	expect_true(md_result@estimate < 0) # A vs C should be negative
})

test_that("indirect_comparison validates standard errors", {
	expect_error(
		indirect_comparison(
			effect_ab = log(0.75),
			se_ab = 0, # Invalid: SE = 0
			effect_bc = log(0.85),
			se_bc = 0.10,
			effect_measure = "hr"
		),
		"positive"
	)

	expect_error(
		indirect_comparison(
			effect_ab = log(0.75),
			se_ab = -0.1, # Invalid: negative SE
			effect_bc = log(0.85),
			se_bc = 0.10,
			effect_measure = "hr"
		),
		"positive"
	)
})

# =============================================================================
# compare_direct_indirect function tests
# =============================================================================

test_that("compare_direct_indirect tests consistency", {
	# Direct comparison (A vs C)
	direct <- indirect_comparison(
		effect_ab = log(0.75),
		se_ab = 0.12,
		effect_bc = log(0.85),
		se_bc = 0.10,
		effect_measure = "hr"
	)

	# Indirect comparison through B
	indirect <- indirect_comparison(
		effect_ab = log(0.78),
		se_ab = 0.13,
		effect_bc = log(0.82),
		se_bc = 0.11,
		effect_measure = "hr"
	)

	result <- compare_direct_indirect(direct, indirect, effect_measure = "hr")

	expect_true(is.list(result))
	expect_true("direct_estimate" %in% names(result))
	expect_true("indirect_estimate" %in% names(result))
	expect_true("inconsistency_test" %in% names(result))
	expect_true("pooled" %in% names(result))
})

# =============================================================================
# assess_transitivity function tests
# =============================================================================

test_that("assess_transitivity evaluates population balance", {
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
		treatment = c("A", "B", "B", "C", "A", "C"),
		mean_age = c(55, 55, 58, 58, 52, 52),
		pct_male = c(60, 60, 65, 65, 55, 55),
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("mean_age", "pct_male"),
		continuous_vars = c("mean_age", "pct_male")
	)

	expect_true(is.list(result))
	expect_true("summaries" %in% names(result))
	expect_true("overall_assessment" %in% names(result))
	expect_equal(result$n_treatments, 3)
})

test_that("assess_transitivity handles categorical characteristics", {
	# Test with only categorical variables
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2"),
		treatment = c("A", "B", "A", "B"),
		region = c("US", "US", "EU", "EU"),
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("region"),
		continuous_vars = character(0) # No continuous vars
	)

	expect_true(is.list(result))
	expect_true("summaries" %in% names(result))
})

test_that("assess_transitivity handles insufficient variation", {
	# Create data where all means are the same - no variation
	study_chars <- data.frame(
		study_id = c("S1", "S1", "S2", "S2"),
		treatment = c("A", "B", "A", "B"),
		mean_age = c(55, 55, 55, 55), # No variation
		stringsAsFactors = FALSE
	)

	result <- assess_transitivity(
		study_characteristics = study_chars,
		char_vars = c("mean_age"),
		continuous_vars = c("mean_age")
	)

	expect_true(is.list(result))
	expect_true("overall_assessment" %in% names(result))
})

test_that("assess_transitivity validates inputs", {
	# Missing study_id column
	bad_data <- data.frame(
		treatment = c("A", "B"),
		mean_age = c(55, 55)
	)

	expect_error(
		assess_transitivity(bad_data, char_vars = "mean_age"),
		"study_id"
	)

	# Missing characteristic variable
	study_data <- data.frame(
		study_id = c("S1", "S2"),
		treatment = c("A", "B"),
		mean_age = c(55, 58)
	)

	expect_error(
		assess_transitivity(study_data, char_vars = "nonexistent"),
		"not found"
	)
})
