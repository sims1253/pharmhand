# Tests for GBA/AMNOG SMD utilities
# Tests SMD calculations for baseline balance assessment

library(testthat)
library(pharmhand)
source("fixtures.R")

# =============================================================================
# Tests for calculate_smd() - Continuous Variables
# =============================================================================

describe("calculate_smd()", {
	it("calculates Cohen's d correctly with known values", {
		# Known example: two groups with same SD = 10
		# Group 1: mean = 50, SD = 10, n = 100
		# Group 2: mean = 45, SD = 10, n = 100
		# Expected Cohen's d = (50 - 45) / 10 = 0.5
		result <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 100,
			mean2 = 45,
			sd2 = 10,
			n2 = 100,
			method = "cohens_d"
		)

		expect_equal(result$smd, 0.5, tolerance = 1e-6)
		expect_equal(result$method, "cohens_d")
		expect_true(!is.na(result$ci_lower))
		expect_true(!is.na(result$ci_upper))
		expect_true(!is.na(result$se))
	})

	it("calculates pooled SD correctly with different group SDs", {
		# Group 1: mean = 50, SD = 8, n = 50
		# Group 2: mean = 46, SD = 12, n = 50
		# Pooled SD = sqrt(((49 * 64) + (49 * 144)) / 98) = sqrt(104) = 10.198
		# Expected d = (50 - 46) / 10.198 = 0.3923
		result <- calculate_smd(
			mean1 = 50,
			sd1 = 8,
			n1 = 50,
			mean2 = 46,
			sd2 = 12,
			n2 = 50
		)

		pooled_sd <- sqrt(((49 * 64) + (49 * 144)) / 98)
		expected_d <- 4 / pooled_sd

		expect_equal(result$smd, expected_d, tolerance = 1e-4)
	})

	it("applies Hedges' g small-sample correction", {
		# For small samples, Hedges' g should be smaller than Cohen's d
		result_d <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 20,
			mean2 = 45,
			sd2 = 10,
			n2 = 20,
			method = "cohens_d"
		)

		result_g <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 20,
			mean2 = 45,
			sd2 = 10,
			n2 = 20,
			method = "hedges_g"
		)

		# Hedges' g applies a correction factor < 1 for small samples
		# correction = 1 - 3 / (4*(n1+n2) - 9) = 1 - 3/151 ~= 0.9801
		expected_correction <- 1 - (3 / (4 * (20 + 20) - 9))
		expected_g <- result_d$smd * expected_correction

		expect_equal(result_g$method, "hedges_g")
		expect_lt(abs(result_g$smd), abs(result_d$smd))
		expect_equal(result_g$smd, expected_g, tolerance = 1e-6)
	})

	it("calculates confidence intervals correctly", {
		result <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 100,
			mean2 = 45,
			sd2 = 10,
			n2 = 100,
			conf_level = 0.95
		)

		# CI should bracket the point estimate
		expect_lt(result$ci_lower, result$smd)
		expect_gt(result$ci_upper, result$smd)

		# CI width should be approximately 2 * 1.96 * SE
		ci_width <- result$ci_upper - result$ci_lower
		expected_width <- 2 * qnorm(0.975) * result$se
		expect_equal(ci_width, expected_width, tolerance = 1e-6)
	})

	it("respects custom confidence levels", {
		result_95 <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 100,
			mean2 = 45,
			sd2 = 10,
			n2 = 100,
			conf_level = 0.95
		)

		result_90 <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 100,
			mean2 = 45,
			sd2 = 10,
			n2 = 100,
			conf_level = 0.90
		)

		# 90% CI should be narrower than 95% CI
		ci_width_95 <- result_95$ci_upper - result_95$ci_lower
		ci_width_90 <- result_90$ci_upper - result_90$ci_lower

		expect_lt(ci_width_90, ci_width_95)
	})

	it("returns correct structure", {
		result <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 100,
			mean2 = 45,
			sd2 = 10,
			n2 = 100
		)

		expect_type(result, "list")
		expect_named(result, c("smd", "ci_lower", "ci_upper", "method", "se"))
		expect_type(result$smd, "double")
		expect_type(result$ci_lower, "double")
		expect_type(result$ci_upper, "double")
		expect_type(result$method, "character")
		expect_type(result$se, "double")
	})

	# Input validation tests
	it("errors on negative standard deviation", {
		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = -10,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 100
			),
			"sd1"
		)

		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = 10,
				n1 = 100,
				mean2 = 45,
				sd2 = -10,
				n2 = 100
			),
			"sd2"
		)
	})

	it("errors on zero standard deviation", {
		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = 0,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 100
			),
			"sd1"
		)
	})

	it("errors on sample size less than 2", {
		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = 10,
				n1 = 1,
				mean2 = 45,
				sd2 = 10,
				n2 = 100
			),
			"n1"
		)

		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = 10,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 1
			),
			"n2"
		)
	})

	it("errors on NA inputs", {
		expect_error(
			calculate_smd(
				mean1 = NA,
				sd1 = 10,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 100
			),
			"mean1"
		)

		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = NA,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 100
			),
			"sd1"
		)
	})

	it("errors on invalid conf_level", {
		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = 10,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 100,
				conf_level = 0
			),
			"conf_level"
		)

		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = 10,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 100,
				conf_level = 1
			),
			"conf_level"
		)

		expect_error(
			calculate_smd(
				mean1 = 50,
				sd1 = 10,
				n1 = 100,
				mean2 = 45,
				sd2 = 10,
				n2 = 100,
				conf_level = 1.5
			),
			"conf_level"
		)
	})

	it("handles edge case with very small pooled SD", {
		# When both SDs are very small, pooled SD approaches zero
		# This should return NA with a warning
		expect_warning(
			result <- calculate_smd(
				mean1 = 50,
				sd1 = 1e-16,
				n1 = 100,
				mean2 = 50,
				sd2 = 1e-16,
				n2 = 100
			),
			"essentially zero"
		)

		expect_true(is.na(result$smd))
		expect_true(is.na(result$ci_lower))
		expect_true(is.na(result$ci_upper))
		expect_true(is.na(result$se))
	})

	it("handles identical groups (SMD = 0)", {
		result <- calculate_smd(
			mean1 = 50,
			sd1 = 10,
			n1 = 100,
			mean2 = 50,
			sd2 = 10,
			n2 = 100
		)

		expect_equal(result$smd, 0, tolerance = 1e-10)
		# CI should include 0
		expect_lte(result$ci_lower, 0)
		expect_gte(result$ci_upper, 0)
	})

	it("handles negative SMD (group 1 < group 2)", {
		result <- calculate_smd(
			mean1 = 45,
			sd1 = 10,
			n1 = 100,
			mean2 = 50,
			sd2 = 10,
			n2 = 100
		)

		expect_lt(result$smd, 0)
		expect_equal(result$smd, -0.5, tolerance = 1e-6)
	})
})

# =============================================================================
# Tests for calculate_smd_binary() - Binary/Categorical Variables
# =============================================================================

describe("calculate_smd_binary()", {
	it("calculates arcsine SMD correctly", {
		# Known proportions
		result <- calculate_smd_binary(
			p1 = 0.6,
			n1 = 100,
			p2 = 0.5,
			n2 = 100,
			method = "arcsine"
		)

		# Cohen's h = 2 * (asin(sqrt(p1)) - asin(sqrt(p2)))
		expected_h <- 2 * (asin(sqrt(0.6)) - asin(sqrt(0.5)))

		expect_equal(result$smd, expected_h, tolerance = 1e-3)
		expect_equal(result$method, "arcsine")
	})

	it("calculates logit SMD correctly", {
		result <- calculate_smd_binary(
			p1 = 0.6,
			n1 = 100,
			p2 = 0.4,
			n2 = 100,
			method = "logit"
		)

		# Log OR converted to SMD scale
		log_or <- log((0.6 / 0.4) / (0.4 / 0.6))
		expected_smd <- log_or * sqrt(3) / pi

		expect_equal(result$smd, expected_smd, tolerance = 1e-3)
		expect_equal(result$method, "logit")
	})

	it("calculates raw SMD correctly", {
		result <- calculate_smd_binary(
			p1 = 0.6,
			n1 = 100,
			p2 = 0.4,
			n2 = 100,
			method = "raw"
		)

		# Raw difference standardized by pooled proportion
		p_pooled <- (100 * 0.6 + 100 * 0.4) / 200
		pooled_sd <- sqrt(p_pooled * (1 - p_pooled))
		expected_smd <- (0.6 - 0.4) / pooled_sd

		expect_equal(result$smd, expected_smd, tolerance = 1e-6)
		expect_equal(result$method, "raw")
	})

	it("applies continuity correction for extreme proportions", {
		# When p = 0 or p = 1, continuity correction should be applied
		# The function should not error and should return reasonable values
		result_zero <- calculate_smd_binary(
			p1 = 0,
			n1 = 100,
			p2 = 0.5,
			n2 = 100,
			method = "arcsine"
		)

		result_one <- calculate_smd_binary(
			p1 = 1,
			n1 = 100,
			p2 = 0.5,
			n2 = 100,
			method = "arcsine"
		)

		# Results should be finite and not NA
		expect_true(is.finite(result_zero$smd))
		expect_true(is.finite(result_one$smd))

		# SMD for p1=0 should be negative, p1=1 should be positive
		expect_lt(result_zero$smd, 0)
		expect_gt(result_one$smd, 0)
	})

	it("handles extreme proportions with logit method", {
		# Logit method with p = 0 or 1 would be undefined without correction
		result <- calculate_smd_binary(
			p1 = 0,
			n1 = 100,
			p2 = 0.5,
			n2 = 100,
			method = "logit"
		)

		expect_true(is.finite(result$smd))
	})

	it("returns correct structure", {
		result <- calculate_smd_binary(
			p1 = 0.6,
			n1 = 100,
			p2 = 0.5,
			n2 = 100
		)

		expect_type(result, "list")
		expect_named(result, c("smd", "ci_lower", "ci_upper", "method", "se"))
		expect_type(result$smd, "double")
	})

	it("calculates confidence intervals", {
		result <- calculate_smd_binary(
			p1 = 0.6,
			n1 = 100,
			p2 = 0.5,
			n2 = 100
		)

		expect_lt(result$ci_lower, result$smd)
		expect_gt(result$ci_upper, result$smd)
	})

	# Input validation tests
	it("errors on proportions outside [0, 1]", {
		expect_error(
			calculate_smd_binary(p1 = -0.1, n1 = 100, p2 = 0.5, n2 = 100),
			"p1"
		)

		expect_error(
			calculate_smd_binary(p1 = 1.1, n1 = 100, p2 = 0.5, n2 = 100),
			"p1"
		)

		expect_error(
			calculate_smd_binary(p1 = 0.5, n1 = 100, p2 = -0.1, n2 = 100),
			"p2"
		)
	})

	it("errors on sample size less than 2", {
		expect_error(
			calculate_smd_binary(p1 = 0.5, n1 = 1, p2 = 0.5, n2 = 100),
			"n1"
		)
	})

	it("handles identical proportions (SMD = 0)", {
		result <- calculate_smd_binary(
			p1 = 0.5,
			n1 = 100,
			p2 = 0.5,
			n2 = 100
		)

		expect_equal(result$smd, 0, tolerance = 1e-6)
	})

	it("handles edge case with raw method at boundary pooled proportion", {
		# When all subjects in both groups have same outcome, pooled proportion
		# is 0 or 1, which would make pooled_sd = 0
		expect_warning(
			result <- calculate_smd_binary(
				p1 = 1,
				n1 = 100,
				p2 = 1,
				n2 = 100,
				method = "raw"
			),
			"boundary"
		)

		expect_true(is.na(result$smd))
	})
})

# =============================================================================
# Tests for calculate_smd_from_data()
# =============================================================================

describe("calculate_smd_from_data()", {
	# Create test data with known properties
	set.seed(42)
	test_data <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:200),
		TRT01P = rep(c("Treatment", "Control"), each = 100),
		AGE = c(rnorm(100, mean = 55, sd = 10), rnorm(100, mean = 53, sd = 10)),
		SEX = c(
			sample(c("M", "F"), 100, replace = TRUE, prob = c(0.4, 0.6)),
			sample(c("M", "F"), 100, replace = TRUE, prob = c(0.5, 0.5))
		),
		RACE = c(
			sample(
				c("White", "Black", "Asian"),
				100,
				replace = TRUE,
				prob = c(0.7, 0.2, 0.1)
			),
			sample(
				c("White", "Black", "Asian"),
				100,
				replace = TRUE,
				prob = c(0.6, 0.25, 0.15)
			)
		),
		SCORE = c(
			sample.int(5, 100, replace = TRUE),
			sample.int(5, 100, replace = TRUE)
		),
		stringsAsFactors = FALSE
	)

	it("auto-detects continuous variables", {
		result <- calculate_smd_from_data(
			data = test_data,
			var = "AGE",
			trt_var = "TRT01P",
			ref_group = "Control",
			method = "auto"
		)

		expect_equal(result$var_type, "continuous")
		# Method should be cohens_d for continuous
		expect_equal(result$method, "cohens_d")
	})

	it("auto-detects categorical variables", {
		result <- calculate_smd_from_data(
			data = test_data,
			var = "SEX",
			trt_var = "TRT01P",
			ref_group = "Control",
			method = "auto"
		)

		expect_equal(result$var_type, "categorical")
		# Method should be arcsine for categorical
		expect_equal(result$method, "arcsine")
	})

	it("treats low-cardinality numeric as categorical", {
		# SCORE has only 5 unique values, should be treated as categorical
		result <- calculate_smd_from_data(
			data = test_data,
			var = "SCORE",
			trt_var = "TRT01P",
			ref_group = "Control"
		)

		expect_equal(result$var_type, "categorical")
	})

	it("works with actual data frame", {
		result <- calculate_smd_from_data(
			data = test_data,
			var = "AGE",
			trt_var = "TRT01P",
			ref_group = "Control"
		)

		expect_true(!is.na(result$smd))
		expect_true(!is.na(result$se))
		expect_true(!is.na(result$ci_lower))
		expect_true(!is.na(result$ci_upper))
	})

	it("handles multi-level categorical variables", {
		# RACE has 3 levels
		result <- calculate_smd_from_data(
			data = test_data,
			var = "RACE",
			trt_var = "TRT01P",
			ref_group = "Control"
		)

		# For multi-level categorical, returns maximum absolute SMD
		expect_true(!is.na(result$smd))
		expect_equal(result$var_type, "categorical")
	})

	it("uses first level as reference when ref_group is NULL", {
		result <- calculate_smd_from_data(
			data = test_data,
			var = "AGE",
			trt_var = "TRT01P",
			ref_group = NULL
		)

		# Should work without error, using sorted first value as reference
		expect_true(!is.na(result$smd))
	})

	it("respects specified method for continuous", {
		result_d <- calculate_smd_from_data(
			data = test_data,
			var = "AGE",
			trt_var = "TRT01P",
			method = "cohens_d"
		)

		result_g <- calculate_smd_from_data(
			data = test_data,
			var = "AGE",
			trt_var = "TRT01P",
			method = "hedges_g"
		)

		expect_equal(result_d$method, "cohens_d")
		expect_equal(result_g$method, "hedges_g")
		# Hedges' g should be slightly smaller
		expect_lt(abs(result_g$smd), abs(result_d$smd))
	})

	it("respects specified method for binary", {
		result_arc <- calculate_smd_from_data(
			data = test_data,
			var = "SEX",
			trt_var = "TRT01P",
			method = "arcsine"
		)

		result_logit <- calculate_smd_from_data(
			data = test_data,
			var = "SEX",
			trt_var = "TRT01P",
			method = "logit"
		)

		expect_equal(result_arc$method, "arcsine")
		expect_equal(result_logit$method, "logit")
	})

	it("errors on missing variable", {
		expect_error(
			calculate_smd_from_data(
				data = test_data,
				var = "NONEXISTENT",
				trt_var = "TRT01P"
			),
			"not found"
		)
	})

	it("errors on missing treatment variable", {
		expect_error(
			calculate_smd_from_data(
				data = test_data,
				var = "AGE",
				trt_var = "NONEXISTENT"
			),
			"not found"
		)
	})

	it("errors on invalid reference group", {
		expect_error(
			calculate_smd_from_data(
				data = test_data,
				var = "AGE",
				trt_var = "TRT01P",
				ref_group = "InvalidGroup"
			),
			"not found"
		)
	})

	it("warns on insufficient observations", {
		small_data <- test_data[1:3, ]
		small_data$TRT01P <- c("Treatment", "Control", "Control")

		expect_warning(
			result <- calculate_smd_from_data(
				data = small_data,
				var = "AGE",
				trt_var = "TRT01P"
			),
			"Insufficient"
		)

		expect_true(is.na(result$smd))
	})

	it("reclassifies low-cardinality numeric as continuous with threshold", {
		# SCORE has only 5 unique values, normally categorical
		# But with continuous_threshold = 4, it should be treated as continuous
		result <- calculate_smd_from_data(
			data = test_data,
			var = "SCORE",
			trt_var = "TRT01P",
			ref_group = "Control",
			continuous_threshold = 4
		)

		expect_equal(result$var_type, "continuous")
		expect_equal(result$method, "cohens_d")
	})

	it("warns when more than two treatment groups are present", {
		# Create data with three treatment groups
		multi_trt_data <- test_data
		multi_trt_data$TRT01P <- rep(
			c("Treatment A", "Treatment B", "Control"),
			length.out = nrow(multi_trt_data)
		)

		expect_warning(
			result <- calculate_smd_from_data(
				data = multi_trt_data,
				var = "AGE",
				trt_var = "TRT01P",
				ref_group = "Control"
			),
			"first non-reference group"
		)

		# Should still compute SMD
		expect_true(!is.na(result$smd))
	})
})

# =============================================================================
# Tests for add_smd_to_table()
# =============================================================================

describe("add_smd_to_table()", {
	set.seed(42)
	test_data <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:200),
		TRT01P = rep(c("Treatment", "Control"), each = 100),
		AGE = c(rnorm(100, 55, 10), rnorm(100, 53, 10)),
		WEIGHT = c(rnorm(100, 75, 15), rnorm(100, 74, 14)),
		SEX = c(
			sample(c("M", "F"), 100, replace = TRUE),
			sample(c("M", "F"), 100, replace = TRUE)
		),
		stringsAsFactors = FALSE
	)

	it("adds correct columns to result", {
		result <- add_smd_to_table(
			data = test_data,
			trt_var = "TRT01P",
			vars = c("AGE", "WEIGHT", "SEX")
		)

		expect_true(is.data.frame(result))
		expect_true("variable" %in% names(result))
		expect_true("smd" %in% names(result))
		expect_true("ci" %in% names(result))
		expect_true("imbalanced" %in% names(result))
		expect_true("smd_display" %in% names(result))
		expect_true("var_type" %in% names(result))
	})

	it("returns one row per variable", {
		result <- add_smd_to_table(
			data = test_data,
			trt_var = "TRT01P",
			vars = c("AGE", "WEIGHT", "SEX")
		)

		expect_equal(nrow(result), 3)
		expect_equal(result$variable, c("AGE", "WEIGHT", "SEX"))
	})

	it("flags imbalanced variables at threshold", {
		# Create data with known imbalance
		imbal_data <- test_data
		imbal_data$IMBAL_VAR <- c(rep(1, 100), rep(0, 100))

		result <- add_smd_to_table(
			data = imbal_data,
			trt_var = "TRT01P",
			vars = c("IMBAL_VAR"),
			threshold = 0.1
		)

		# Extreme imbalance should be flagged
		expect_true(result$imbalanced[1])
		expect_true(grepl("\\*", result$smd_display[1]))
	})

	it("does not flag balanced variables", {
		# AGE and WEIGHT should be roughly balanced
		result <- add_smd_to_table(
			data = test_data,
			trt_var = "TRT01P",
			vars = c("AGE"),
			threshold = 0.5 # High threshold
		)

		expect_false(result$imbalanced[1])
		expect_false(grepl("\\*", result$smd_display[1]))
	})

	it("uses custom flag symbol", {
		imbal_data <- test_data
		imbal_data$IMBAL_VAR <- c(rep(1, 100), rep(0, 100))

		result <- add_smd_to_table(
			data = imbal_data,
			trt_var = "TRT01P",
			vars = c("IMBAL_VAR"),
			threshold = 0.1,
			flag_symbol = "+"
		)

		expect_true(grepl("\\+", result$smd_display[1]))
	})

	it("warns on missing variables", {
		expect_warning(
			result <- add_smd_to_table(
				data = test_data,
				trt_var = "TRT01P",
				vars = c("AGE", "NONEXISTENT")
			),
			"not found"
		)

		# Should still return results for valid variables
		expect_equal(nrow(result), 1)
	})

	it("errors when all variables are missing", {
		expect_warning(
			expect_error(
				add_smd_to_table(
					data = test_data,
					trt_var = "TRT01P",
					vars = c("NONEXISTENT1", "NONEXISTENT2")
				),
				"No valid variables"
			),
			"Variables not found in data: NONEXISTENT1, NONEXISTENT2"
		)
	})

	it("formats CI correctly", {
		result <- add_smd_to_table(
			data = test_data,
			trt_var = "TRT01P",
			vars = c("AGE")
		)

		# CI should be formatted as "(lower, upper)"
		expect_true(grepl("^\\(-?[0-9]", result$ci[1]))
		expect_true(grepl("[0-9]\\)$", result$ci[1]))
	})

	it("respects reference group", {
		result1 <- add_smd_to_table(
			data = test_data,
			trt_var = "TRT01P",
			vars = c("AGE"),
			ref_group = "Control"
		)

		result2 <- add_smd_to_table(
			data = test_data,
			trt_var = "TRT01P",
			vars = c("AGE"),
			ref_group = "Treatment"
		)

		# SMD sign should be opposite
		expect_equal(result1$smd, -result2$smd, tolerance = 1e-6)
	})
})

# =============================================================================
# Tests for assess_baseline_balance()
# =============================================================================

describe("assess_baseline_balance()", {
	set.seed(42)
	test_data <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:200),
		TRT01P = rep(c("Treatment", "Control"), each = 100),
		AGE = c(rnorm(100, 55, 10), rnorm(100, 53, 10)),
		WEIGHT = c(rnorm(100, 75, 15), rnorm(100, 74, 14)),
		HEIGHT = c(rnorm(100, 170, 10), rnorm(100, 169, 10)),
		SEX = c(
			sample(c("M", "F"), 100, replace = TRUE, prob = c(0.4, 0.6)),
			sample(c("M", "F"), 100, replace = TRUE, prob = c(0.5, 0.5))
		),
		RACE = c(
			sample(c("White", "Black", "Asian"), 100, replace = TRUE),
			sample(c("White", "Black", "Asian"), 100, replace = TRUE)
		),
		stringsAsFactors = FALSE
	)

	it("creates valid BalanceAssessment object", {
		result <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT"),
			categorical_vars = c("SEX", "RACE"),
			ref_group = "Control"
		)

		expect_true(S7::S7_inherits(result, BalanceAssessment))
	})

	it("computes n_vars property correctly", {
		result <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT"),
			categorical_vars = c("SEX", "RACE")
		)

		expect_equal(result@n_vars, 4L)
	})

	it("computes n_imbalanced property correctly", {
		# Create data with known imbalance
		imbal_data <- test_data
		imbal_data$IMBAL <- c(rep(1, 100), rep(0, 100))

		result <- assess_baseline_balance(
			data = imbal_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE"),
			categorical_vars = c("IMBAL"),
			threshold = 0.1
		)

		# IMBAL should be flagged as imbalanced
		expect_gte(result@n_imbalanced, 1L)
		expect_equal(result@n_imbalanced, length(result@imbalanced_vars))
	})

	it("computes balanced property correctly", {
		# With high threshold, should be balanced
		result_high <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE"),
			threshold = 2.0 # Very high threshold
		)

		expect_true(result_high@balanced)

		# Create data with extreme imbalance
		imbal_data <- test_data
		imbal_data$IMBAL <- c(rep(1, 100), rep(0, 100))

		result_low <- assess_baseline_balance(
			data = imbal_data,
			trt_var = "TRT01P",
			categorical_vars = c("IMBAL"),
			threshold = 0.1
		)

		expect_false(result_low@balanced)
	})

	it("handles mix of continuous and categorical variables", {
		result <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT", "HEIGHT"),
			categorical_vars = c("SEX", "RACE")
		)

		# Check var_type in results
		expect_true(all(
			result@smd_results$var_type %in% c("continuous", "categorical")
		))
		expect_equal(sum(result@smd_results$var_type == "continuous"), 3)
		expect_equal(sum(result@smd_results$var_type == "categorical"), 2)
	})

	it("includes correct summary statistics", {
		result <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT"),
			categorical_vars = c("SEX", "RACE")
		)

		stats <- result@summary_stats

		expect_true("n_vars" %in% names(stats))
		expect_true("n_continuous" %in% names(stats))
		expect_true("n_categorical" %in% names(stats))
		expect_true("n_imbalanced" %in% names(stats))
		expect_true("pct_imbalanced" %in% names(stats))
		expect_true("mean_abs_smd" %in% names(stats))
		expect_true("max_abs_smd" %in% names(stats))

		expect_equal(stats$n_vars, 4)
		expect_equal(stats$n_continuous, 2)
		expect_equal(stats$n_categorical, 2)
	})

	it("stores sample sizes correctly", {
		result <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE")
		)

		expect_equal(result@n_treatment, 100L)
		expect_equal(result@n_control, 100L)
	})

	it("respects threshold parameter", {
		result_strict <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT"),
			threshold = 0.05
		)

		result_lenient <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT"),
			threshold = 0.5
		)

		# More imbalanced vars with strict threshold
		expect_gte(result_strict@n_imbalanced, result_lenient@n_imbalanced)
		expect_equal(result_strict@threshold, 0.05)
		expect_equal(result_lenient@threshold, 0.5)
	})

	it("creates love_plot_data sorted by absolute SMD", {
		result <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT"),
			categorical_vars = c("SEX")
		)

		plot_data <- result@love_plot_data

		# Should be sorted by decreasing absolute SMD
		abs_smds <- abs(plot_data$smd)
		expect_true(all(diff(abs_smds) <= 0))
	})

	it("stores metadata correctly", {
		result <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE"),
			conf_level = 0.90,
			continuous_method = "hedges_g"
		)

		expect_equal(result@metadata$conf_level, 0.90)
		expect_equal(result@metadata$continuous_method, "hedges_g")
		expect_true("created" %in% names(result@metadata))
	})

	it("errors when no variables specified", {
		expect_error(
			assess_baseline_balance(
				data = test_data,
				trt_var = "TRT01P"
			),
			"At least one variable"
		)
	})

	it("warns on missing variables but continues", {
		expect_warning(
			result <- assess_baseline_balance(
				data = test_data,
				trt_var = "TRT01P",
				continuous_vars = c("AGE", "NONEXISTENT")
			),
			"not found"
		)

		# Should still have results for AGE
		expect_equal(result@n_vars, 1L)
	})
})

# =============================================================================
# Tests for create_love_plot()
# =============================================================================

describe("create_love_plot()", {
	set.seed(42)
	test_data <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:200),
		TRT01P = rep(c("Treatment", "Control"), each = 100),
		AGE = c(rnorm(100, 55, 10), rnorm(100, 53, 10)),
		WEIGHT = c(rnorm(100, 75, 15), rnorm(100, 74, 14)),
		SEX = c(
			sample(c("M", "F"), 100, replace = TRUE),
			sample(c("M", "F"), 100, replace = TRUE)
		),
		stringsAsFactors = FALSE
	)

	balance <- assess_baseline_balance(
		data = test_data,
		trt_var = "TRT01P",
		continuous_vars = c("AGE", "WEIGHT"),
		categorical_vars = c("SEX")
	)

	it("returns ClinicalPlot object from BalanceAssessment", {
		plot_result <- create_love_plot(balance)

		expect_true(S7::S7_inherits(plot_result, ClinicalPlot))
		expect_equal(plot_result@type, "love_plot")
	})

	it("contains ggplot object", {
		plot_result <- create_love_plot(balance)

		expect_true(inherits(plot_result@plot, "ggplot"))
	})

	it("works with data frame input", {
		df_input <- data.frame(
			variable = c("Age", "Sex", "Weight"),
			smd = c(0.05, -0.12, 0.08),
			ci_lower = c(-0.02, -0.20, 0.01),
			ci_upper = c(0.12, -0.04, 0.15),
			var_type = c("continuous", "categorical", "continuous")
		)

		plot_result <- create_love_plot(df_input, threshold = 0.1)

		expect_true(S7::S7_inherits(plot_result, ClinicalPlot))
	})

	it("errors on invalid data frame input", {
		bad_df <- data.frame(x = 1:3, y = 4:6)

		expect_error(
			create_love_plot(bad_df),
			"variable.*smd"
		)
	})

	it("sorts by absolute SMD by default", {
		plot_result <- create_love_plot(balance, sort_by = "abs_smd")
		plot_data <- plot_result@data

		# Variable should be a factor with proper ordering
		expect_true(is.factor(plot_data$variable))
	})

	it("respects sort_by parameter", {
		# Test different sort options
		plot_abs <- create_love_plot(balance, sort_by = "abs_smd")
		plot_name <- create_love_plot(balance, sort_by = "name")
		plot_smd <- create_love_plot(balance, sort_by = "smd")
		plot_none <- create_love_plot(balance, sort_by = "none")

		# All should return valid plots
		expect_true(S7::S7_inherits(plot_abs, ClinicalPlot))
		expect_true(S7::S7_inherits(plot_name, ClinicalPlot))
		expect_true(S7::S7_inherits(plot_smd, ClinicalPlot))
		expect_true(S7::S7_inherits(plot_none, ClinicalPlot))
	})

	it("includes metadata about threshold and variables", {
		plot_result <- create_love_plot(balance)

		expect_true("threshold" %in% names(plot_result@metadata))
		expect_true("n_variables" %in% names(plot_result@metadata))
		expect_true("n_imbalanced" %in% names(plot_result@metadata))
	})

	it("uses threshold from BalanceAssessment", {
		balance_custom <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE"),
			threshold = 0.2
		)

		plot_result <- create_love_plot(balance_custom)

		expect_equal(plot_result@metadata$threshold, 0.2)
	})

	it("handles missing CI columns in data frame", {
		df_no_ci <- data.frame(
			variable = c("Age", "Sex"),
			smd = c(0.05, -0.12)
		)

		# Should work without CI columns (no error)
		plot_result <- create_love_plot(df_no_ci)

		expect_true(S7::S7_inherits(plot_result, ClinicalPlot))
	})

	it("handles missing var_type column", {
		df_no_type <- data.frame(
			variable = c("Age", "Sex"),
			smd = c(0.05, -0.12),
			ci_lower = c(-0.02, -0.20),
			ci_upper = c(0.12, -0.04)
		)

		# Should work without var_type (color_by_type will be FALSE)
		plot_result <- create_love_plot(df_no_type)

		expect_true(S7::S7_inherits(plot_result, ClinicalPlot))
	})

	it("respects custom title and labels", {
		plot_result <- create_love_plot(
			balance,
			title = "Custom Title",
			xlab = "Custom X Label"
		)

		expect_equal(plot_result@title, "Custom Title")
		# The plot object should have the title
		expect_equal(plot_result@plot$labels$title, "Custom Title")
		expect_equal(plot_result@plot$labels$x, "Custom X Label")
	})

	it("calculates appropriate height based on variables", {
		# More variables should result in taller plot
		balance_small <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE")
		)

		balance_large <- assess_baseline_balance(
			data = test_data,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT"),
			categorical_vars = c("SEX")
		)

		plot_small <- create_love_plot(balance_small)
		plot_large <- create_love_plot(balance_large)

		# Larger assessment should have taller plot
		expect_lte(plot_small@height, plot_large@height)
	})
})

# =============================================================================
# Tests for BalanceAssessment class validation
# =============================================================================

describe("BalanceAssessment class", {
	it("validates threshold is positive", {
		expect_error(
			BalanceAssessment(threshold = 0),
			"positive"
		)

		expect_error(
			BalanceAssessment(threshold = -0.1),
			"positive"
		)
	})

	it("accepts valid threshold", {
		ba <- BalanceAssessment(threshold = 0.1)
		expect_equal(ba@threshold, 0.1)

		ba2 <- BalanceAssessment(threshold = 0.25)
		expect_equal(ba2@threshold, 0.25)
	})

	it("has correct default values", {
		ba <- BalanceAssessment()

		expect_equal(ba@threshold, 0.1)
		expect_equal(nrow(ba@smd_results), 0)
		expect_equal(length(ba@imbalanced_vars), 0)
		expect_equal(ba@n_treatment, 0L)
		expect_equal(ba@n_control, 0L)
	})

	it("computed properties work correctly", {
		smd_results <- data.frame(
			variable = c("AGE", "SEX", "WEIGHT"),
			smd = c(0.05, 0.15, 0.08),
			imbalanced = c(FALSE, TRUE, FALSE)
		)

		ba <- BalanceAssessment(
			smd_results = smd_results,
			imbalanced_vars = c("SEX"),
			threshold = 0.1
		)

		expect_equal(ba@n_vars, 3L)
		expect_equal(ba@n_imbalanced, 1L)
		expect_false(ba@balanced)

		# With no imbalanced vars
		ba2 <- BalanceAssessment(
			smd_results = smd_results,
			imbalanced_vars = character()
		)

		expect_true(ba2@balanced)
	})
})

# =============================================================================
# Integration tests
# =============================================================================

describe("SMD workflow integration", {
	it("full workflow from data to Love plot", {
		set.seed(123)
		# Create realistic-ish clinical trial data
		n <- 300
		adsl <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:n),
			TRT01P = factor(
				rep(c("Placebo", "Active"), each = n / 2),
				levels = c("Placebo", "Active")
			),
			AGE = c(rnorm(n / 2, 55, 12), rnorm(n / 2, 56, 11)),
			WEIGHT = c(rnorm(n / 2, 78, 18), rnorm(n / 2, 77, 17)),
			BMI = c(rnorm(n / 2, 26, 4), rnorm(n / 2, 25.8, 4.2)),
			SEX = c(
				sample(c("M", "F"), n / 2, replace = TRUE, prob = c(0.55, 0.45)),
				sample(c("M", "F"), n / 2, replace = TRUE, prob = c(0.52, 0.48))
			),
			RACE = c(
				sample(
					c("White", "Black", "Asian", "Other"),
					n / 2,
					replace = TRUE,
					prob = c(0.7, 0.15, 0.1, 0.05)
				),
				sample(
					c("White", "Black", "Asian", "Other"),
					n / 2,
					replace = TRUE,
					prob = c(0.68, 0.17, 0.1, 0.05)
				)
			),
			stringsAsFactors = FALSE
		)

		# Step 1: Assess balance
		balance <- assess_baseline_balance(
			data = adsl,
			trt_var = "TRT01P",
			continuous_vars = c("AGE", "WEIGHT", "BMI"),
			categorical_vars = c("SEX", "RACE"),
			ref_group = "Placebo",
			threshold = 0.1
		)

		expect_true(S7::S7_inherits(balance, BalanceAssessment))
		expect_equal(balance@n_vars, 5L)

		# Step 2: Get SMD table
		smd_table <- add_smd_to_table(
			data = adsl,
			trt_var = "TRT01P",
			vars = c("AGE", "WEIGHT", "BMI", "SEX", "RACE"),
			ref_group = "Placebo"
		)

		expect_equal(nrow(smd_table), 5)
		expect_true(all(c("smd", "ci", "imbalanced") %in% names(smd_table)))

		# Step 3: Create visualization
		love_plot <- create_love_plot(balance)

		expect_true(S7::S7_inherits(love_plot, ClinicalPlot))
		expect_equal(love_plot@type, "love_plot")

		# Check the plot can be rendered (doesn't error)
		expect_no_error(print(love_plot@plot))
	})
})
