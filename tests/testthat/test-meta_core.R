# Tests for meta-analysis core functions (R/meta_core.R)

library(testthat)
library(pharmhand)

# =============================================================================
# meta_analysis function tests
# =============================================================================

test_that("meta_analysis performs fixed-effect analysis", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_s7_class(result, MetaResult)
	expect_equal(result@model, "fixed")
	expect_equal(result@effect_measure, "hr")
	expect_equal(result@n, 5L)
	expect_true(result@estimate > 0) # HR should be positive
	expect_length(result@ci, 2)
})

test_that("meta_analysis performs random-effects analysis", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "random"
	)

	expect_s7_class(result, MetaResult)
	expect_equal(result@model, "random")
	expect_true(!is.null(result@heterogeneity$tau2))
	expect_true(!is.null(result@heterogeneity$I2))
})

test_that("meta_analysis calculates heterogeneity statistics", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6) # Variable effects
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		model = "random"
	)

	het <- result@heterogeneity
	expect_true(het$Q > 0)
	expect_true(het$I2 >= 0 && het$I2 <= 100)
	expect_true(het$tau2 >= 0)
})

test_that("meta_analysis errors with insufficient studies", {
	yi <- c(0.5)
	sei <- c(0.1)

	expect_error(
		meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
		"At least 2 studies"
	)
})

test_that("meta_analysis errors with mismatched lengths", {
	yi <- c(0.5, 0.6)
	sei <- c(0.1)

	expect_error(
		meta_analysis(yi = yi, sei = sei, effect_measure = "hr"),
		"same length"
	)
})

test_that("meta_analysis errors with missing yi or sei", {
	expect_error(
		meta_analysis(yi = NULL, sei = c(0.1, 0.2), effect_measure = "hr"),
		"required"
	)
})

test_that("meta_analysis accepts data frame input", {
	df <- data.frame(
		yi = log(c(0.75, 0.82, 0.68)),
		sei = c(0.12, 0.15, 0.18)
	)

	result <- meta_analysis(data = df, effect_measure = "hr")

	expect_s7_class(result, MetaResult)
	expect_equal(result@n, 3L)
})

test_that("meta_analysis calculates prediction intervals for random effects", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		model = "random",
		prediction = TRUE
	)

	expect_true(!is.null(result@prediction_interval))
	expect_length(result@prediction_interval, 2)
})

test_that("meta_analysis respects different effect measures", {
	yi <- c(0.3, 0.5, 0.7, 0.4, 0.6)
	sei <- c(0.15, 0.2, 0.18, 0.22, 0.17)

	# Test different effect measures (using DL method for stable convergence)
	hr_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		method = "DL"
	)
	or_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "or",
		method = "DL"
	)
	rr_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "rr",
		method = "DL"
	)
	rd_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "rd",
		method = "DL"
	)
	md_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		method = "DL"
	)

	expect_s7_class(hr_result, MetaResult)
	expect_s7_class(or_result, MetaResult)
	expect_s7_class(rr_result, MetaResult)
	expect_s7_class(rd_result, MetaResult)
	expect_s7_class(md_result, MetaResult)
})

test_that("meta_analysis uses different tau2 estimation methods", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result_dl <- meta_analysis(yi = yi, sei = sei, method = "DL")
	result_reml <- meta_analysis(yi = yi, sei = sei, method = "REML")
	result_pm <- meta_analysis(yi = yi, sei = sei, method = "PM")

	expect_s7_class(result_dl, MetaResult)
	expect_s7_class(result_reml, MetaResult)
	expect_s7_class(result_pm, MetaResult)

	# Different methods may produce slightly different tau2 estimates
	expect_true(!is.na(result_dl@heterogeneity$tau2))
	expect_true(!is.na(result_reml@heterogeneity$tau2))
	expect_true(!is.na(result_pm@heterogeneity$tau2))
})

test_that("meta_analysis applies Knapp-Hartung adjustment", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	result_kh <- meta_analysis(yi = yi, sei = sei, knapp_hartung = TRUE)
	result_no_kh <- meta_analysis(yi = yi, sei = sei, knapp_hartung = FALSE)

	expect_s7_class(result_kh, MetaResult)
	expect_s7_class(result_no_kh, MetaResult)

	# Knapp-Hartung should affect the confidence intervals
	expect_true(result_kh@metadata$knapp_hartung)
	expect_false(result_no_kh@metadata$knapp_hartung)
})

test_that("meta_analysis stores individual study results", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		study_labels = c("Study 1", "Study 2", "Study 3")
	)

	expect_true(is.list(result@study_results))
	expect_equal(length(result@study_results), 3)
	expect_true(all(sapply(result@study_results, function(x) {
		S7::S7_inherits(x, ComparisonResult)
	})))
	expect_equal(names(result@study_results), c("Study 1", "Study 2", "Study 3"))
})

test_that("meta_analysis stores study weights", {
	yi <- log(c(0.75, 0.82, 0.68))
	sei <- c(0.12, 0.15, 0.18)

	result <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	expect_true(!is.null(result@weights))
	expect_length(result@weights, 3)
	expect_true(all(result@weights > 0))
	expect_equal(sum(result@weights), 1, tolerance = 0.001)
	# Weights should sum to 1
})

# =============================================================================
# calculate_heterogeneity function tests
# =============================================================================

test_that("calculate_heterogeneity returns all statistics", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	het <- calculate_heterogeneity(yi, sei)

	expect_true(is.list(het))
	expect_true("Q" %in% names(het))
	expect_true("I2" %in% names(het))
	expect_true("tau2" %in% names(het))
	expect_true("interpretation" %in% names(het))
})

test_that("calculate_heterogeneity supports different methods", {
	yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
	sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)

	het_dl <- calculate_heterogeneity(yi, sei, method = "DL")
	het_reml <- calculate_heterogeneity(yi, sei, method = "REML")
	het_pm <- calculate_heterogeneity(yi, sei, method = "PM")

	expect_equal(het_dl$method, "DL")
	expect_equal(het_reml$method, "REML")
	expect_equal(het_pm$method, "PM")

	# All should have heterogeneity statistics
	expect_true(!is.na(het_dl$tau2))
	expect_true(!is.na(het_reml$tau2))
	expect_true(!is.na(het_pm$tau2))
})

# =============================================================================
# leave_one_out function tests
# =============================================================================

test_that("leave_one_out performs sensitivity analysis", {
	yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
	sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
	loo <- leave_one_out(meta_res)

	expect_true(is.data.frame(loo$results))
	expect_equal(nrow(loo$results), 5) # One row per excluded study
})

test_that("leave_one_out identifies influential studies", {
	# Create data where one study has extreme effect
	yi <- c(0.1, 0.15, 0.12, 2.5, 0.13) # 4th study is outlier
	sei <- c(0.1, 0.1, 0.1, 0.1, 0.1)

	meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")
	loo <- leave_one_out(meta_res)

	expect_true("influential_studies" %in% names(loo))
	expect_true("n_influential" %in% names(loo))

	# The 4th study (Study 4) should be identified as influential
	expect_true(any(grepl("4", loo$influential_studies, fixed = TRUE)))
})

test_that("leave_one_out works with direct yi/sei input", {
	yi <- c(0.5, 0.6, 0.4)
	sei <- c(0.1, 0.1, 0.1)

	loo <- leave_one_out(yi = yi, sei = sei)

	expect_true(is.data.frame(loo$results))
	expect_equal(nrow(loo$results), 3)
})
