# Tests for meta-analysis core functions (R/meta_core.R)

# =============================================================================
# meta_analysis function tests
# =============================================================================

test_that("meta_analysis returns MetaResult for fixed-effect model", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_s7_class(result, MetaResult)
})

test_that("meta_analysis sets correct model for fixed-effect", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_equal(result@model, "fixed")
})

test_that("meta_analysis sets correct effect measure", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_equal(result@effect_measure, "hr")
})

test_that("meta_analysis records number of studies", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_equal(result@n, 5L)
})

test_that("meta_analysis produces positive estimate for HR", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_true(result@estimate > 0) # HR should be positive
})

test_that("meta_analysis produces confidence interval", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "fixed"
	)

	expect_length(result@ci, 2)
})

test_that("meta_analysis returns MetaResult for random-effects model", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "random"
	)

	expect_s7_class(result, MetaResult)
})

test_that("meta_analysis sets correct model for random-effects", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "random"
	)

	expect_equal(result@model, "random")
})

test_that("meta_analysis calculates tau2 for random-effects", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "random"
	)

	expect_true(!is.null(result@heterogeneity$tau2))
})

test_that("meta_analysis calculates I2 for random-effects", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		model = "random"
	)

	expect_true(!is.null(result@heterogeneity$I2))
})

test_that("meta_analysis calculates Q statistic", {
	yi <- .meta_yi_md # Variable effects
	sei <- .meta_sei_md

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		model = "random"
	)

	het <- result@heterogeneity
	expect_true(het$Q > 0)
})

test_that("meta_analysis calculates I2 statistic", {
	yi <- .meta_yi_md # Variable effects
	sei <- .meta_sei_md

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		model = "random"
	)

	het <- result@heterogeneity
	expect_true(het$I2 >= 0 && het$I2 <= 100)
})

test_that("meta_analysis calculates tau2 statistic", {
	yi <- .meta_yi_md # Variable effects
	sei <- .meta_sei_md

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		model = "random"
	)

	het <- result@heterogeneity
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

test_that("meta_analysis errors with missing yi", {
	expect_error(
		meta_analysis(yi = NULL, sei = c(0.1, 0.2), effect_measure = "hr"),
		"required"
	)
})

test_that("meta_analysis errors with missing sei", {
	expect_error(
		meta_analysis(yi = c(0.1, 0.2), sei = NULL, effect_measure = "hr"),
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
	yi <- .meta_yi_md
	sei <- .meta_sei_md

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

test_that("meta_analysis works with HR effect measure", {
	yi <- .meta_yi_or
	sei <- .meta_sei_or

	hr_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		method = "DL"
	)

	expect_s7_class(hr_result, MetaResult)
})

test_that("meta_analysis works with OR effect measure", {
	yi <- .meta_yi_or
	sei <- .meta_sei_or

	or_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "or",
		method = "DL"
	)

	expect_s7_class(or_result, MetaResult)
})

test_that("meta_analysis works with RR effect measure", {
	yi <- .meta_yi_or
	sei <- .meta_sei_or

	rr_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "rr",
		method = "DL"
	)

	expect_s7_class(rr_result, MetaResult)
})

test_that("meta_analysis works with RD effect measure", {
	yi <- .meta_yi_or
	sei <- .meta_sei_or

	rd_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "rd",
		method = "DL"
	)

	expect_s7_class(rd_result, MetaResult)
})

test_that("meta_analysis works with MD effect measure", {
	yi <- .meta_yi_or
	sei <- .meta_sei_or

	md_result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "md",
		method = "DL"
	)

	expect_s7_class(md_result, MetaResult)
})

test_that("meta_analysis uses DL method", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	result_dl <- meta_analysis(yi = yi, sei = sei, method = "DL")

	expect_s7_class(result_dl, MetaResult)
	expect_true(!is.na(result_dl@heterogeneity$tau2))
})

test_that("meta_analysis uses REML method", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	result_reml <- meta_analysis(yi = yi, sei = sei, method = "REML")

	expect_s7_class(result_reml, MetaResult)
	expect_true(!is.na(result_reml@heterogeneity$tau2))
})

test_that("meta_analysis uses PM method", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	result_pm <- meta_analysis(yi = yi, sei = sei, method = "PM")

	expect_s7_class(result_pm, MetaResult)
	expect_true(!is.na(result_pm@heterogeneity$tau2))
})

test_that("meta_analysis applies Knapp-Hartung adjustment", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	result_kh <- meta_analysis(yi = yi, sei = sei, knapp_hartung = TRUE)

	expect_s7_class(result_kh, MetaResult)
	expect_true(result_kh@metadata$knapp_hartung)
})

test_that("meta_analysis omits Knapp-Hartung adjustment when FALSE", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	result_no_kh <- meta_analysis(yi = yi, sei = sei, knapp_hartung = FALSE)

	expect_s7_class(result_no_kh, MetaResult)
	expect_false(result_no_kh@metadata$knapp_hartung)
})

test_that("meta_analysis stores individual study results", {
	yi <- .meta_yi_hr_small
	sei <- .meta_sei_hr_small

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		study_labels = c("Study 1", "Study 2", "Study 3")
	)

	expect_true(is.list(result@study_results))
	expect_equal(length(result@study_results), 3)
})

test_that("meta_analysis stores study results as ComparisonResult", {
	yi <- .meta_yi_hr_small
	sei <- .meta_sei_hr_small

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		study_labels = c("Study 1", "Study 2", "Study 3")
	)

	expect_true(all(sapply(result@study_results, function(x) {
		S7::S7_inherits(x, ComparisonResult)
	})))
})

test_that("meta_analysis labels study results correctly", {
	yi <- .meta_yi_hr_small
	sei <- .meta_sei_hr_small

	result <- meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = "hr",
		study_labels = c("Study 1", "Study 2", "Study 3")
	)

	expect_equal(names(result@study_results), c("Study 1", "Study 2", "Study 3"))
})

test_that("meta_analysis stores study weights", {
	yi <- .meta_yi_hr_small
	sei <- .meta_sei_hr_small

	result <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	expect_true(!is.null(result@weights))
	expect_length(result@weights, 3)
})

test_that("meta_analysis has positive study weights", {
	yi <- .meta_yi_hr_small
	sei <- .meta_sei_hr_small

	result <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	expect_true(all(result@weights > 0))
})

test_that("meta_analysis study weights sum to 1", {
	yi <- .meta_yi_hr_small
	sei <- .meta_sei_hr_small

	result <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")

	expect_equal(sum(result@weights), 1, tolerance = 0.001)
})

# =============================================================================
# calculate_heterogeneity function tests
# =============================================================================

test_that("calculate_heterogeneity returns Q statistic", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	het <- calculate_heterogeneity(yi, sei)

	expect_true("Q" %in% names(het))
})

test_that("calculate_heterogeneity returns I2 statistic", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	het <- calculate_heterogeneity(yi, sei)

	expect_true("I2" %in% names(het))
})

test_that("calculate_heterogeneity returns tau2 statistic", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	het <- calculate_heterogeneity(yi, sei)

	expect_true("tau2" %in% names(het))
})

test_that("calculate_heterogeneity returns interpretation", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	het <- calculate_heterogeneity(yi, sei)

	expect_true("interpretation" %in% names(het))
})

test_that("calculate_heterogeneity supports DL method", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	het_dl <- calculate_heterogeneity(yi, sei, method = "DL")

	expect_equal(het_dl$method, "DL")
	expect_true(!is.na(het_dl$tau2))
})

test_that("calculate_heterogeneity supports REML method", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	het_reml <- calculate_heterogeneity(yi, sei, method = "REML")

	expect_equal(het_reml$method, "REML")
	expect_true(!is.na(het_reml$tau2))
})

test_that("calculate_heterogeneity supports PM method", {
	yi <- .meta_yi_md
	sei <- .meta_sei_md

	het_pm <- calculate_heterogeneity(yi, sei, method = "PM")

	expect_equal(het_pm$method, "PM")
	expect_true(!is.na(het_pm$tau2))
})

# =============================================================================
# leave_one_out function tests
# =============================================================================

test_that("leave_one_out performs sensitivity analysis", {
	yi <- .meta_yi_hr
	sei <- .meta_sei_hr

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
