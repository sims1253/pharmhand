# Tests for R/efficacy_tte.R
library(testthat)
library(pharmhand)

test_that("create_tte_summary_table works with basic data", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	tbl <- create_tte_summary_table(adtte)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "tte_summary")
	expect_true("Statistic" %in% names(tbl@data))
	expect_true(any(grepl("Median", tbl@data$Statistic, fixed = TRUE)))
	expect_true(any(grepl("Events", tbl@data$Statistic, fixed = TRUE)))
})

test_that("create_tte_summary_table with landmarks", {
	skip_if_not_installed("survival")

	# Use fixed times to ensure unique landmark matches (no duplicates at 12 or 24)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(
			# Placebo: times spread across range, none at landmarks 12 or 24
			c(
				3,
				5,
				7,
				8,
				9,
				10,
				11,
				13,
				14,
				15,
				16,
				17,
				18,
				19,
				20,
				21,
				22,
				23,
				25,
				26
			),
			# Active: times spread across range, none at landmarks 12 or 24
			c(
				4,
				6,
				8,
				10,
				11,
				13,
				14,
				15,
				16,
				17,
				18,
				19,
				20,
				21,
				22,
				23,
				24,
				27,
				28,
				30
			)
		),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	tbl <- create_tte_summary_table(
		adtte,
		landmarks = c(12, 24),
		time_unit = "months"
	)

	expect_s7_class(tbl, ClinicalTable)
	expect_true(any(grepl("12-months Rate", tbl@data$Statistic, fixed = TRUE)))
	expect_true(any(grepl("24-months Rate", tbl@data$Statistic, fixed = TRUE)))
})

test_that("create_tte_summary_table works with ADaMData", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte_df <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		SAFFL = "Y",
		stringsAsFactors = FALSE
	)

	adam <- ADaMData(data = adtte_df, population = "SAF")
	tbl <- create_tte_summary_table(adam)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "tte_summary")
})

test_that("create_tte_summary_table includes HR for two-arm studies", {
	skip_if_not_installed("survival")

	set.seed(42)
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:40),
		TRT01P = rep(c("Placebo", "Active"), each = 20),
		AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
		CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	tbl <- create_tte_summary_table(adtte)

	expect_true(any(grepl("HR", tbl@data$Statistic, fixed = TRUE)))
	expect_true(any(grepl("p-value", tbl@data$Statistic, fixed = TRUE)))
})

test_that("create_tte_summary_table warns on PH violations", {
	skip_if_not_installed("survival")

	set.seed(123)
	n <- 400
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("A", "B"), each = n / 2),
		TIME = c(
			rweibull(n / 2, shape = 1, scale = 10),
			rweibull(n / 2, shape = 2.5, scale = 10)
		),
		EVENT = rep(1, n),
		stringsAsFactors = FALSE
	)

	expect_warning(
		{
			tbl <- create_tte_summary_table(
				adtte,
				time_var = "TIME",
				event_var = "EVENT",
				trt_var = "TRT01P"
			)
		},
		"Proportional hazards assumption may be violated"
	)

	expect_true(is.list(tbl@metadata$ph_test))
	expect_true("results" %in% names(tbl@metadata$ph_test))
	expect_true(tbl@metadata$ph_test$violation)
})

test_that("create_tte_summary_table check_ph can suppress warnings", {
	skip_if_not_installed("survival")

	set.seed(123)
	n <- 400
	adtte <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("A", "B"), each = n / 2),
		TIME = c(
			rweibull(n / 2, shape = 1, scale = 10),
			rweibull(n / 2, shape = 2.5, scale = 10)
		),
		EVENT = rep(1, n),
		stringsAsFactors = FALSE
	)

	expect_no_warning({
		tbl <- create_tte_summary_table(
			adtte,
			time_var = "TIME",
			event_var = "EVENT",
			trt_var = "TRT01P",
			check_ph = FALSE
		)
	})

	expect_null(tbl@metadata$ph_test)
})

test_that("test_ph_assumption flags PH violations", {
	skip_if_not_installed("survival")

	set.seed(123)
	n <- 400
	ph_data <- data.frame(
		TIME = c(
			rweibull(n / 2, shape = 1, scale = 10),
			rweibull(n / 2, shape = 2.5, scale = 10)
		),
		EVENT = rep(1, n),
		TRT01P = rep(c("A", "B"), each = n / 2),
		stringsAsFactors = FALSE
	)

	result <- test_ph_assumption(
		data = ph_data,
		time_var = "TIME",
		event_var = "EVENT",
		trt_var = "TRT01P"
	)

	expect_true(result$violation)
	expect_true(all(
		c(
			"variable",
			"rho",
			"chisq",
			"p_value",
			"violation"
		) %in%
			names(result$results)
	))
	expect_true(any(result$results$violation))
	expect_true(is.numeric(result$global_test))
})

test_that("test_ph_assumption handles proportional hazards data", {
	skip_if_not_installed("survival")

	set.seed(456)
	n <- 400
	ph_data <- data.frame(
		TIME = c(
			rweibull(n / 2, shape = 1.2, scale = 10),
			rweibull(n / 2, shape = 1.2, scale = 8)
		),
		EVENT = rep(1, n),
		TRT01P = rep(c("A", "B"), each = n / 2),
		stringsAsFactors = FALSE
	)

	result <- test_ph_assumption(
		data = ph_data,
		time_var = "TIME",
		event_var = "EVENT",
		trt_var = "TRT01P"
	)

	expect_false(result$violation)
	expect_true(result$global_test > 0.1)
})

test_that("test_ph_assumption accepts coxph model input", {
	skip_if_not_installed("survival")

	set.seed(456)
	n <- 200
	ph_data <- data.frame(
		TIME = c(
			rweibull(n / 2, shape = 1.2, scale = 10),
			rweibull(n / 2, shape = 1.2, scale = 8)
		),
		EVENT = rep(1, n),
		TRT01P = rep(c("A", "B"), each = n / 2),
		stringsAsFactors = FALSE
	)

	fit <- survival::coxph(
		survival::Surv(TIME, EVENT) ~ TRT01P,
		data = ph_data
	)

	result <- test_ph_assumption(fit)

	expect_s3_class(result$model, "coxph")
	expect_s3_class(result$zph, "cox.zph")
})

test_that("test_ph_assumption can generate Schoenfeld plots", {
	skip_if_not_installed("survival")
	skip_if_not_installed("ggplot2")

	set.seed(456)
	n <- 200
	ph_data <- data.frame(
		TIME = c(
			rweibull(n / 2, shape = 1.2, scale = 10),
			rweibull(n / 2, shape = 1.2, scale = 8)
		),
		EVENT = rep(1, n),
		TRT01P = rep(c("A", "B"), each = n / 2),
		stringsAsFactors = FALSE
	)

	result <- test_ph_assumption(
		data = ph_data,
		time_var = "TIME",
		event_var = "EVENT",
		trt_var = "TRT01P",
		plot = TRUE
	)

	expect_s7_class(result$plot, ClinicalPlot)
	expect_true(inherits(result$plot@plot, "ggplot"))
})
