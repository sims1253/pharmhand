# Test S7 architecture and high-performance reporting
library(testthat)
library(FunctionReport)
source("fixtures.R")

test_that("ADaMData and Analysis Core works", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	# Basic analysis
	results <- analyze(adam)
	expect_s3_class(results, "FunctionReport::AnalysisResults")
	expect_equal(results@type, "baseline")
	expect_equal(nrow(results@stats), 2) # Placebo and Active
})

test_that("Baseline Characteristics calculation works", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl)

	results <- calculate_baseline(adam, vars = c("AGE"))
	expect_equal(results@type, "baseline")
	expect_true("mean" %in% names(results@stats))
	expect_equal(nrow(results@stats), 2) # Two groups
})

test_that("SOC-PT Analysis works", {
	adae <- create_mock_adae(n = 20)
	results <- analyze_soc_pt(adae)

	expect_equal(results@type, "safety_ae")
	expect_true("level" %in% names(results@stats))
	expect_true(any(results@stats$level == "SOC"))
	expect_true(any(results@stats$level == "PT"))
})

test_that("Reporting Engine (flextable/gt) works", {
	adsl <- create_mock_adsl(n = 10)
	adam <- ADaMData(data = adsl)
	results <- calculate_baseline(adam, vars = c("AGE"))

	# Flextable
	ft <- as_flextable(results)
	expect_s3_class(ft, "flextable")

	# GT
	gt_tbl <- as_gt(results)
	expect_s3_class(gt_tbl, "gt_tbl")
})

test_that("Study Logic works", {
	adsl <- create_mock_adsl(n = 20)
	# Add AE columns for safety analysis test
	adsl$AEBODSYS <- "Nervous system"
	adsl$AEDECOD <- "Headache"

	# Two Arm Study
	study <- TwoArmStudy(
		data = adsl,
		group_var = "TRT01P",
		study_id = "TEST-001",
		study_title = "Test Study"
	)

	study <- analyze_study(study)
	expect_length(study@results, 2)
	expect_true("baseline" %in% names(study@results))
	expect_true("safety" %in% names(study@results))

	# Report Generation
	report <- create_study_report(study)
	expect_s3_class(report, "FunctionReport::ClinicalReport")
	expect_length(report@sections, 2)
})
