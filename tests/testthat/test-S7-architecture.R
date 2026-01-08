# Test S7 architecture and high-performance reporting

test_that("ADaMData and Analysis Core works", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	# Basic analysis
	results <- analyze(adam)
	expect_true(S7::S7_inherits(results, AnalysisResults))
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
		treatment_var = "TRT01P",
		study_id = "TEST-001",
		study_title = "Test Study"
	)

	study <- analyze_study(study)
	expect_length(study@results, 2)
	expect_true("baseline" %in% names(study@results))
	expect_true("safety" %in% names(study@results))

	# Report Generation
	report <- create_study_report(study)
	expect_true(S7::S7_inherits(report, ClinicalReport))
	expect_length(report@sections, 2)
})

# Tests for ADaMData Computed Properties ----

test_that("ADaMData filtered_data computed property works", {
	adsl <- create_mock_adsl(n = 20)

	# With ITT population (all subjects should have ITTFL = "Y")
	adam <- ADaMData(data = adsl, population = "ITT")
	expect_equal(nrow(adam@filtered_data), 20)

	# With population that filters subjects - set all to Y first, then change some
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:5] <- "N"
	adam_pps <- ADaMData(data = adsl, population = "PPS")
	expect_lt(nrow(adam_pps@filtered_data), 20)
	expect_equal(nrow(adam_pps@filtered_data), 15)
})

test_that("ADaMData filtered_data returns all for ALL population", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ALL")

	expect_equal(nrow(adam@filtered_data), 20)
})

test_that("ADaMData trt_n computed property works", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	trt_n <- adam@trt_n

	expect_true(is.data.frame(trt_n))
	expect_true("N" %in% names(trt_n))
	expect_true("TRT01P" %in% names(trt_n))
	expect_equal(nrow(trt_n), 2) # Placebo and Active
	expect_equal(sum(trt_n$N), 20)
})

test_that("ADaMData trt_n respects population filter", {
	adsl <- create_mock_adsl(n = 20)
	# Mark some subjects as not in PPS - set all to Y first
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:4] <- "N"

	adam <- ADaMData(data = adsl, population = "PPS")
	trt_n <- adam@trt_n

	expect_equal(sum(trt_n$N), 16) # 20 - 4 filtered out
})

# Tests for Helper Functions ----

test_that("get_trt_n works with ADaMData", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	trt_n <- get_trt_n(adam)

	expect_true(is.data.frame(trt_n))
	expect_true("N" %in% names(trt_n))
	expect_equal(sum(trt_n$N), 20)
})

test_that("get_trt_n works with data frame", {
	adsl <- create_mock_adsl(n = 20)

	trt_n <- get_trt_n(adsl, trt_var = "TRT01P")

	expect_true(is.data.frame(trt_n))
	expect_true("N" %in% names(trt_n))
	expect_equal(sum(trt_n$N), 20)
})

test_that("get_trt_n applies population filter for data frames", {
	adsl <- create_mock_adsl(n = 20)
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:4] <- "N"

	trt_n <- get_trt_n(adsl, population = "PPS")

	expect_equal(sum(trt_n$N), 16)
})

test_that("get_filtered_data works with ADaMData", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	df <- get_filtered_data(adam)

	expect_true(is.data.frame(df))
	expect_equal(nrow(df), 20)
})

test_that("get_filtered_data works with data frame", {
	adsl <- create_mock_adsl(n = 20)
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:4] <- "N"

	df <- get_filtered_data(adsl, population = "PPS")

	expect_true(is.data.frame(df))
	expect_equal(nrow(df), 16)
})

test_that("get_trt_var returns correct variable", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl, trt_var = "TRT01P")

	expect_equal(get_trt_var(adam), "TRT01P")
	expect_equal(get_trt_var(adsl, default = "TRT01P"), "TRT01P")
})

test_that("get_subject_var returns correct variable", {
	adsl <- create_mock_adsl(n = 20)
	adam <- ADaMData(data = adsl)

	expect_equal(get_subject_var(adam), "USUBJID")
	expect_equal(get_subject_var(adsl, default = "USUBJID"), "USUBJID")
})
