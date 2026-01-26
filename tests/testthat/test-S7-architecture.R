# Test S7 architecture and high-performance reporting

test_that("ADaMData and Analysis Core works", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	# Basic analysis
	results <- analyze(adam)
	expect_true(S7::S7_inherits(results, AnalysisResults))
	expect_equal(results@type, "baseline")
	expect_equal(nrow(results@stats), 2) # Placebo and Active
})

test_that("Baseline Characteristics calculation works", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl)

	results <- calculate_baseline(adam, vars = c("AGE"))
	expect_equal(results@type, "baseline")
	expect_true("mean" %in% names(results@stats))
	expect_equal(nrow(results@stats), 2) # Two groups
})

test_that("SOC-PT Analysis works", {
	adae <- get_shared_adae(n = 20)
	results <- analyze_soc_pt(adae)

	expect_equal(results@type, "safety_ae")
	expect_true("level" %in% names(results@stats))
	expect_true(any(results@stats$level == "SOC"))
	expect_true(any(results@stats$level == "PT"))
})

test_that("Reporting Engine (flextable/gt) works", {
	adsl <- get_shared_adsl(n = 10)
	adam <- ADaMData(data = adsl)
	results <- calculate_baseline(adam, vars = c("AGE"))

	# Flextable
	ft <- as_flextable(results)
	expect_s3_class(ft, "flextable")

	# GT
	gt_tbl <- as_gt(results)
	expect_s3_class(gt_tbl, "gt_tbl")
})

test_that("AnalysisResults computed properties work with empty stats", {
	empty_results <- AnalysisResults(
		stats = data.frame(),
		type = "test"
	)

	expect_true(empty_results@is_empty)
	expect_equal(empty_results@n_rows, 0L)
	expect_equal(empty_results@n_cols, 0L)
	expect_equal(empty_results@summary_label, "test (empty)")
	expect_equal(empty_results@column_names, character())
})

test_that("Study Logic works", {
	adsl <- get_shared_adsl(n = 20)
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
	adsl <- get_shared_adsl(n = 20)

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
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ALL")

	expect_equal(nrow(adam@filtered_data), 20)
})

test_that("ADaMData trt_n computed property works", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	trt_n <- adam@trt_n

	expect_true(is.data.frame(trt_n))
	expect_true("N" %in% names(trt_n))
	expect_true("TRT01P" %in% names(trt_n))
	expect_equal(nrow(trt_n), 2) # Placebo and Active
	expect_equal(sum(trt_n$N), 20)
})

test_that("ADaMData trt_n respects population filter", {
	adsl <- get_shared_adsl(n = 20)
	# Mark some subjects as not in PPS - set all to Y first
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:4] <- "N"

	adam <- ADaMData(data = adsl, population = "PPS")
	trt_n <- adam@trt_n

	expect_equal(sum(trt_n$N), 16) # 20 - 4 filtered out
})

test_that("ADaMData subject_n computed property works", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	expect_equal(adam@subject_n, 20L)
})

test_that("ADaMData subject_n handles empty filtered_data", {
	empty_df <- data.frame(
		USUBJID = character(),
		TRT01P = character(),
		ITTFL = character()
	)
	adam <- ADaMData(data = empty_df, population = "ITT")

	expect_equal(adam@subject_n, 0L)
})

test_that("ADaMData trt_levels computed property works", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	levels <- adam@trt_levels
	expect_true(is.character(levels))
	expect_true("Placebo" %in% levels)
	expect_true("Active" %in% levels)
	expect_equal(sort(levels), levels) # Should be sorted
})

test_that("ADaMData trt_levels handles empty filtered_data", {
	empty_df <- data.frame(
		USUBJID = character(),
		TRT01P = character(),
		ITTFL = character()
	)
	adam <- ADaMData(data = empty_df, population = "ITT")

	expect_equal(adam@trt_levels, character())
})

test_that("ADaMData is_empty computed property works", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	expect_false(adam@is_empty)
})

test_that("ADaMData is_empty returns TRUE for empty filtered_data", {
	empty_df <- data.frame(
		USUBJID = character(),
		TRT01P = character(),
		ITTFL = character()
	)
	adam <- ADaMData(data = empty_df, population = "ITT")

	expect_true(adam@is_empty)
})

test_that("ADaMData summary_label computed property works", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "FAS")

	expect_equal(adam@summary_label, "FAS (N=20)")
})

test_that("ADaMData summary_label handles empty data", {
	empty_df <- data.frame(
		USUBJID = character(),
		TRT01P = character(),
		FASFL = character()
	)
	adam <- ADaMData(data = empty_df, population = "FAS")

	expect_equal(adam@summary_label, "FAS (N=0)")
})

# Tests for Helper Functions ----

test_that("get_trt_n works with ADaMData", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	trt_n <- get_trt_n(adam)

	expect_true(is.data.frame(trt_n))
	expect_true("N" %in% names(trt_n))
	expect_equal(sum(trt_n$N), 20)
})

test_that("get_trt_n works with data frame", {
	adsl <- get_shared_adsl(n = 20)

	trt_n <- get_trt_n(adsl, trt_var = "TRT01P")

	expect_true(is.data.frame(trt_n))
	expect_true("N" %in% names(trt_n))
	expect_equal(sum(trt_n$N), 20)
})

test_that("get_trt_n applies population filter for data frames", {
	adsl <- get_shared_adsl(n = 20)
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:4] <- "N"

	trt_n <- get_trt_n(adsl, population = "PPS")

	expect_equal(sum(trt_n$N), 16)
})

test_that("get_filtered_data works with ADaMData", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	df <- get_filtered_data(adam)

	expect_true(is.data.frame(df))
	expect_equal(nrow(df), 20)
})

test_that("get_filtered_data works with data frame", {
	adsl <- get_shared_adsl(n = 20)
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:4] <- "N"

	df <- get_filtered_data(adsl, population = "PPS")

	expect_true(is.data.frame(df))
	expect_equal(nrow(df), 16)
})

test_that("get_trt_var returns correct variable", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, trt_var = "TRT01P")

	expect_equal(get_trt_var(adam), "TRT01P")
	expect_equal(get_trt_var(adsl, default = "TRT01P"), "TRT01P")
})

test_that("get_subject_var returns correct variable", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl)

	expect_equal(get_subject_var(adam), "USUBJID")
	expect_equal(get_subject_var(adsl, default = "USUBJID"), "USUBJID")
})

test_that("get_subject_n works with ADaMData", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "ITT")

	expect_equal(get_subject_n(adam), 20L)
})

test_that("get_subject_n works with data frame", {
	adsl <- get_shared_adsl(n = 20)

	expect_equal(get_subject_n(adsl), 20L)
})

test_that("get_subject_n applies population filter for data frames", {
	adsl <- get_shared_adsl(n = 20)
	adsl$PPSFL <- "Y"
	adsl$PPSFL[1:4] <- "N"

	expect_equal(get_subject_n(adsl, population = "PPS"), 16L)
})

test_that("get_summary_label works with ADaMData", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl, population = "FAS")

	expect_equal(get_summary_label(adam), "FAS (N=20)")
})

test_that("get_summary_label works with AnalysisResults", {
	adsl <- get_shared_adsl(n = 20)
	adam <- ADaMData(data = adsl)
	results <- calculate_baseline(adam, vars = c("AGE"))

	expect_equal(get_summary_label(results), "baseline (n=2)")
})

test_that("get_summary_label works with empty AnalysisResults", {
	empty_results <- AnalysisResults(
		stats = data.frame(),
		type = "test"
	)

	expect_equal(get_summary_label(empty_results), "test (empty)")
})
