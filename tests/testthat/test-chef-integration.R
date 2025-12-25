# Tests for chef pipeline integration

test_that("chef_to_analysis_results converts data.table", {
	skip_if_not_installed("data.table")

	# Create mock chef output
	mock_output <- data.table::data.table(
		endpoint_id = c("EP1", "EP1", "EP2"),
		strat_id = c("Overall", "Male", "Overall"),
		stat_id = c("n", "n", "mean"),
		stat_result_value = c(100, 60, 25.5),
		stat_result_label = c("N", "N", "Mean")
	)

	result <- chef_to_analysis_results(mock_output, type = "hta")

	expect_true(S7::S7_inherits(result, AnalysisResults))
	expect_equal(result@type, "hta")
	expect_true(is.data.frame(result@stats))
	expect_true(
		"endpoint" %in%
			names(result@stats) ||
			"endpoint_id" %in% names(result@stats)
	)
})

test_that("chef_to_analysis_results converts data.frame", {
	mock_output <- data.frame(
		endpoint_id = c("EP1", "EP2"),
		stat_result_value = c(100, 200),
		stat_result_label = c("N", "N")
	)

	result <- chef_to_analysis_results(mock_output)

	expect_true(S7::S7_inherits(result, AnalysisResults))
	expect_true(is.data.frame(result@stats))
})

test_that("chef_to_analysis_results attaches metadata", {
	mock_output <- data.frame(
		endpoint_id = "EP1",
		strat_id = "Overall",
		stat_id = "n",
		stat_result_value = 100
	)

	custom_meta <- list(study = "TEST001")
	result <- chef_to_analysis_results(mock_output, metadata = custom_meta)

	expect_true("study" %in% names(result@metadata))
	expect_equal(result@metadata$study, "TEST001")
	expect_true("source" %in% names(result@metadata))
	expect_equal(result@metadata$source, "chef")
})

test_that("create_chef_endpoint creates valid spec", {
	spec <- create_chef_endpoint(
		name = "Response Rate",
		variable = "AVALC",
		type = "binary",
		strata = c("SEX", "AGEGR1")
	)

	expect_type(spec, "list")
	expect_equal(spec$name, "Response Rate")
	expect_equal(spec$variable, "AVALC")
	expect_equal(spec$type, "binary")
	expect_equal(spec$strata, c("SEX", "AGEGR1"))
})

test_that("flatten_chef_results handles hierarchical output", {
	mock_dt <- data.frame(
		endpoint_id = c("EP1", "EP1"),
		strat_id = c("Overall", "Male"),
		stat_id = c("n", "n"),
		stat_result_value = c(100, 60),
		stat_result_label = c("N", "N"),
		stat_result_qualifier = c("", "")
	)

	result <- flatten_chef_results(mock_dt)

	expect_true(is.data.frame(result))
	expect_true("row_label" %in% names(result))
	expect_true(
		"value" %in% names(result) || "stat_result_value" %in% names(result)
	)
})

test_that("extract_chef_metadata extracts correct info", {
	mock_dt <- data.frame(
		endpoint_id = c("EP1", "EP2", "EP1"),
		strat_id = c("Overall", "Overall", "Male")
	)

	meta <- extract_chef_metadata(mock_dt)

	expect_type(meta, "list")
	expect_equal(meta$source, "chef")
	expect_equal(meta$n_endpoints, 2)
	expect_equal(meta$n_strata, 2)
	expect_true("generated_at" %in% names(meta))
})

test_that("run_chef_pipeline returns correct output type", {
	skip_if_not_installed("chef")
	# This test checks the mock/placeholder behavior
	# Full chef integration requires chef package

	adam_data <- list(
		adsl = data.frame(USUBJID = "01", TRT01A = "A"),
		adae = data.frame(USUBJID = "01", AEDECOD = "Headache")
	)

	endpoints <- list(
		create_chef_endpoint("AE Count", "AEDECOD", type = "count")
	)

	# Test results output
	result <- run_chef_pipeline(adam_data, endpoints, output_type = "results")
	expect_true(S7::S7_inherits(result, AnalysisResults))

	# Test table output
	result <- run_chef_pipeline(adam_data, endpoints, output_type = "table")
	expect_true(S7::S7_inherits(result, ClinicalTable))

	# Test report output
	result <- run_chef_pipeline(adam_data, endpoints, output_type = "report")
	expect_true(S7::S7_inherits(result, ClinicalReport))
})

test_that("HTAEndpoint class works correctly", {
	ep <- HTAEndpoint(
		name = "Primary Efficacy",
		variable = "AVAL",
		type = "continuous",
		strata = c("SEX", "AGEGR1"),
		criteria = list()
	)

	expect_true(S7::S7_inherits(ep, HTAEndpoint))
	expect_true(S7::S7_inherits(ep, PrimaryEndpoint))
	expect_equal(ep@name, "Primary Efficacy")
	expect_equal(ep@strata, c("SEX", "AGEGR1"))
})

test_that("AnalysisMeta tracks metadata correctly", {
	meta <- AnalysisMeta(
		source_vars = c("AGE", "SEX"),
		row_id = "demo_001",
		derivation = "count distinct USUBJID"
	)

	expect_true(S7::S7_inherits(meta, AnalysisMeta))
	expect_equal(meta@source_vars, c("AGE", "SEX"))
	expect_equal(meta@row_id, "demo_001")
})

test_that("create_analysis_meta populates environment info", {
	# FunctionReport package is always available since this is part of the package
	meta <- create_analysis_meta(
		source_vars = "AGE",
		derivation = "mean"
	)

	expect_true(S7::S7_inherits(meta, AnalysisMeta))
	expect_false(is.null(meta@timestamp))
	expect_true(nchar(meta@r_version) > 0)
})

test_that("HTASection creates valid section", {
	section <- HTASection(
		title = "Primary Analysis",
		section_type = "efficacy",
		comparator = "Placebo",
		population = "ITT"
	)

	expect_true(S7::S7_inherits(section, HTASection))
	expect_true(S7::S7_inherits(section, ReportSection))
	expect_equal(section@comparator, "Placebo")
	expect_equal(section@population, "ITT")
})
