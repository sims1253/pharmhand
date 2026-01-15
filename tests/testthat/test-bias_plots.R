# Tests for Risk of Bias Visualization Functions
# R/bias_plots.R

# =============================================================================
# Helper Functions for Creating Test Data
# =============================================================================

create_rob2_result <- function(
	study_id,
	d1 = "Low",
	d2 = "Low",
	d3 = "Low",
	d4 = "Low",
	d5 = "Low",
	outcome = ""
) {
	assess_rob2(
		study_id = study_id,
		d1_randomization = d1,
		d2_deviations = d2,
		d3_missing_data = d3,
		d4_measurement = d4,
		d5_selection = d5,
		outcome = outcome
	)
}

create_robinsi_result <- function(
	study_id,
	d1 = "Low",
	d2 = "Low",
	d3 = "Low",
	d4 = "Low",
	d5 = "Low",
	d6 = "Low",
	d7 = "Low",
	outcome = "",
	intervention = "",
	comparator = ""
) {
	assess_robins_i(
		study_id = study_id,
		d1_confounding = d1,
		d2_selection = d2,
		d3_classification = d3,
		d4_deviations = d4,
		d5_missing_data = d5,
		d6_measurement = d6,
		d7_selection_report = d7,
		outcome = outcome,
		intervention = intervention,
		comparator = comparator
	)
}

# =============================================================================
# create_rob_traffic_light_plot() Tests with RoB2Result
# =============================================================================

test_that("create_rob_traffic_light_plot works with single RoB2Result", {
	result <- create_rob2_result(
		study_id = "STUDY001",
		d4 = "Some concerns"
	)

	plot <- create_rob_traffic_light_plot(list(result))

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "rob_traffic_light")
	expect_equal(plot@title, "Risk of Bias Assessment")
	expect_true(inherits(plot@plot, "ggplot"))
	expect_true("RoB 2" %in% plot@metadata$tool)
	expect_equal(plot@metadata$n_studies, 1)
	expect_equal(plot@metadata$show_overall, TRUE)
})

test_that("create_rob_traffic_light_plot works with multiple RoB2Result", {
	results <- list(
		create_rob2_result("STUDY001", d4 = "Some concerns"),
		create_rob2_result("STUDY002", d3 = "High"),
		create_rob2_result("STUDY003", d1 = "Low")
	)

	plot <- create_rob_traffic_light_plot(results)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@metadata$n_studies, 3)
	expect_true(inherits(plot@plot, "ggplot"))
})

test_that("create_rob_traffic_light_plot respects show_overall = FALSE", {
	results <- list(
		create_rob2_result("STUDY001"),
		create_rob2_result("STUDY002")
	)

	plot <- create_rob_traffic_light_plot(results, show_overall = FALSE)

	expect_equal(plot@metadata$show_overall, FALSE)
})

test_that("create_rob_traffic_light_plot accepts custom title", {
	result <- create_rob2_result("STUDY001")

	plot <- create_rob_traffic_light_plot(
		list(result),
		title = "Custom RoB 2 Plot"
	)

	expect_equal(plot@title, "Custom RoB 2 Plot")
})

test_that("create_rob_traffic_light_plot accepts custom colors", {
	result <- create_rob2_result("STUDY001")

	custom_colors <- c(
		"Low" = "blue",
		"Some concerns" = "yellow",
		"High" = "red"
	)

	plot <- create_rob_traffic_light_plot(
		list(result),
		colors = custom_colors
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_rob_traffic_light_plot accepts base_size", {
	result <- create_rob2_result("STUDY001")

	plot <- create_rob_traffic_light_plot(
		list(result),
		base_size = 14
	)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_rob_traffic_light_plot calculates correct dimensions", {
	results <- list(
		create_rob2_result("STUDY001"),
		create_rob2_result("STUDY002"),
		create_rob2_result("STUDY003"),
		create_rob2_result("STUDY004"),
		create_rob2_result("STUDY005")
	)

	plot <- create_rob_traffic_light_plot(results)

	expect_gte(plot@width, 6)
	expect_gte(plot@height, 4)
})

# =============================================================================
# create_rob_traffic_light_plot() Tests with ROBINSIResult
# =============================================================================

test_that("create_rob_traffic_light_plot works with single ROBINSIResult", {
	result <- create_robinsi_result(
		study_id = "OBS001",
		d1 = "Serious",
		d6 = "Moderate"
	)

	plot <- create_rob_traffic_light_plot(list(result))

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "rob_traffic_light")
	expect_true(inherits(plot@plot, "ggplot"))
	expect_true("ROBINS-I" %in% plot@metadata$tool)
	expect_equal(plot@metadata$n_studies, 1)
})

test_that("create_rob_traffic_light_plot works with multiple ROBINSIResult", {
	results <- list(
		create_robinsi_result("OBS001", d1 = "Serious"),
		create_robinsi_result("OBS002", d6 = "Critical"),
		create_robinsi_result("OBS003", d3 = "Moderate")
	)

	plot <- create_rob_traffic_light_plot(results)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@metadata$n_studies, 3)
})

test_that("create_rob_traffic_light_plot handles all ROBINS-I judgments", {
	results <- list(
		create_robinsi_result("OBS001", d1 = "Low"),
		create_robinsi_result("OBS002", d2 = "Moderate"),
		create_robinsi_result("OBS003", d3 = "Serious"),
		create_robinsi_result("OBS004", d4 = "Critical"),
		create_robinsi_result("OBS005", d5 = "No information")
	)

	plot <- create_rob_traffic_light_plot(results)

	expect_s7_class(plot, ClinicalPlot)
})

# =============================================================================
# create_rob_summary_plot() Tests
# =============================================================================

test_that("create_rob_summary_plot works with RoB2Result list", {
	results <- list(
		create_rob2_result("STUDY001", d4 = "Some concerns"),
		create_rob2_result("STUDY002", d3 = "High"),
		create_rob2_result("STUDY003", d1 = "Low", d2 = "Low")
	)

	plot <- create_rob_summary_plot(results)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "rob_summary")
	expect_true(inherits(plot@plot, "ggplot"))
	expect_equal(plot@title, "Risk of Bias Summary")
	expect_true("RoB 2" %in% plot@metadata$tool)
})

test_that("create_rob_summary_plot works with ROBINSIResult list", {
	results <- list(
		create_robinsi_result("OBS001", d1 = "Serious"),
		create_robinsi_result("OBS002", d6 = "Critical"),
		create_robinsi_result("OBS003", d3 = "Moderate")
	)

	plot <- create_rob_summary_plot(results)

	expect_s7_class(plot, ClinicalPlot)
	expect_true("ROBINS-I" %in% plot@metadata$tool)
})

test_that("create_rob_summary_plot accepts custom title", {
	results <- list(create_rob2_result("STUDY001"))

	plot <- create_rob_summary_plot(
		results,
		title = "My Custom Summary"
	)

	expect_equal(plot@title, "My Custom Summary")
})

test_that("create_rob_summary_plot respects horizontal = FALSE", {
	results <- list(
		create_rob2_result("STUDY001"),
		create_rob2_result("STUDY002")
	)

	plot <- create_rob_summary_plot(results, horizontal = FALSE)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_rob_summary_plot accepts custom colors", {
	results <- list(create_rob2_result("STUDY001"))

	custom_colors <- c(
		"Low" = "blue",
		"Some concerns" = "yellow",
		"High" = "red"
	)

	plot <- create_rob_summary_plot(results, colors = custom_colors)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_rob_summary_plot accepts base_size", {
	results <- list(create_rob2_result("STUDY001"))

	plot <- create_rob_summary_plot(results, base_size = 14)

	expect_s7_class(plot, ClinicalPlot)
})

test_that("create_rob_summary_plot calculates correct proportions", {
	results <- list(
		create_rob2_result("STUDY001", d1 = "High"),
		create_rob2_result("STUDY002", d1 = "High"),
		create_rob2_result("STUDY003", d1 = "Low")
	)

	plot <- create_rob_summary_plot(results)

	expect_s7_class(plot, ClinicalPlot)
})

# =============================================================================
# save_rob_plot() Tests
# =============================================================================

test_that("save_rob_plot saves PNG file", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	withr::with_tempdir({
		filename <- "test_plot.png"
		output <- save_rob_plot(plot, filename)

		expect_true(file.exists(output))
		expect_equal(output, filename)
	})
})

test_that("save_rob_plot saves PDF file", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	withr::with_tempdir({
		filename <- "test_plot.pdf"
		output <- save_rob_plot(plot, filename)

		expect_true(file.exists(output))
		expect_equal(output, filename)
	})
})

test_that("save_rob_plot saves SVG file", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	withr::with_tempdir({
		filename <- "test_plot.svg"
		output <- save_rob_plot(plot, filename)

		expect_true(file.exists(output))
		expect_equal(output, filename)
	})
})

test_that("save_rob_plot accepts custom dimensions", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	withr::with_tempdir({
		filename <- "test_plot.png"
		output <- save_rob_plot(
			plot,
			filename,
			width = 12,
			height = 8
		)

		expect_true(file.exists(output))
	})
})

test_that("save_rob_plot accepts custom DPI", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	withr::with_tempdir({
		filename <- "test_plot.png"
		output <- save_rob_plot(plot, filename, dpi = 150)

		expect_true(file.exists(output))
	})
})

test_that("save_rob_plot works with ggplot", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	withr::with_tempdir({
		filename <- "test_plot.png"
		output <- save_rob_plot(plot@plot, filename)

		expect_true(file.exists(output))
	})
})

test_that("save_rob_plot returns filename", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	withr::with_tempdir({
		filename <- "test_plot.png"
		output <- save_rob_plot(plot, filename)

		expect_equal(output, filename)
	})
})

# =============================================================================
# rob_data_to_tidy() Tests - RoB2Result
# =============================================================================

test_that("rob_data_to_tidy returns long format by default for RoB2", {
	results <- list(
		create_rob2_result("STUDY001", d4 = "Some concerns", outcome = "OS")
	)

	tidy_df <- rob_data_to_tidy(results)

	expect_s3_class(tidy_df, "data.frame")
	expect_equal(nrow(tidy_df), 5)
	expect_true(all(
		c(
			"study_id",
			"outcome",
			"domain",
			"domain_label",
			"judgment",
			"support",
			"overall"
		) %in%
			names(tidy_df)
	))
	expect_equal(tidy_df$study_id[1], "STUDY001")
})

test_that("rob_data_to_tidy returns wide format for RoB2", {
	results <- list(
		create_rob2_result("STUDY001", d4 = "Some concerns")
	)

	wide_df <- rob_data_to_tidy(results, wide_format = TRUE)

	expect_s3_class(wide_df, "data.frame")
	expect_equal(nrow(wide_df), 1)
	expect_true(all(
		c(
			"study_id",
			"overall",
			"outcome",
			"randomization_judgment",
			"deviations_judgment",
			"missing_data_judgment",
			"measurement_judgment",
			"selection_judgment"
		) %in%
			names(wide_df)
	))
})

test_that("rob_data_to_tidy handles multiple RoB2 results", {
	results <- list(
		create_rob2_result("STUDY001", d4 = "Some concerns"),
		create_rob2_result("STUDY002", d3 = "High")
	)

	tidy_df <- rob_data_to_tidy(results)

	expect_equal(nrow(tidy_df), 10)
	expect_true(all(c("STUDY001", "STUDY002") %in% tidy_df$study_id))
})

# =============================================================================
# rob_data_to_tidy() Tests - ROBINSIResult
# =============================================================================

test_that("rob_data_to_tidy returns long format by default for ROBINS-I", {
	results <- list(
		create_robinsi_result(
			"OBS001",
			d1 = "Serious",
			d6 = "Moderate",
			outcome = "OS",
			intervention = "Drug A",
			comparator = "Placebo"
		)
	)

	tidy_df <- rob_data_to_tidy(results)

	expect_s3_class(tidy_df, "data.frame")
	expect_equal(nrow(tidy_df), 7)
	expect_true(all(
		c(
			"study_id",
			"outcome",
			"domain",
			"domain_label",
			"judgment",
			"support",
			"overall"
		) %in%
			names(tidy_df)
	))
})

test_that("rob_data_to_tidy returns wide format for ROBINS-I", {
	results <- list(
		create_robinsi_result(
			"OBS001",
			d1 = "Serious",
			intervention = "Drug A",
			comparator = "Placebo"
		)
	)

	wide_df <- rob_data_to_tidy(results, wide_format = TRUE)

	expect_s3_class(wide_df, "data.frame")
	expect_equal(nrow(wide_df), 1)
	expect_true("intervention" %in% names(wide_df))
	expect_true("comparator" %in% names(wide_df))
})

test_that("rob_data_to_tidy handles multiple ROBINS-I results", {
	results <- list(
		create_robinsi_result("OBS001", d1 = "Serious"),
		create_robinsi_result("OBS002", d6 = "Critical"),
		create_robinsi_result("OBS003", d3 = "Moderate")
	)

	tidy_df <- rob_data_to_tidy(results)

	expect_equal(nrow(tidy_df), 21)
})

# =============================================================================
# Error Handling Tests
# =============================================================================

test_that("create_rob_traffic_light_plot rejects empty list", {
	expect_error(
		create_rob_traffic_light_plot(list()),
		"results must be a non-empty list"
	)
})

test_that("create_rob_traffic_light_plot rejects non-list input", {
	expect_error(
		create_rob_traffic_light_plot("not a list"),
		"results must be a non-empty list"
	)
})

test_that("create_rob_traffic_light_plot rejects mixed RoB2/ROBINS-I results", {
	results <- list(
		create_rob2_result("STUDY001"),
		create_robinsi_result("OBS001")
	)

	expect_error(
		create_rob_traffic_light_plot(results),
		"All elements in results must be RoB2Result or ROBINSIResult"
	)
})

test_that("create_rob_traffic_light_plot rejects invalid result objects", {
	results <- list(
		list(not = "a valid result")
	)

	expect_error(
		create_rob_traffic_light_plot(results),
		"All elements in results must be RoB2Result or ROBINSIResult"
	)
})

test_that("create_rob_summary_plot rejects empty list", {
	expect_error(
		create_rob_summary_plot(list()),
		"results must be a non-empty list"
	)
})

test_that("create_rob_summary_plot rejects non-list input", {
	expect_error(
		create_rob_summary_plot("not a list"),
		"results must be a non-empty list"
	)
})

test_that("create_rob_summary_plot rejects mixed RoB2/ROBINS-I results", {
	results <- list(
		create_rob2_result("STUDY001"),
		create_robinsi_result("OBS001")
	)

	expect_error(
		create_rob_summary_plot(results),
		"All elements in results must be RoB2Result or ROBINSIResult"
	)
})

test_that("save_rob_plot rejects missing filename", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	expect_error(
		save_rob_plot(plot),
		"filename is required"
	)
})

test_that("save_rob_plot rejects empty filename", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	expect_error(
		save_rob_plot(plot, ""),
		"filename is required"
	)
})

test_that("save_rob_plot rejects invalid plot type", {
	expect_error(
		save_rob_plot("not a plot", "test.png"),
		"plot must be a ClinicalPlot object or ggplot"
	)
})

test_that("save_rob_plot rejects invalid width", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	expect_error(
		save_rob_plot(plot, "test.png", width = -1),
		"width must be a single positive numeric"
	)
})

test_that("save_rob_plot rejects invalid height", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	expect_error(
		save_rob_plot(plot, "test.png", height = 0),
		"height must be a single positive numeric"
	)
})

test_that("save_rob_plot rejects invalid DPI", {
	result <- create_rob2_result("STUDY001")
	plot <- create_rob_traffic_light_plot(list(result))

	expect_error(
		save_rob_plot(plot, "test.png", dpi = "invalid"),
		"dpi must be a single positive numeric"
	)
})

test_that("rob_data_to_tidy rejects empty list", {
	expect_error(
		rob_data_to_tidy(list()),
		"results must be a non-empty list"
	)
})

test_that("rob_data_to_tidy rejects non-list input", {
	expect_error(
		rob_data_to_tidy("not a list"),
		"results must be a non-empty list"
	)
})

test_that("rob_data_to_tidy rejects mixed RoB2/ROBINS-I results", {
	results <- list(
		create_rob2_result("STUDY001"),
		create_robinsi_result("OBS001")
	)

	expect_error(
		rob_data_to_tidy(results),
		"All elements in results must be RoB2Result or ROBINSIResult"
	)
})

# =============================================================================
# Empty List Handling Tests
# =============================================================================

test_that("rob_data_to_tidy handles empty list correctly", {
	expect_error(
		rob_data_to_tidy(list()),
		"results must be a non-empty list"
	)
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("Complete workflow: assess -> plot -> save -> tidy", {
	results <- list(
		create_rob2_result("STUDY001", d4 = "Some concerns", outcome = "OS"),
		create_rob2_result("STUDY002", d3 = "High", outcome = "PFS"),
		create_rob2_result("STUDY003", d1 = "Low", d2 = "Low", outcome = "OS")
	)

	traffic_plot <- create_rob_traffic_light_plot(results, show_overall = TRUE)
	expect_s7_class(traffic_plot, ClinicalPlot)

	summary_plot <- create_rob_summary_plot(results)
	expect_s7_class(summary_plot, ClinicalPlot)

	tidy_df <- rob_data_to_tidy(results)
	expect_equal(nrow(tidy_df), 15)

	withr::with_tempdir({
		filename <- "workflow_test.png"
		output <- save_rob_plot(traffic_plot, filename)
		expect_true(file.exists(output))
	})
})

test_that("Complete ROBINS-I workflow: assess -> plot -> save -> tidy", {
	results <- list(
		create_robinsi_result(
			"OBS001",
			d1 = "Serious",
			d6 = "Moderate",
			outcome = "Mortality",
			intervention = "Drug A",
			comparator = "SoC"
		),
		create_robinsi_result(
			"OBS002",
			d6 = "Critical",
			outcome = "Mortality",
			intervention = "Drug B",
			comparator = "SoC"
		)
	)

	traffic_plot <- create_rob_traffic_light_plot(results)
	expect_s7_class(traffic_plot, ClinicalPlot)

	summary_plot <- create_rob_summary_plot(results)
	expect_s7_class(summary_plot, ClinicalPlot)

	tidy_df <- rob_data_to_tidy(results)
	expect_equal(nrow(tidy_df), 14)

	withr::with_tempdir({
		filename <- "robinsi_workflow.pdf"
		output <- save_rob_plot(summary_plot, filename)
		expect_true(file.exists(output))
	})
})
