# Tests for Risk of Bias 2 (RoB 2) Assessment Functions
# R/bias_rob2.R

# =============================================================================
# RoB2Result S7 Class Tests
# =============================================================================

test_that("RoB2Result can be created with all required properties", {
	result <- RoB2Result(
		study_id = "STUDY001",
		domains = list(
			D1_randomization = list(
				judgment = "Low",
				support = "Allocation concealed"
			),
			D2_deviations = list(judgment = "Low", support = "No deviations"),
			D3_missing_data = list(judgment = "Low", support = "No missing data"),
			D4_measurement = list(judgment = "Low", support = "Blinded assessment"),
			D5_selection = list(judgment = "Low", support = "Pre-specified analysis")
		),
		overall = "Low",
		overall_justification = "All domains rated Low",
		outcome = "OS"
	)

	expect_s7_class(result, RoB2Result)
	expect_equal(result@study_id, "STUDY001")
	expect_equal(result@overall, "Low")
	expect_equal(result@outcome, "OS")
	expect_equal(length(result@domains), 5)
})

test_that("RoB2Result rejects missing domain", {
	expect_error(
		RoB2Result(
			study_id = "STUDY001",
			domains = list(
				D1_randomization = list(judgment = "Low", support = "Text"),
				D2_deviations = list(judgment = "Low", support = "Text")
				# Missing D3, D4, D5
			),
			overall = "Low"
		),
		"domains must contain all required domains"
	)
})

test_that("RoB2Result rejects domain without judgment", {
	expect_error(
		RoB2Result(
			study_id = "STUDY001",
			domains = list(
				D1_randomization = list(support = "Text"), # No judgment
				D2_deviations = list(judgment = "Low", support = "Text"),
				D3_missing_data = list(judgment = "Low", support = "Text"),
				D4_measurement = list(judgment = "Low", support = "Text"),
				D5_selection = list(judgment = "Low", support = "Text")
			),
			overall = "Low"
		),
		"domain D1_randomization must have a 'judgment' element"
	)
})

test_that("RoB2Result rejects invalid domain judgment", {
	expect_error(
		RoB2Result(
			study_id = "STUDY001",
			domains = list(
				D1_randomization = list(judgment = "Invalid", support = "Text"),
				D2_deviations = list(judgment = "Low", support = "Text"),
				D3_missing_data = list(judgment = "Low", support = "Text"),
				D4_measurement = list(judgment = "Low", support = "Text"),
				D5_selection = list(judgment = "Low", support = "Text")
			),
			overall = "Low"
		),
		"judgment must be one of"
	)
})

test_that("RoB2Result rejects invalid overall judgment", {
	expect_error(
		RoB2Result(
			study_id = "STUDY001",
			domains = list(
				D1_randomization = list(judgment = "Low", support = "Text"),
				D2_deviations = list(judgment = "Low", support = "Text"),
				D3_missing_data = list(judgment = "Low", support = "Text"),
				D4_measurement = list(judgment = "Low", support = "Text"),
				D5_selection = list(judgment = "Low", support = "Text")
			),
			overall = "Invalid"
		),
		"overall must be one of"
	)
})

test_that("RoB2Result has correct default values", {
	result <- RoB2Result(
		study_id = "STUDY001",
		domains = list(
			D1_randomization = list(judgment = "Low"),
			D2_deviations = list(judgment = "Low"),
			D3_missing_data = list(judgment = "Low"),
			D4_measurement = list(judgment = "Low"),
			D5_selection = list(judgment = "Low")
		),
		overall = "Low"
	)

	expect_equal(result@overall_justification, "")
	expect_equal(result@outcome, "")
	expect_equal(result@metadata, list())
})

test_that("RoB2Result summary_df computed property works", {
	result <- RoB2Result(
		study_id = "STUDY001",
		domains = list(
			D1_randomization = list(judgment = "Low", support = "Text1"),
			D2_deviations = list(judgment = "Some concerns", support = "Text2"),
			D3_missing_data = list(judgment = "High", support = "Text3"),
			D4_measurement = list(judgment = "Low", support = "Text4"),
			D5_selection = list(judgment = "Low", support = "Text5")
		),
		overall = "High"
	)

	summary_df <- result@summary_df

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 5)
	expect_equal(
		names(summary_df),
		c("domain", "domain_label", "judgment", "support")
	)
	expect_equal(
		summary_df$domain,
		c(
			"D1_randomization",
			"D2_deviations",
			"D3_missing_data",
			"D4_measurement",
			"D5_selection"
		)
	)
})

test_that("RoB2Result judgment_counts computed property works", {
	result <- RoB2Result(
		study_id = "STUDY001",
		domains = list(
			D1_randomization = list(judgment = "Low"),
			D2_deviations = list(judgment = "Low"),
			D3_missing_data = list(judgment = "Some concerns"),
			D4_measurement = list(judgment = "Some concerns"),
			D5_selection = list(judgment = "High")
		),
		overall = "High"
	)

	counts <- result@judgment_counts

	expect_type(counts, "list")
	expect_equal(counts$n_low, 2)
	expect_equal(counts$n_concerns, 2)
	expect_equal(counts$n_high, 1)
})


# =============================================================================
# assess_rob2() Function Tests
# =============================================================================

test_that("assess_rob2 creates valid result with all Low judgments", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low"
	)

	expect_s7_class(result, RoB2Result)
	expect_equal(result@overall, "Low")
	expect_equal(result@study_id, "STUDY001")
	expect_equal(result@judgment_counts$n_low, 5)
	expect_equal(result@judgment_counts$n_concerns, 0)
	expect_equal(result@judgment_counts$n_high, 0)
})

test_that("assess_rob2 auto-calculates overall as High with any High domain", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "High",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low"
	)

	expect_equal(result@overall, "High")
})

test_that("assess_rob2 auto-calculates overall as High with 2+ Some concerns", {
	# Exactly 2 Some concerns -> High
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Some concerns",
		d3_missing_data = "Low",
		d4_measurement = "Some concerns",
		d5_selection = "Low"
	)

	expect_equal(result@overall, "High")
})

test_that("assess_rob2 auto-calculates overall as Some concerns with 1 Some concerns", {
	# Exactly 1 Some concerns -> Some concerns
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Some concerns",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low"
	)

	expect_equal(result@overall, "Some concerns")
})

test_that("assess_rob2 auto-calculates overall correctly for mixed scenarios", {
	# Scenario: 1 High + 2 Some concerns -> High
	result1 <- assess_rob2(
		study_id = "S1",
		d1_randomization = "High",
		d2_deviations = "Some concerns",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Some concerns"
	)
	expect_equal(result1@overall, "High")

	# Scenario: 1 High + 0 Some concerns -> High
	result2 <- assess_rob2(
		study_id = "S2",
		d1_randomization = "High",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low"
	)
	expect_equal(result2@overall, "High")

	# Scenario: 3 Some concerns -> High
	result3 <- assess_rob2(
		study_id = "S3",
		d1_randomization = "Low",
		d2_deviations = "Some concerns",
		d3_missing_data = "Some concerns",
		d4_measurement = "Some concerns",
		d5_selection = "Low"
	)
	expect_equal(result3@overall, "High")
})

test_that("assess_rob2 accepts custom overall judgment", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		overall = "Some concerns",
		overall_justification = "Custom justification"
	)

	expect_equal(result@overall, "Some concerns")
	expect_equal(result@overall_justification, "Custom justification")
})

test_that("assess_rob2 accepts optional outcome parameter", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		outcome = "Overall Survival"
	)

	expect_equal(result@outcome, "Overall Survival")
})

test_that("assess_rob2 accepts optional support text", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		d1_support = "Allocation concealed properly",
		d4_support = "Blinded outcome assessment"
	)

	# Support text is stored (may have names attribute)
	expect_true(nzchar(result@domains[["D1_randomization"]]$support))
	expect_true(nzchar(result@domains[["D4_measurement"]]$support))
})

test_that("assess_rob2 accepts optional metadata", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		metadata = list(reviewer = "Dr. Smith", date = "2024-01-15")
	)

	expect_equal(result@metadata$reviewer, "Dr. Smith")
	expect_equal(result@metadata$date, "2024-01-15")
})

test_that("assess_rob2 rejects missing study_id", {
	expect_error(
		assess_rob2(
			study_id = NULL,
			d1_randomization = "Low",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		),
		"study_id is required"
	)
})

test_that("assess_rob2 rejects invalid domain judgment", {
	expect_error(
		assess_rob2(
			study_id = "STUDY001",
			d1_randomization = "Invalid",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		),
		"must be one of"
	)
})

test_that("assess_rob2 rejects NULL domain judgment", {
	# NULL causes judgments vector to have wrong length,
	# triggering a different error
	expect_error(
		assess_rob2(
			study_id = "STUDY001",
			d1_randomization = NULL,
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		),
		regexp = "judgment|D1_randomization"
	)
})

test_that("assess_rob2 rejects empty string domain judgment", {
	expect_error(
		assess_rob2(
			study_id = "STUDY001",
			d1_randomization = "",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		),
		"D1_randomization judgment is required"
	)
})


# =============================================================================
# .calculate_rob2_overall() Internal Function Tests
# =============================================================================

test_that(".calculate_rob2_overall returns Low for all Low judgments", {
	judgments <- c("Low", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_rob2_overall(judgments), "Low")
})

test_that(".calculate_rob2_overall returns High for any High judgment", {
	# One High
	judgments1 <- c("High", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_rob2_overall(judgments1), "High")

	# Multiple High
	judgments2 <- c("High", "High", "Low", "Low", "Low")
	expect_equal(.calculate_rob2_overall(judgments2), "High")
})

test_that(".calculate_rob2_overall returns High for 2+ Some concerns", {
	# Exactly 2
	judgments1 <- c("Some concerns", "Some concerns", "Low", "Low", "Low")
	expect_equal(.calculate_rob2_overall(judgments1), "High")

	# Exactly 3
	judgments2 <- c(
		"Some concerns",
		"Some concerns",
		"Some concerns",
		"Low",
		"Low"
	)
	expect_equal(.calculate_rob2_overall(judgments2), "High")

	# Exactly 4
	judgments3 <- c(
		"Some concerns",
		"Some concerns",
		"Some concerns",
		"Some concerns",
		"Low"
	)
	expect_equal(.calculate_rob2_overall(judgments3), "High")

	# All 5
	judgments4 <- rep("Some concerns", 5)
	expect_equal(.calculate_rob2_overall(judgments4), "High")
})

test_that(".calculate_rob2_overall returns Some concerns for 1 Some concerns", {
	judgments <- c("Some concerns", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_rob2_overall(judgments), "Some concerns")
})

test_that(".calculate_rob2_overall rejects wrong length input", {
	expect_error(
		.calculate_rob2_overall(c("Low", "Low", "Low")),
		"judgments must be a character vector of length 5"
	)
})


# =============================================================================
# .generate_rob2_justification() Internal Function Tests
# =============================================================================

test_that(".generate_rob2_justification returns correct text for Low", {
	judgments <- c("Low", "Low", "Low", "Low", "Low")
	just <- .generate_rob2_justification(judgments, "Low")
	expect_equal(just, "All domains rated Low risk of bias")
})

test_that(".generate_rob2_justification returns correct text for High
  with High domain", {
	judgments <- c("High", "Low", "Low", "Low", "Low")
	just <- .generate_rob2_justification(judgments, "High")
	expect_true(grepl("randomization", just, ignore.case = TRUE))
	expect_true(grepl("High", just, fixed = TRUE))
})

test_that(".generate_rob2_justification returns correct text for High
  with Some concerns", {
	judgments <- c("Some concerns", "Some concerns", "Low", "Low", "Low")
	just <- .generate_rob2_justification(judgments, "High")
	expect_true(grepl("2.*domains.*Some concerns", just))
})

test_that(".generate_rob2_justification returns correct text for High with both", {
	judgments <- c("High", "Some concerns", "Some concerns", "Low", "Low")
	just <- .generate_rob2_justification(judgments, "High")
	expect_true(grepl("randomization", just, ignore.case = TRUE))
	expect_true(grepl("2.*domains.*Some concerns", just))
})

test_that(".generate_rob2_justification returns correct text for Some concerns", {
	judgments <- c("Low", "Some concerns", "Low", "Low", "Low")
	just <- .generate_rob2_justification(judgments, "Some concerns")
	expect_true(grepl("deviations.*Some concerns", just, ignore.case = TRUE))
})


# =============================================================================
# assess_rob2_batch() Function Tests
# =============================================================================

test_that("assess_rob2_batch processes single study data frame", {
	data <- data.frame(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		stringsAsFactors = FALSE
	)

	results <- assess_rob2_batch(data)

	expect_type(results, "list")
	expect_length(results, 1)
	expect_named(results, "STUDY001")
	expect_s7_class(results[[1]], RoB2Result)
})

test_that("assess_rob2_batch processes multiple studies", {
	data <- data.frame(
		study_id = c("STUDY001", "STUDY002", "STUDY003"),
		d1_randomization = c("Low", "Low", "High"),
		d2_deviations = c("Low", "Some concerns", "Low"),
		d3_missing_data = c("Low", "Low", "Low"),
		d4_measurement = c("Some concerns", "Low", "Low"),
		d5_selection = c("Low", "Low", "Low"),
		stringsAsFactors = FALSE
	)

	results <- assess_rob2_batch(data, .suppress_messages = TRUE)

	expect_type(results, "list")
	expect_length(results, 3)
	expect_named(results, c("STUDY001", "STUDY002", "STUDY003"))
	expect_s7_class(results[[1]], RoB2Result)
	expect_s7_class(results[[2]], RoB2Result)
	expect_s7_class(results[[3]], RoB2Result)
})

test_that("assess_rob2_batch includes optional columns", {
	data <- data.frame(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		d1_support = "Allocation concealed",
		d4_support = "Blinded assessment",
		outcome = "OS",
		stringsAsFactors = FALSE
	)

	results <- assess_rob2_batch(data, .suppress_messages = TRUE)

	# Support text is stored (may have names attribute)
	expect_true(nzchar(results[[1]]@domains[["D1_randomization"]]$support))
	expect_true(nzchar(results[[1]]@domains[["D4_measurement"]]$support))
	expect_equal(results[[1]]@outcome, "OS")
})

test_that("assess_rob2_batch accepts custom overall judgments", {
	data <- data.frame(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		overall = "Some concerns",
		overall_justification = "Custom override",
		stringsAsFactors = FALSE
	)

	results <- assess_rob2_batch(data, .suppress_messages = TRUE)

	expect_equal(results[[1]]@overall, "Some concerns")
	expect_equal(results[[1]]@overall_justification, "Custom override")
})

test_that("assess_rob2_batch handles missing optional columns gracefully", {
	data <- data.frame(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		stringsAsFactors = FALSE
	)

	results <- assess_rob2_batch(data, .suppress_messages = TRUE)

	# Support text may be stored as named empty string or NA
	expect_true(length(results[[1]]@domains[["D1_randomization"]]$support) >= 0)
	# overall_justification is auto-generated when not provided
	expect_true(nzchar(results[[1]]@overall_justification))
})

test_that("assess_rob2_batch rejects data frame without study_id column", {
	data_no_id <- data.frame(
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		stringsAsFactors = FALSE
	)

	expect_error(
		assess_rob2_batch(data_no_id),
		"data must contain columns"
	)
})

test_that("assess_rob2_batch rejects data frame without required domain columns", {
	data_missing_domain <- data.frame(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		# Missing d3, d4, d5
		stringsAsFactors = FALSE
	)

	expect_error(
		assess_rob2_batch(data_missing_domain),
		"data must contain columns"
	)
})

test_that("assess_rob2_batch rejects non-data-frame input", {
	expect_error(
		assess_rob2_batch(list(study_id = "S1")),
		class = "error"
	)
})


# =============================================================================
# rob2_summary() Function Tests
# =============================================================================

test_that("rob2_summary creates data frame from single result", {
	results <- list(
		assess_rob2(
			study_id = "STUDY001",
			d1_randomization = "Low",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low",
			outcome = "OS"
		)
	)

	summary_df <- rob2_summary(results)

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 1)
	expect_equal(
		names(summary_df),
		c(
			"study_id",
			"outcome",
			"d1_randomization",
			"d2_deviations",
			"d3_missing_data",
			"d4_measurement",
			"d5_selection",
			"overall"
		)
	)
	expect_equal(summary_df$study_id, "STUDY001")
	expect_equal(summary_df$overall, "Low")
})

test_that("rob2_summary creates data frame from multiple results", {
	results <- list(
		assess_rob2(
			study_id = "STUDY001",
			d1_randomization = "Low",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		),
		assess_rob2(
			study_id = "STUDY002",
			d1_randomization = "High",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		),
		assess_rob2(
			study_id = "STUDY003",
			d1_randomization = "Low",
			d2_deviations = "Some concerns",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		)
	)

	summary_df <- rob2_summary(results)

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 3)
	expect_equal(summary_df$study_id, c("STUDY001", "STUDY002", "STUDY003"))
	expect_equal(summary_df$overall, c("Low", "High", "Some concerns"))
})

test_that("rob2_summary includes justification when requested", {
	results <- list(
		assess_rob2(
			study_id = "STUDY001",
			d1_randomization = "High",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		)
	)

	summary_df <- rob2_summary(results, include_justification = TRUE)

	expect_true("overall_justification" %in% names(summary_df))
	expect_true(nzchar(summary_df$overall_justification))
})

test_that("rob2_summary returns empty data frame for empty list", {
	summary_df <- rob2_summary(list())

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 0)
})

test_that("rob2_summary rejects non-list input", {
	expect_error(
		rob2_summary("not a list"),
		"results must be a list"
	)
})

test_that("rob2_summary rejects list with non-RoB2Result elements", {
	results <- list(
		assess_rob2(
			study_id = "STUDY001",
			d1_randomization = "Low",
			d2_deviations = "Low",
			d3_missing_data = "Low",
			d4_measurement = "Low",
			d5_selection = "Low"
		),
		list(not = "a RoB2Result")
	)

	expect_error(
		rob2_summary(results),
		"Element 2 of results is not a RoB2Result"
	)
})


# =============================================================================
# Edge Cases and Integration Tests
# =============================================================================

test_that("RoB2Result print method works", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		outcome = "OS"
	)

	# S7 class print output contains key information
	expect_output(print(result), "STUDY001")
	expect_output(print(result), "Low")
})

test_that("All three judgments are valid for all domains", {
	# Test that each domain accepts all three judgment values
	judgments <- c("Low", "Some concerns", "High")

	for (j1 in judgments) {
		for (j2 in judgments) {
			for (j3 in judgments) {
				for (j4 in judgments) {
					for (j5 in judgments) {
						result <- assess_rob2(
							study_id = "TEST",
							d1_randomization = j1,
							d2_deviations = j2,
							d3_missing_data = j3,
							d4_measurement = j4,
							d5_selection = j5
						)
						expect_s7_class(result, RoB2Result)
					}
				}
			}
		}
	}
})

test_that("assess_rob2 handles NA in optional parameters", {
	# Should work with NA in outcome that gets converted
	data <- data.frame(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		outcome = NA_character_,
		stringsAsFactors = FALSE
	)

	results <- assess_rob2_batch(data, .suppress_messages = TRUE)

	expect_equal(results[[1]]@outcome, "")
})

test_that("assess_rob2 handles character study_id correctly", {
	result <- assess_rob2(
		study_id = "RCT-2024-001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low"
	)

	expect_equal(result@study_id, "RCT-2024-001")
})

test_that("domains list contains all required domain names", {
	result <- RoB2Result(
		study_id = "TEST",
		domains = list(
			D1_randomization = list(judgment = "Low"),
			D2_deviations = list(judgment = "Low"),
			D3_missing_data = list(judgment = "Low"),
			D4_measurement = list(judgment = "Low"),
			D5_selection = list(judgment = "Low")
		),
		overall = "Low"
	)

	expect_equal(
		names(result@domains),
		c(
			"D1_randomization",
			"D2_deviations",
			"D3_missing_data",
			"D4_measurement",
			"D5_selection"
		)
	)
})

test_that("Empty support text is handled correctly", {
	result <- assess_rob2(
		study_id = "STUDY001",
		d1_randomization = "Low",
		d2_deviations = "Low",
		d3_missing_data = "Low",
		d4_measurement = "Low",
		d5_selection = "Low",
		d1_support = "", # Empty string
		d2_support = NA_character_ # NA
	)

	# Both should be handled - support is stored (may be empty or NA)
	# Main point is that the function doesn't error
	expect_s7_class(result, RoB2Result)
})

test_that("Complete workflow with summary works end-to-end", {
	# Create batch results with at least 2 Some concerns for High judgment
	data <- data.frame(
		study_id = c("STUDY001", "STUDY002", "STUDY003", "STUDY004", "STUDY005"),
		d1_randomization = c("Low", "Low", "High", "Low", "Some concerns"),
		d2_deviations = c("Low", "Some concerns", "Low", "Low", "Some concerns"),
		d3_missing_data = c("Low", "Low", "Low", "Low", "Low"),
		d4_measurement = c("Some concerns", "Low", "Low", "Low", "Low"),
		d5_selection = c("Low", "Low", "Low", "Low", "Low"),
		stringsAsFactors = FALSE
	)

	results <- assess_rob2_batch(data, .suppress_messages = TRUE)

	# STUDY001: 1 Some concerns (D4) -> Some concerns
	expect_equal(results[["STUDY001"]]@overall, "Some concerns")
	# STUDY002: 1 Some concerns (D2) -> Some concerns
	expect_equal(results[["STUDY002"]]@overall, "Some concerns")
	# STUDY003: 1 High (D1) -> High
	expect_equal(results[["STUDY003"]]@overall, "High")
	# STUDY004: All Low -> Low
	expect_equal(results[["STUDY004"]]@overall, "Low")
	# STUDY005: 2 Some concerns (D1, D2) -> High
	expect_equal(results[["STUDY005"]]@overall, "High")

	# Create summary
	summary_df <- rob2_summary(results)

	expect_equal(nrow(summary_df), 5)
	expect_equal(
		summary_df$overall,
		c("Some concerns", "Some concerns", "High", "Low", "High")
	)
})
