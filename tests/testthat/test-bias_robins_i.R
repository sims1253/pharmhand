# Tests for ROBINS-I (Risk Of Bias In Non-randomized Studies - of Interventions)
# R/bias_robins_i.R

# =============================================================================
# ROBINSIResult S7 Class Tests
# =============================================================================

test_that("ROBINSIResult can be created with all required properties", {
	result <- ROBINSIResult(
		study_id = "OBS001",
		domains = list(
			D1_confounding = list(
				judgment = "Low",
				support = "Confounders appropriately adjusted"
			),
			D2_selection = list(judgment = "Low", support = "Appropriate selection"),
			D3_classification = list(
				judgment = "Low",
				support = "Intervention clearly defined"
			),
			D4_deviations = list(judgment = "Low", support = "No deviations"),
			D5_missing_data = list(judgment = "Low", support = "No missing data"),
			D6_measurement = list(
				judgment = "Low",
				support = "Blinded outcome assessment"
			),
			D7_selection_report = list(
				judgment = "Low",
				support = "Pre-specified outcomes reported"
			)
		),
		overall = "Low",
		overall_justification = "All domains rated Low",
		outcome = "Overall Survival",
		intervention = "Drug A",
		comparator = "Standard of Care"
	)

	expect_s7_class(result, ROBINSIResult)
	expect_equal(result@study_id, "OBS001")
	expect_equal(result@overall, "Low")
	expect_equal(result@outcome, "Overall Survival")
	expect_equal(result@intervention, "Drug A")
	expect_equal(result@comparator, "Standard of Care")
	expect_equal(length(result@domains), 7)
})

test_that("ROBINSIResult rejects missing domain", {
	expect_error(
		ROBINSIResult(
			study_id = "OBS001",
			domains = list(
				D1_confounding = list(judgment = "Low", support = "Text"),
				D2_selection = list(judgment = "Low", support = "Text")
				# Missing D3, D4, D5, D6, D7
			),
			overall = "Low"
		),
		"domains must contain all required domains"
	)
})

test_that("ROBINSIResult rejects domain without judgment", {
	expect_error(
		ROBINSIResult(
			study_id = "OBS001",
			domains = list(
				D1_confounding = list(support = "Text"), # No judgment
				D2_selection = list(judgment = "Low", support = "Text"),
				D3_classification = list(judgment = "Low", support = "Text"),
				D4_deviations = list(judgment = "Low", support = "Text"),
				D5_missing_data = list(judgment = "Low", support = "Text"),
				D6_measurement = list(judgment = "Low", support = "Text"),
				D7_selection_report = list(judgment = "Low", support = "Text")
			),
			overall = "Low"
		),
		"domain D1_confounding must have a 'judgment' element"
	)
})

test_that("ROBINSIResult rejects invalid domain judgment", {
	expect_error(
		ROBINSIResult(
			study_id = "OBS001",
			domains = list(
				D1_confounding = list(judgment = "Invalid", support = "Text"),
				D2_selection = list(judgment = "Low", support = "Text"),
				D3_classification = list(judgment = "Low", support = "Text"),
				D4_deviations = list(judgment = "Low", support = "Text"),
				D5_missing_data = list(judgment = "Low", support = "Text"),
				D6_measurement = list(judgment = "Low", support = "Text"),
				D7_selection_report = list(judgment = "Low", support = "Text")
			),
			overall = "Low"
		),
		"judgment must be one of"
	)
})

test_that("ROBINSIResult rejects invalid overall judgment", {
	expect_error(
		ROBINSIResult(
			study_id = "OBS001",
			domains = list(
				D1_confounding = list(judgment = "Low", support = "Text"),
				D2_selection = list(judgment = "Low", support = "Text"),
				D3_classification = list(judgment = "Low", support = "Text"),
				D4_deviations = list(judgment = "Low", support = "Text"),
				D5_missing_data = list(judgment = "Low", support = "Text"),
				D6_measurement = list(judgment = "Low", support = "Text"),
				D7_selection_report = list(judgment = "Low", support = "Text")
			),
			overall = "Invalid"
		),
		"overall must be one of"
	)
})

test_that("ROBINSIResult has correct default values", {
	result <- ROBINSIResult(
		study_id = "OBS001",
		domains = list(
			D1_confounding = list(judgment = "Low"),
			D2_selection = list(judgment = "Low"),
			D3_classification = list(judgment = "Low"),
			D4_deviations = list(judgment = "Low"),
			D5_missing_data = list(judgment = "Low"),
			D6_measurement = list(judgment = "Low"),
			D7_selection_report = list(judgment = "Low")
		),
		overall = "Low"
	)

	expect_equal(result@overall_justification, "")
	expect_equal(result@outcome, "")
	expect_equal(result@intervention, "")
	expect_equal(result@comparator, "")
	expect_equal(result@metadata, list())
})

test_that("ROBINSIResult summary_df computed property works", {
	result <- ROBINSIResult(
		study_id = "OBS001",
		domains = list(
			D1_confounding = list(judgment = "Serious", support = "Text1"),
			D2_selection = list(judgment = "Low", support = "Text2"),
			D3_classification = list(judgment = "Moderate", support = "Text3"),
			D4_deviations = list(judgment = "Low", support = "Text4"),
			D5_missing_data = list(judgment = "Low", support = "Text5"),
			D6_measurement = list(judgment = "Critical", support = "Text6"),
			D7_selection_report = list(judgment = "Low", support = "Text7")
		),
		overall = "Critical"
	)

	summary_df <- result@summary_df

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 7)
	expect_equal(
		names(summary_df),
		c("domain", "domain_label", "judgment", "support")
	)
	expect_equal(
		summary_df$domain,
		c(
			"D1_confounding",
			"D2_selection",
			"D3_classification",
			"D4_deviations",
			"D5_missing_data",
			"D6_measurement",
			"D7_selection_report"
		)
	)
	expect_equal(
		summary_df$domain_label,
		c(
			"D1: Confounding",
			"D2: Selection of participants",
			"D3: Classification of interventions",
			"D4: Deviations from intended interventions",
			"D5: Missing data",
			"D6: Measurement of outcomes",
			"D7: Selection of reported result"
		)
	)
})

test_that("ROBINSIResult judgment_counts computed property works", {
	result <- ROBINSIResult(
		study_id = "OBS001",
		domains = list(
			D1_confounding = list(judgment = "Low"),
			D2_selection = list(judgment = "Low"),
			D3_classification = list(judgment = "Moderate"),
			D4_deviations = list(judgment = "Moderate"),
			D5_missing_data = list(judgment = "Serious"),
			D6_measurement = list(judgment = "Critical"),
			D7_selection_report = list(judgment = "No information")
		),
		overall = "Critical"
	)

	counts <- result@judgment_counts

	expect_type(counts, "list")
	expect_equal(counts$n_low, 2)
	expect_equal(counts$n_moderate, 2)
	expect_equal(counts$n_serious, 1)
	expect_equal(counts$n_critical, 1)
	expect_equal(counts$n_no_info, 1)
})

test_that("ROBINSIResult worst_domain computed property returns most severe", {
	# Critical is most severe
	result1 <- ROBINSIResult(
		study_id = "OBS001",
		domains = list(
			D1_confounding = list(judgment = "Low"),
			D2_selection = list(judgment = "Moderate"),
			D3_classification = list(judgment = "Serious"),
			D4_deviations = list(judgment = "Critical"),
			D5_missing_data = list(judgment = "Low"),
			D6_measurement = list(judgment = "Low"),
			D7_selection_report = list(judgment = "Low")
		),
		overall = "Critical"
	)
	expect_equal(result1@worst_domain, "D4_deviations")

	# Serious when no Critical
	result2 <- ROBINSIResult(
		study_id = "OBS002",
		domains = list(
			D1_confounding = list(judgment = "Serious"),
			D2_selection = list(judgment = "Low"),
			D3_classification = list(judgment = "Low"),
			D4_deviations = list(judgment = "Low"),
			D5_missing_data = list(judgment = "Low"),
			D6_measurement = list(judgment = "Low"),
			D7_selection_report = list(judgment = "Low")
		),
		overall = "Serious"
	)
	expect_equal(result2@worst_domain, "D1_confounding")

	# Low when all domains are Low
	result3 <- ROBINSIResult(
		study_id = "OBS003",
		domains = list(
			D1_confounding = list(judgment = "Low"),
			D2_selection = list(judgment = "Low"),
			D3_classification = list(judgment = "Low"),
			D4_deviations = list(judgment = "Low"),
			D5_missing_data = list(judgment = "Low"),
			D6_measurement = list(judgment = "Low"),
			D7_selection_report = list(judgment = "Low")
		),
		overall = "Low"
	)
	expect_equal(result3@worst_domain, "D1_confounding")
})


# =============================================================================
# assess_robins_i() Function Tests
# =============================================================================

test_that("assess_robins_i creates valid result with all Low judgments", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)

	expect_s7_class(result, ROBINSIResult)
	expect_equal(result@overall, "Low")
	expect_equal(result@study_id, "OBS001")
	expect_equal(result@judgment_counts$n_low, 7)
	expect_equal(result@judgment_counts$n_moderate, 0)
	expect_equal(result@judgment_counts$n_serious, 0)
	expect_equal(result@judgment_counts$n_critical, 0)
	expect_equal(result@judgment_counts$n_no_info, 0)
})

test_that("assess_robins_i auto-calculates overall as Critical with any
           Critical domain", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Critical",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)

	expect_equal(result@overall, "Critical")
})

test_that("assess_robins_i auto-calculates overall as Serious with any
           Serious (no Critical)", {
	# Single Serious domain
	result1 <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Serious",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result1@overall, "Serious")

	# Multiple Serious domains
	result2 <- assess_robins_i(
		study_id = "OBS002",
		d1_confounding = "Serious",
		d2_selection = "Serious",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result2@overall, "Serious")
})

test_that("assess_robins_i auto-calculates overall as Moderate with any
           Moderate (no Serious/Critical)", {
	# Single Moderate domain
	result1 <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Moderate",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result1@overall, "Moderate")

	# Multiple Moderate domains
	result2 <- assess_robins_i(
		study_id = "OBS002",
		d1_confounding = "Moderate",
		d2_selection = "Moderate",
		d3_classification = "Moderate",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result2@overall, "Moderate")
})

test_that("assess_robins_i auto-calculates overall as No information with
           any No information", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "No information",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)

	expect_equal(result@overall, "No information")
})

test_that("assess_robins_i auto-calculates overall correctly for mixed
           scenarios", {
	# Scenario: Critical + Serious -> Critical
	result1 <- assess_robins_i(
		study_id = "S1",
		d1_confounding = "Critical",
		d2_selection = "Serious",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result1@overall, "Critical")

	# Scenario: Serious + Moderate -> Serious
	result2 <- assess_robins_i(
		study_id = "S2",
		d1_confounding = "Serious",
		d2_selection = "Moderate",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result2@overall, "Serious")

	# Scenario: Moderate + No information -> Moderate
	# (No information only with Low/Moderate)
	result3 <- assess_robins_i(
		study_id = "S3",
		d1_confounding = "Moderate",
		d2_selection = "No information",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result3@overall, "Moderate")

	# Scenario: All 7 Moderate -> Moderate
	result4 <- assess_robins_i(
		study_id = "S4",
		d1_confounding = "Moderate",
		d2_selection = "Moderate",
		d3_classification = "Moderate",
		d4_deviations = "Moderate",
		d5_missing_data = "Moderate",
		d6_measurement = "Moderate",
		d7_selection_report = "Moderate"
	)
	expect_equal(result4@overall, "Moderate")
})

test_that("assess_robins_i accepts custom overall judgment", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		overall = "Moderate",
		overall_justification = "Custom justification for moderate risk"
	)

	expect_equal(result@overall, "Moderate")
	expect_equal(
		result@overall_justification,
		"Custom justification for moderate risk"
	)
})

test_that("assess_robins_i accepts optional outcome and intervention
           parameters", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		outcome = "Overall Survival",
		intervention = "Drug A",
		comparator = "Standard of Care"
	)

	expect_equal(result@outcome, "Overall Survival")
	expect_equal(result@intervention, "Drug A")
	expect_equal(result@comparator, "Standard of Care")
})

test_that("assess_robins_i accepts optional support text", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Serious",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Moderate",
		d7_selection_report = "Low",
		d1_support = "No adjustment for confounders",
		d6_support = "Outcome assessor not fully blinded"
	)

	expect_true(nzchar(result@domains[["D1_confounding"]]$support))
	expect_equal(
		unname(result@domains[["D1_confounding"]]$support),
		"No adjustment for confounders"
	)
	expect_true(nzchar(result@domains[["D6_measurement"]]$support))
	expect_equal(
		unname(result@domains[["D6_measurement"]]$support),
		"Outcome assessor not fully blinded"
	)
})

test_that("assess_robins_i accepts optional metadata", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		metadata = list(reviewer = "Dr. Smith", date = "2024-01-15")
	)

	expect_equal(result@metadata$reviewer, "Dr. Smith")
	expect_equal(result@metadata$date, "2024-01-15")
})

test_that("assess_robins_i rejects missing study_id", {
	expect_error(
		assess_robins_i(
			study_id = NULL,
			d1_confounding = "Low",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		"study_id is required"
	)
})

test_that("assess_robins_i rejects invalid domain judgment", {
	expect_error(
		assess_robins_i(
			study_id = "OBS001",
			d1_confounding = "Invalid",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		"must be one of"
	)
})

test_that("assess_robins_i rejects NULL domain judgment", {
	expect_error(
		assess_robins_i(
			study_id = "OBS001",
			d1_confounding = NULL,
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		regexp = "judgment|D1_confounding"
	)
})

test_that("assess_robins_i rejects empty string domain judgment", {
	expect_error(
		assess_robins_i(
			study_id = "OBS001",
			d1_confounding = "",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		"D1_confounding judgment is required"
	)
})


# =============================================================================
# .calculate_robins_i_overall() Internal Function Tests
# =============================================================================

test_that(".calculate_robins_i_overall returns Low for all Low judgments", {
	judgments <- rep("Low", 7)
	expect_equal(.calculate_robins_i_overall(judgments), "Low")
})

test_that(".calculate_robins_i_overall returns Critical for any Critical
           judgment", {
	# One Critical
	judgments1 <- c("Critical", "Low", "Low", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_robins_i_overall(judgments1), "Critical")

	# Multiple Critical
	judgments2 <- c("Critical", "Critical", "Low", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_robins_i_overall(judgments2), "Critical")
})

test_that(".calculate_robins_i_overall returns Serious for any Serious
           (no Critical)", {
	# One Serious
	judgments1 <- c("Serious", "Low", "Low", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_robins_i_overall(judgments1), "Serious")

	# Multiple Serious
	judgments2 <- c("Serious", "Serious", "Serious", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_robins_i_overall(judgments2), "Serious")
})

test_that(".calculate_robins_i_overall returns Moderate for any Moderate
           (no Serious/Critical)", {
	# One Moderate
	judgments1 <- c("Moderate", "Low", "Low", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_robins_i_overall(judgments1), "Moderate")

	# All Moderate
	judgments2 <- rep("Moderate", 7)
	expect_equal(.calculate_robins_i_overall(judgments2), "Moderate")
})

test_that(".calculate_robins_i_overall returns No information for any
           No information", {
	judgments <- c("No information", "Low", "Low", "Low", "Low", "Low", "Low")
	expect_equal(.calculate_robins_i_overall(judgments), "No information")
})

test_that(".calculate_robins_i_overall rejects wrong length input", {
	expect_error(
		.calculate_robins_i_overall(c("Low", "Low", "Low")),
		"judgments must be a character vector of length 7"
	)
})


# =============================================================================
# .generate_robins_i_justification() Internal Function Tests
# =============================================================================

test_that(".generate_robins_i_justification returns correct text for Low", {
	judgments <- rep("Low", 7)
	just <- .generate_robins_i_justification(judgments, "Low")
	expect_equal(just, "All domains rated Low risk of bias")
})

test_that(".generate_robins_i_justification returns correct text for
           Critical", {
	judgments <- c("Critical", "Low", "Low", "Low", "Low", "Low", "Low")
	just <- .generate_robins_i_justification(judgments, "Critical")
	expect_true(grepl("Critical", just, fixed = TRUE))
	expect_true(grepl("confounding", just, ignore.case = TRUE))
})

test_that(".generate_robins_i_justification returns correct text for Critical
           with multiple", {
	judgments <- c("Critical", "Critical", "Low", "Low", "Low", "Low", "Low")
	just <- .generate_robins_i_justification(judgments, "Critical")
	expect_true(grepl("domains", just, ignore.case = TRUE))
	expect_true(grepl("s$", trimws(just))) # plural "domains"
})

test_that(".generate_robins_i_justification returns correct text for Serious", {
	judgments <- c("Serious", "Low", "Low", "Low", "Low", "Low", "Low")
	just <- .generate_robins_i_justification(judgments, "Serious")
	expect_true(grepl("Serious", just, fixed = TRUE))
	expect_true(grepl("confounding", just, ignore.case = TRUE))
})

test_that(".generate_robins_i_justification returns correct text for
           Moderate", {
	judgments <- c("Moderate", "Low", "Low", "Low", "Low", "Low", "Low")
	just <- .generate_robins_i_justification(judgments, "Moderate")
	expect_true(grepl("Moderate", just, fixed = TRUE))
	expect_true(grepl("confounding", just, ignore.case = TRUE))
})

test_that(".generate_robins_i_justification returns correct text for No
           information", {
	judgments <- c("No information", "Low", "Low", "Low", "Low", "Low", "Low")
	just <- .generate_robins_i_justification(judgments, "No information")
	expect_true(grepl("No information", just, fixed = TRUE))
	expect_true(grepl("confounding", just, ignore.case = TRUE))
	expect_true(grepl("Low", just, fixed = TRUE))
})


# =============================================================================
# assess_robins_i_batch() Function Tests
# =============================================================================

test_that("assess_robins_i_batch processes single study data frame", {
	data <- data.frame(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		stringsAsFactors = FALSE
	)

	results <- assess_robins_i_batch(data)

	expect_type(results, "list")
	expect_length(results, 1)
	expect_named(results, "OBS001")
	expect_s7_class(results[[1]], ROBINSIResult)
})

test_that("assess_robins_i_batch processes multiple studies", {
	data <- data.frame(
		study_id = c("OBS001", "OBS002", "OBS003"),
		d1_confounding = c("Low", "Moderate", "Serious"),
		d2_selection = c("Low", "Low", "Low"),
		d3_classification = c("Low", "Low", "Low"),
		d4_deviations = c("Low", "Low", "Low"),
		d5_missing_data = c("Low", "Low", "Low"),
		d6_measurement = c("Low", "Low", "Low"),
		d7_selection_report = c("Low", "Low", "Low"),
		stringsAsFactors = FALSE
	)

	results <- assess_robins_i_batch(data, .suppress_messages = TRUE)

	expect_type(results, "list")
	expect_length(results, 3)
	expect_named(results, c("OBS001", "OBS002", "OBS003"))
	expect_s7_class(results[[1]], ROBINSIResult)
	expect_s7_class(results[[2]], ROBINSIResult)
	expect_s7_class(results[[3]], ROBINSIResult)
	expect_equal(results[["OBS001"]]@overall, "Low")
	expect_equal(results[["OBS002"]]@overall, "Moderate")
	expect_equal(results[["OBS003"]]@overall, "Serious")
})

test_that("assess_robins_i_batch includes optional columns", {
	data <- data.frame(
		study_id = "OBS001",
		d1_confounding = "Serious",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Moderate",
		d7_selection_report = "Low",
		d1_support = "No adjustment for confounders",
		d6_support = "Blinded outcome assessment",
		outcome = "OS",
		intervention = "Drug A",
		comparator = "Placebo",
		stringsAsFactors = FALSE
	)

	results <- assess_robins_i_batch(data, .suppress_messages = TRUE)

	expect_true(nzchar(results[[1]]@domains[["D1_confounding"]]$support))
	expect_true(nzchar(results[[1]]@domains[["D6_measurement"]]$support))
	expect_equal(results[[1]]@outcome, "OS")
	expect_equal(results[[1]]@intervention, "Drug A")
	expect_equal(results[[1]]@comparator, "Placebo")
})

test_that("assess_robins_i_batch accepts custom overall judgments", {
	data <- data.frame(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		overall = "Critical",
		overall_justification = "Custom override to Critical",
		stringsAsFactors = FALSE
	)

	results <- assess_robins_i_batch(data, .suppress_messages = TRUE)

	expect_equal(results[[1]]@overall, "Critical")
	expect_equal(
		results[[1]]@overall_justification,
		"Custom override to Critical"
	)
})

test_that("assess_robins_i_batch handles missing optional columns gracefully", {
	data <- data.frame(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		stringsAsFactors = FALSE
	)

	results <- assess_robins_i_batch(data, .suppress_messages = TRUE)

	expect_true(length(results[[1]]@domains[["D1_confounding"]]$support) >= 0)
	expect_true(nzchar(results[[1]]@overall_justification))
})

test_that("assess_robins_i_batch rejects data frame without study_id column", {
	data_no_id <- data.frame(
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		stringsAsFactors = FALSE
	)

	expect_error(
		assess_robins_i_batch(data_no_id),
		"data must contain columns"
	)
})

test_that("assess_robins_i_batch rejects data frame without required domain
           columns", {
	data_missing_domain <- data.frame(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		# Missing d3, d4, d5, d6, d7
		stringsAsFactors = FALSE
	)

	expect_error(
		assess_robins_i_batch(data_missing_domain),
		"data must contain columns"
	)
})

test_that("assess_robins_i_batch rejects non-data-frame input", {
	expect_error(
		assess_robins_i_batch(list(study_id = "OBS001")),
		class = "error"
	)
})


# =============================================================================
# robins_i_summary() Function Tests
# =============================================================================

test_that("robins_i_summary creates data frame from single result", {
	results <- list(
		assess_robins_i(
			study_id = "OBS001",
			d1_confounding = "Low",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low",
			outcome = "OS",
			intervention = "Drug A",
			comparator = "Placebo"
		)
	)

	summary_df <- robins_i_summary(results)

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 1)
	expect_equal(
		names(summary_df),
		c(
			"study_id",
			"outcome",
			"intervention",
			"comparator",
			"d1_confounding",
			"d2_selection",
			"d3_classification",
			"d4_deviations",
			"d5_missing_data",
			"d6_measurement",
			"d7_selection_report",
			"overall"
		)
	)
	expect_equal(summary_df$study_id, "OBS001")
	expect_equal(summary_df$overall, "Low")
	expect_equal(summary_df$outcome, "OS")
})

test_that("robins_i_summary creates data frame from multiple results", {
	results <- list(
		assess_robins_i(
			study_id = "OBS001",
			d1_confounding = "Low",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		assess_robins_i(
			study_id = "OBS002",
			d1_confounding = "Serious",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		assess_robins_i(
			study_id = "OBS003",
			d1_confounding = "Moderate",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		)
	)

	summary_df <- robins_i_summary(results)

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 3)
	expect_equal(summary_df$study_id, c("OBS001", "OBS002", "OBS003"))
	expect_equal(summary_df$overall, c("Low", "Serious", "Moderate"))
})

test_that("robins_i_summary includes justification when requested", {
	results <- list(
		assess_robins_i(
			study_id = "OBS001",
			d1_confounding = "Critical",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		)
	)

	summary_df <- robins_i_summary(results, include_justification = TRUE)

	expect_true("overall_justification" %in% names(summary_df))
	expect_true(nzchar(summary_df$overall_justification))
})

test_that("robins_i_summary returns empty data frame for empty list", {
	summary_df <- robins_i_summary(list())

	expect_s3_class(summary_df, "data.frame")
	expect_equal(nrow(summary_df), 0)
	# Check that all expected columns are present
	expect_true("study_id" %in% names(summary_df))
	expect_true("overall" %in% names(summary_df))
	expect_true("d1_confounding" %in% names(summary_df))
})

test_that("robins_i_summary rejects non-list input", {
	expect_error(
		robins_i_summary("not a list"),
		"results must be a list"
	)
})

test_that("robins_i_summary rejects list with non-ROBINSIResult elements", {
	results <- list(
		assess_robins_i(
			study_id = "OBS001",
			d1_confounding = "Low",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		list(not = "a ROBINSIResult")
	)

	expect_error(
		robins_i_summary(results),
		"Element 2 of results is not a ROBINSIResult"
	)
})


# =============================================================================
# Edge Cases and Integration Tests
# =============================================================================

test_that("ROBINSIResult print method works", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		outcome = "OS"
	)

	expect_output(print(result), "OBS001")
	expect_output(print(result), "Low")
	expect_output(print(result), "OS")
})

test_that("All five judgments are valid for all domains", {
	# Test representative combinations to avoid 5^7 = 78125 iterations
	# Test each judgment type at least once in each domain position
	judgments <- c("Low", "Moderate", "Serious", "Critical", "No information")

	# Test all Low and all Critical
	result_all_low <- assess_robins_i(
		study_id = "TEST_ALL_LOW",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_s7_class(result_all_low, ROBINSIResult)
	expect_equal(result_all_low@overall, "Low")

	result_all_critical <- assess_robins_i(
		study_id = "TEST_ALL_CRITICAL",
		d1_confounding = "Critical",
		d2_selection = "Critical",
		d3_classification = "Critical",
		d4_deviations = "Critical",
		d5_missing_data = "Critical",
		d6_measurement = "Critical",
		d7_selection_report = "Critical"
	)
	expect_s7_class(result_all_critical, ROBINSIResult)
	expect_equal(result_all_critical@overall, "Critical")

	# Test mixed judgments - each judgment type appears at least once
	result_mixed <- assess_robins_i(
		study_id = "TEST_MIXED",
		d1_confounding = "Low",
		d2_selection = "Moderate",
		d3_classification = "Serious",
		d4_deviations = "Critical",
		d5_missing_data = "No information",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_s7_class(result_mixed, ROBINSIResult)
	expect_equal(result_mixed@overall, "Critical") # Critical takes priority

	# Test boundary cases: single domain differs
	for (judgment in judgments) {
		if (judgment == "Low") {
			next
		} # Already tested

		result_single <- assess_robins_i(
			study_id = paste0("TEST_SINGLE_", judgment),
			d1_confounding = judgment,
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		)
		expect_s7_class(result_single, ROBINSIResult)
		expect_equal(result_single@overall, judgment)
	}

	# Verify all domains accept all judgment values
	# Test a representative subset: all judgments for d1, and Low/Critical for rest
	limited_judgments <- c("Low", "Critical")
	for (d1 in judgments) {
		for (d2 in limited_judgments) {
			for (d3 in limited_judgments) {
				for (d4 in limited_judgments) {
					for (d5 in limited_judgments) {
						for (d6 in limited_judgments) {
							for (d7 in limited_judgments) {
								result <- assess_robins_i(
									study_id = "TEST_BOUNDARY",
									d1_confounding = d1,
									d2_selection = d2,
									d3_classification = d3,
									d4_deviations = d4,
									d5_missing_data = d5,
									d6_measurement = d6,
									d7_selection_report = d7
								)
								expect_s7_class(result, ROBINSIResult)
							}
						}
					}
				}
			}
		}
	}
})

test_that("assess_robins_i handles NA in optional parameters", {
	data <- data.frame(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		outcome = NA_character_,
		stringsAsFactors = FALSE
	)

	results <- assess_robins_i_batch(data, .suppress_messages = TRUE)

	expect_equal(results[[1]]@outcome, "")
})

test_that("assess_robins_i handles character study_id correctly", {
	result <- assess_robins_i(
		study_id = "OBS-2024-001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)

	expect_equal(result@study_id, "OBS-2024-001")
})

test_that("domains list contains all required domain names", {
	result <- ROBINSIResult(
		study_id = "TEST",
		domains = list(
			D1_confounding = list(judgment = "Low"),
			D2_selection = list(judgment = "Low"),
			D3_classification = list(judgment = "Low"),
			D4_deviations = list(judgment = "Low"),
			D5_missing_data = list(judgment = "Low"),
			D6_measurement = list(judgment = "Low"),
			D7_selection_report = list(judgment = "Low")
		),
		overall = "Low"
	)

	expect_equal(
		names(result@domains),
		c(
			"D1_confounding",
			"D2_selection",
			"D3_classification",
			"D4_deviations",
			"D5_missing_data",
			"D6_measurement",
			"D7_selection_report"
		)
	)
})

test_that("Empty support text is handled correctly", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Low",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low",
		d1_support = "", # Empty string
		d2_support = NA_character_ # NA
	)

	expect_s7_class(result, ROBINSIResult)
})

test_that("Complete workflow with summary works end-to-end", {
	# Create batch results testing various judgment scenarios
	data <- data.frame(
		study_id = c("OBS001", "OBS002", "OBS003", "OBS004", "OBS005"),
		d1_confounding = c(
			"Low",
			"Moderate",
			"Serious",
			"Critical",
			"No information"
		),
		d2_selection = c("Low", "Low", "Low", "Low", "Low"),
		d3_classification = c("Low", "Low", "Low", "Low", "Low"),
		d4_deviations = c("Low", "Low", "Low", "Low", "Low"),
		d5_missing_data = c("Low", "Low", "Low", "Low", "Low"),
		d6_measurement = c("Low", "Low", "Low", "Low", "Low"),
		d7_selection_report = c("Low", "Low", "Low", "Low", "Low"),
		stringsAsFactors = FALSE
	)

	results <- assess_robins_i_batch(data, .suppress_messages = TRUE)

	# OBS001: All Low -> Low
	expect_equal(results[["OBS001"]]@overall, "Low")
	# OBS002: One Moderate -> Moderate
	expect_equal(results[["OBS002"]]@overall, "Moderate")
	# OBS003: One Serious -> Serious
	expect_equal(results[["OBS003"]]@overall, "Serious")
	# OBS004: One Critical -> Critical
	expect_equal(results[["OBS004"]]@overall, "Critical")
	# OBS005: One No information -> No information
	expect_equal(results[["OBS005"]]@overall, "No information")

	# Create summary
	summary_df <- robins_i_summary(results)

	expect_equal(nrow(summary_df), 5)
	expect_equal(
		summary_df$overall,
		c("Low", "Moderate", "Serious", "Critical", "No information")
	)
})

test_that("ROBINS-I algorithm prioritizes correctly across all judgment
           levels", {
	# Critical takes priority over everything
	result_critical <- assess_robins_i(
		study_id = "T1",
		d1_confounding = "Critical",
		d2_selection = "Serious",
		d3_classification = "Moderate",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result_critical@overall, "Critical")

	# Serious takes priority over Moderate and No information
	result_serious <- assess_robins_i(
		study_id = "T2",
		d1_confounding = "Low",
		d2_selection = "Serious",
		d3_classification = "Moderate",
		d4_deviations = "No information",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result_serious@overall, "Serious")

	# Moderate takes priority over No information
	result_moderate <- assess_robins_i(
		study_id = "T3",
		d1_confounding = "Moderate",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "No information",
		d7_selection_report = "Low"
	)
	expect_equal(result_moderate@overall, "Moderate")

	# No information only when no Serious/Critical/Moderate
	result_no_info <- assess_robins_i(
		study_id = "T4",
		d1_confounding = "No information",
		d2_selection = "Low",
		d3_classification = "Low",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Low",
		d7_selection_report = "Low"
	)
	expect_equal(result_no_info@overall, "No information")
})

test_that("domains are in correct order in summary", {
	result <- assess_robins_i(
		study_id = "OBS001",
		d1_confounding = "Serious",
		d2_selection = "Low",
		d3_classification = "Moderate",
		d4_deviations = "Low",
		d5_missing_data = "Low",
		d6_measurement = "Critical",
		d7_selection_report = "Low"
	)

	summary_df <- result@summary_df

	# Verify domain order
	expect_equal(summary_df$domain[1], "D1_confounding")
	expect_equal(summary_df$domain[2], "D2_selection")
	expect_equal(summary_df$domain[3], "D3_classification")
	expect_equal(summary_df$domain[4], "D4_deviations")
	expect_equal(summary_df$domain[5], "D5_missing_data")
	expect_equal(summary_df$domain[6], "D6_measurement")
	expect_equal(summary_df$domain[7], "D7_selection_report")

	# Verify corresponding judgments
	expect_equal(summary_df$judgment[1], "Serious")
	expect_equal(summary_df$judgment[3], "Moderate")
	expect_equal(summary_df$judgment[6], "Critical")
})

test_that("robins_i_summary preserves row names from results list", {
	results <- list(
		study_a = assess_robins_i(
			study_id = "study_a",
			d1_confounding = "Low",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		),
		study_b = assess_robins_i(
			study_id = "study_b",
			d1_confounding = "Serious",
			d2_selection = "Low",
			d3_classification = "Low",
			d4_deviations = "Low",
			d5_missing_data = "Low",
			d6_measurement = "Low",
			d7_selection_report = "Low"
		)
	)

	summary_df <- robins_i_summary(results)

	expect_equal(summary_df$study_id[1], "study_a")
	expect_equal(summary_df$study_id[2], "study_b")
	expect_equal(summary_df$overall[1], "Low")
	expect_equal(summary_df$overall[2], "Serious")
})

test_that("Justification generation works for all judgment levels", {
	# Test all possible overall judgments generate valid justifications
	judgments_all_critical <- c(rep("Critical", 7))
	just_critical <- .generate_robins_i_justification(
		judgments_all_critical,
		"Critical"
	)
	expect_true(nzchar(just_critical))
	expect_true(grepl("Critical", just_critical, fixed = TRUE))

	judgments_all_serious <- c(rep("Serious", 7))
	just_serious <- .generate_robins_i_justification(
		judgments_all_serious,
		"Serious"
	)
	expect_true(nzchar(just_serious))
	expect_true(grepl("Serious", just_serious, fixed = TRUE))

	judgments_all_moderate <- c(rep("Moderate", 7))
	just_moderate <- .generate_robins_i_justification(
		judgments_all_moderate,
		"Moderate"
	)
	expect_true(nzchar(just_moderate))
	expect_true(grepl("Moderate", just_moderate, fixed = TRUE))

	judgments_all_no_info <- c(rep("No information", 7))
	just_no_info <- .generate_robins_i_justification(
		judgments_all_no_info,
		"No information"
	)
	expect_true(nzchar(just_no_info))
	expect_true(grepl("No information", just_no_info, fixed = TRUE))
})
