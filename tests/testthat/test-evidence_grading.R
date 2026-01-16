# Tests for IQWiG Evidence Grading Functions
# R/evidence_grading.R

# =============================================================================
# EvidenceGrade S7 Class Tests
# =============================================================================

test_that("EvidenceGrade can be created with all required properties", {
	grade <- EvidenceGrade(
		grade = "proof",
		grade_de = "Beleg",
		direction = "benefit",
		certainty = 0.9,
		n_studies = 5L,
		domains = list(
			limitations = list(level = "low", rating = 1, notes = "Low RoB"),
			inconsistency = list(level = "low", rating = 1, notes = "Low I2"),
			imprecision = list(level = "low", rating = 1, notes = "Precise"),
			indirectness = list(level = "low", rating = 1, notes = "Direct"),
			publication_bias = list(level = "low", rating = 1, notes = "No bias")
		),
		justification = "High certainty from 5 RCTs"
	)

	expect_s7_class(grade, EvidenceGrade)
	expect_equal(grade@grade, "proof")
	expect_equal(grade@grade_de, "Beleg")
	expect_equal(grade@direction, "benefit")
	expect_equal(grade@certainty, 0.9)
	expect_equal(grade@n_studies, 5L)
})

test_that("EvidenceGrade rejects invalid grade", {
	expect_error(
		EvidenceGrade(grade = "invalid", grade_de = "Invalid"),
		"grade must be one of"
	)
})

test_that("EvidenceGrade rejects invalid direction", {
	expect_error(
		EvidenceGrade(grade = "proof", direction = "invalid"),
		"direction must be 'benefit', 'harm', or 'none'"
	)
})

test_that("EvidenceGrade has correct default values", {
	grade <- EvidenceGrade(grade = "indication", grade_de = "Hinweis")

	expect_equal(grade@direction, "none")
	expect_equal(grade@certainty, NA_real_)
	expect_equal(grade@n_studies, NA_integer_)
	expect_equal(grade@domains, list())
	expect_equal(grade@justification, "")
	expect_equal(grade@metadata, list())
})

# =============================================================================
# grade_evidence() with MetaResult
# =============================================================================

test_that("grade_evidence works with MetaResult (high quality - proof)", {
	meta_res <- MetaResult(
		estimate = 0.72,
		ci = c(0.62, 0.84),
		p_value = 0.0001,
		n = 5L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 15, tau2 = 0.005, Q_pvalue = 0.3),
		method = "REML meta-analysis"
	)

	rob_results <- list(
		assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study5", "Low", "Low", "Low", "Low", "Low")
	)

	grade <- grade_evidence(meta_res, rob_results, direction = "benefit")

	expect_s7_class(grade, EvidenceGrade)
	expect_equal(grade@grade, "proof")
	expect_equal(grade@grade_de, "Beleg")
	expect_equal(grade@direction, "benefit")
	expect_equal(grade@n_studies, 5L)
	expect_true(grade@certainty > 0.8)
})

test_that("grade_evidence works with MetaResult (moderate - indication)", {
	meta_res <- MetaResult(
		estimate = 0.78,
		ci = c(0.62, 0.98),
		p_value = 0.03,
		n = 4L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 45, tau2 = 0.02, Q_pvalue = 0.1),
		method = "REML meta-analysis"
	)

	rob_results <- list(
		assess_rob2("Study1", "Low", "Some concerns", "Low", "Low", "Low"),
		assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study3", "Some concerns", "Low", "Low", "Low", "Low"),
		assess_rob2("Study4", "Low", "Low", "Low", "Low", "Low")
	)

	grade <- grade_evidence(meta_res, rob_results, direction = "benefit")

	expect_s7_class(grade, EvidenceGrade)
	expect_equal(grade@grade, "indication")
	expect_equal(grade@grade_de, "Hinweis")
})

test_that("grade_evidence works with MetaResult (low quality - hint)", {
	meta_res <- MetaResult(
		estimate = 0.82,
		ci = c(0.65, 1.04),
		p_value = 0.10,
		n = 3L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 55, tau2 = 0.03, Q_pvalue = 0.08),
		method = "REML meta-analysis"
	)

	rob_results <- list(
		assess_rob2("Study1", "High", "High", "High", "Low", "Low"),
		assess_rob2("Study2", "Low", "Some concerns", "High", "Low", "Low"),
		assess_rob2("Study3", "Some concerns", "Low", "Low", "Low", "Low")
	)

	grade <- grade_evidence(meta_res, rob_results, direction = "benefit")

	expect_s7_class(grade, EvidenceGrade)
	expect_equal(grade@grade, "hint")
	expect_equal(grade@grade_de, "Anhaltspunkt")
})

test_that("grade_evidence works with MetaResult (no proof)", {
	meta_res <- MetaResult(
		estimate = 0.95,
		ci = c(0.75, 1.20),
		p_value = 0.67,
		n = 2L,
		effect_measure = "hr",
		heterogeneity = list(I2 = NA_real_, tau2 = NA_real_, Q_pvalue = NA_real_),
		method = "Fixed-effect meta-analysis"
	)

	rob_results <- list(
		assess_rob2("Study1", "High", "High", "High", "High", "High"),
		assess_rob2("Study2", "High", "High", "High", "High", "High")
	)

	grade <- grade_evidence(meta_res, rob_results)

	expect_s7_class(grade, EvidenceGrade)
	expect_equal(grade@grade, "none")
	expect_equal(grade@grade_de, "Kein Beleg")
})

test_that("grade_evidence with MetaResult uses default RoB when NULL", {
	meta_res <- MetaResult(
		estimate = 0.70,
		ci = c(0.60, 0.82),
		p_value = 0.00001,
		n = 5L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 10, tau2 = 0.003, Q_pvalue = 0.4)
	)

	grade <- grade_evidence(meta_res, rob_results = NULL, direction = "benefit")

	expect_s7_class(grade, EvidenceGrade)
	expect_equal(grade@domains$limitations$level, "low")
})

# =============================================================================
# grade_evidence() with ComparisonResult
# =============================================================================

test_that("grade_evidence works with ComparisonResult (single study)", {
	comp_res <- ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		effect_measure = "hr"
	)

	rob_result <- assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low")

	grade <- grade_evidence(
		comp_res,
		rob_results = rob_result,
		n_studies_override = 1L,
		direction = "benefit"
	)

	expect_s7_class(grade, EvidenceGrade)
	expect_equal(grade@n_studies, 1L)
	expect_in(grade@grade, c("indication", "proof"))
})

test_that("grade_evidence with ComparisonResult defaults n_studies to 1", {
	comp_res <- ComparisonResult(
		estimate = 0.80,
		ci = c(0.68, 0.94),
		p_value = 0.007,
		effect_measure = "hr"
	)

	grade <- grade_evidence(comp_res, rob_results = NULL, direction = "benefit")

	expect_equal(grade@n_studies, 1L)
})

test_that("grade_evidence with ComparisonResult uses n_studies_override", {
	comp_res <- ComparisonResult(
		estimate = 0.80,
		ci = c(0.68, 0.94),
		p_value = 0.007,
		effect_measure = "hr"
	)

	grade <- grade_evidence(
		comp_res,
		rob_results = NULL,
		n_studies_override = 3L,
		direction = "benefit"
	)

	expect_equal(grade@n_studies, 3L)
})

# =============================================================================
# grade_evidence() Error Handling
# =============================================================================

test_that("grade_evidence rejects non-MetaResult/ComparisonResult", {
	expect_error(
		grade_evidence(list(estimate = 0.5)),
		"meta_result must be a MetaResult or ComparisonResult object"
	)

	expect_error(
		grade_evidence(data.frame(x = 1)),
		"meta_result must be a MetaResult or ComparisonResult object"
	)
})

test_that("grade_evidence handles CI including null for proof downgrade", {
	meta_res <- MetaResult(
		estimate = 0.85,
		ci = c(0.70, 1.02),
		p_value = 0.08,
		n = 5L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 20, tau2 = 0.01)
	)

	rob_results <- list(
		assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study5", "Low", "Low", "Low", "Low", "Low")
	)

	grade <- grade_evidence(meta_res, rob_results, direction = "benefit")

	expect_true(grade@grade != "proof")
})

# =============================================================================
# assess_evidence_domains() Tests
# =============================================================================

test_that("assess_evidence_domains returns all five domains", {
	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(I2 = 25, tau2 = 0.01, Q_pvalue = 0.2),
		rob_results = NULL,
		n_studies = 5L,
		publication_bias = NULL,
		indirectness = NULL,
		ci_level = 0.95,
		effect_measure = "hr"
	)

	expect_type(domains, "list")
	expect_equal(
		names(domains),
		c(
			"limitations",
			"inconsistency",
			"imprecision",
			"indirectness",
			"publication_bias"
		)
	)

	for (d in names(domains)) {
		expect_true(is.list(domains[[d]]))
		expect_true("level" %in% names(domains[[d]]))
		expect_true("rating" %in% names(domains[[d]]))
		expect_true("notes" %in% names(domains[[d]]))
	}
})

test_that("assess_evidence_domains assesses limitations with RoB results", {
	rob_results <- list(
		assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low")
	)

	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = rob_results,
		n_studies = 3L
	)

	expect_equal(domains$limitations$level, "low")
	expect_true(domains$limitations$rating >= 0.9)
})

test_that("assess_evidence_domains handles single RoB2Result", {
	rob_result <- assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low")

	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = rob_result,
		n_studies = 1L
	)

	expect_equal(domains$limitations$level, "low")
})

test_that("assess_evidence_domains handles mixed RoB judgments", {
	rob_results <- list(
		assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study2", "Some concerns", "Low", "Low", "Low", "Low"),
		assess_rob2("Study3", "High", "Low", "Low", "Low", "Low")
	)

	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = rob_results,
		n_studies = 3L
	)

	expect_in(domains$limitations$level, c("some_concerns", "high"))
})

test_that("assess_evidence_domains handles NULL rob_results", {
	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 3L
	)

	expect_equal(domains$limitations$level, "low")
})

test_that("assess_evidence_domains handles inconsistency for single study", {
	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(I2 = NA_real_, tau2 = NA_real_),
		rob_results = NULL,
		n_studies = 1L
	)

	expect_equal(domains$inconsistency$level, "unknown")
})

test_that("assess_evidence_domains assesses inconsistency by I2", {
	domains_low <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(I2 = 10, tau2 = 0.001),
		rob_results = NULL,
		n_studies = 3L
	)
	expect_equal(domains_low$inconsistency$level, "low")

	domains_high <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(I2 = 80, tau2 = 0.05),
		rob_results = NULL,
		n_studies = 3L
	)
	expect_equal(domains_high$inconsistency$level, "high")
})

test_that("assess_evidence_domains assesses imprecision by CI inclusion", {
	domains_precise <- assess_evidence_domains(
		estimate = 0.72,
		ci = c(0.65, 0.80),
		p_value = 0.0001,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 3L,
		effect_measure = "hr"
	)
	expect_equal(domains_precise$imprecision$level, "low")

	domains_wide <- assess_evidence_domains(
		estimate = 0.85,
		ci = c(0.70, 1.02),
		p_value = 0.08,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 3L,
		effect_measure = "hr"
	)
	expect_equal(domains_wide$imprecision$level, "high")
})

test_that("assess_evidence_domains handles NA CI", {
	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(NA_real_, NA_real_),
		p_value = NA_real_,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 1L
	)

	expect_equal(domains$imprecision$level, "unknown")
})

test_that("assess_evidence_domains assesses indirectness by score", {
	domains_low <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 3L,
		indirectness = 0.95
	)
	expect_equal(domains_low$indirectness$level, "low")

	domains_high <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 3L,
		indirectness = 0.5
	)
	expect_equal(domains_high$indirectness$level, "high")
})

test_that("assess_evidence_domains defaults indirectness to low when NULL", {
	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 3L,
		indirectness = NULL
	)

	expect_equal(domains$indirectness$level, "low")
})

test_that("assess_evidence_domains assesses publication bias", {
	domains_none <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 5L,
		publication_bias = list(p_value = 0.15)
	)
	expect_equal(domains_none$publication_bias$level, "low")

	domains_strong <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 5L,
		publication_bias = list(p_value = 0.005)
	)
	expect_equal(domains_strong$publication_bias$level, "high")
})

test_that("assess_evidence_domains handles <5 studies for publication bias", {
	domains <- assess_evidence_domains(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		p_value = 0.008,
		heterogeneity = list(),
		rob_results = NULL,
		n_studies = 3L,
		publication_bias = list(p_value = 0.01)
	)

	expect_equal(domains$publication_bias$level, "unknown")
})

# =============================================================================
# format_evidence_grade() Tests - All Grades and Formats
# =============================================================================

test_that("format_evidence_grade works for all four grades in English", {
	grades <- c("proof", "indication", "hint", "none")

	for (g in grades) {
		grade <- EvidenceGrade(
			grade = g,
			grade_de = EVIDENCE_GRADES_DE[g],
			direction = "benefit",
			n_studies = 5L
		)

		formatted <- format_evidence_grade(grade, language = "en")

		expect_type(formatted, "character")
		expect_true(nchar(formatted) > 0)
	}
})

test_that("format_evidence_grade works for all four grades in German", {
	grades <- c("proof", "indication", "hint", "none")
	german_names <- c("Beleg", "Hinweis", "Anhaltspunkt", "Kein Beleg")

	for (i in seq_along(grades)) {
		grade <- EvidenceGrade(
			grade = grades[i],
			grade_de = german_names[i],
			direction = "benefit",
			n_studies = 5L
		)

		formatted <- format_evidence_grade(grade, language = "de")

		expect_type(formatted, "character")
		expect_equal(formatted, german_names[i])
	}
})

test_that("format_evidence_grade text format with details", {
	grade <- EvidenceGrade(
		grade = "indication",
		grade_de = "Hinweis",
		direction = "benefit",
		certainty = 0.75,
		n_studies = 3L,
		justification = "Evidence grade: Hinweis (INDICATION); Based on 3 studies"
	)

	formatted <- format_evidence_grade(
		grade,
		language = "en",
		include_details = TRUE,
		format = "text"
	)

	expect_type(formatted, "character")
	expect_true(grepl("Grade: Indication", formatted, fixed = TRUE))
	expect_true(grepl("Certainty: 75%", formatted, fixed = TRUE))
	expect_true(grepl("Studies: 3", formatted, fixed = TRUE))
})

test_that("format_evidence_grade HTML format", {
	grade <- EvidenceGrade(
		grade = "proof",
		grade_de = "Beleg",
		direction = "benefit",
		certainty = 0.92,
		n_studies = 5L
	)

	formatted <- format_evidence_grade(
		grade,
		language = "en",
		include_details = TRUE,
		format = "html"
	)

	expect_type(formatted, "character")
	expect_true(grepl("<div class='evidence-grade'>", formatted, fixed = TRUE))
	expect_true(grepl("<p class='grade'>", formatted, fixed = TRUE))
	# Verify proper HTML structure: each detail has its own <p class='grade'> tag
	expect_true(grepl("<p class='grade'>.*?</p>", formatted, perl = TRUE))
})

test_that("format_evidence_grade LaTeX format", {
	grade <- EvidenceGrade(
		grade = "hint",
		grade_de = "Anhaltspunkt",
		direction = "benefit",
		certainty = 0.45,
		n_studies = 2L
	)

	formatted <- format_evidence_grade(
		grade,
		language = "en",
		include_details = TRUE,
		format = "latex"
	)

	expect_type(formatted, "character")
	expect_true(grepl("\\\\begin\\{description\\}", formatted))
	expect_true(grepl("\\\\end\\{description\\}", formatted))
	expect_true(grepl("\\\\item\\[", formatted))
})

test_that("format_evidence_grade without details returns just grade name", {
	grade <- EvidenceGrade(
		grade = "indication",
		grade_de = "Hinweis",
		direction = "benefit",
		certainty = 0.7,
		n_studies = 4L
	)

	formatted_en <- format_evidence_grade(
		grade,
		language = "en",
		include_details = FALSE
	)
	formatted_de <- format_evidence_grade(
		grade,
		language = "de",
		include_details = FALSE
	)

	expect_equal(formatted_en, "Indication")
	expect_equal(formatted_de, "Hinweis")
})

# =============================================================================
# format_evidence_grade() Error Handling
# =============================================================================

test_that("format_evidence_grade rejects non-EvidenceGrade object", {
	expect_error(
		format_evidence_grade(list(grade = "proof")),
		"grade must be an EvidenceGrade object"
	)

	expect_error(
		format_evidence_grade(data.frame(grade = "proof")),
		"grade must be an EvidenceGrade object"
	)
})

test_that("format_evidence_grade rejects invalid language", {
	grade <- EvidenceGrade(grade = "proof", grade_de = "Beleg")

	expect_error(
		format_evidence_grade(grade, language = "fr"),
		"'arg' should be one of"
	)
})

test_that("format_evidence_grade rejects invalid format", {
	grade <- EvidenceGrade(grade = "proof", grade_de = "Beleg")

	expect_error(
		format_evidence_grade(grade, format = "json"),
		"'arg' should be one of"
	)
})

# =============================================================================
# print.EvidenceGrade() Tests
# =============================================================================

test_that("print.EvidenceGrade displays grade information", {
	grade <- EvidenceGrade(
		grade = "indication",
		grade_de = "Hinweis",
		direction = "benefit",
		certainty = 0.75,
		n_studies = 3L,
		justification = "Based on 3 RCTs with low RoB",
		domains = list(
			limitations = list(level = "low", rating = 0.9, notes = "Low RoB"),
			inconsistency = list(level = "low", rating = 0.9, notes = "Low I2"),
			imprecision = list(level = "low", rating = 0.9, notes = "Precise"),
			indirectness = list(level = "low", rating = 1.0, notes = "Direct"),
			publication_bias = list(
				level = "unknown",
				rating = NA,
				notes = "Not assessed"
			)
		)
	)

	output <- capture_output(print(grade))

	expect_true(grepl("IQWiG Evidence Grade", output, fixed = TRUE))
	expect_true(grepl("Grade: Hinweis \\(INDICATION\\)", output))
	expect_true(grepl("Certainty: 75%", output, fixed = TRUE))
	expect_true(grepl("Studies: 3", output, fixed = TRUE))
	expect_true(grepl("Direction: benefit", output, fixed = TRUE))
	expect_true(grepl("Justification:", output, fixed = TRUE))
	expect_true(grepl("Domain assessments:", output, fixed = TRUE))
})

test_that("print.EvidenceGrade returns object invisibly", {
	grade <- EvidenceGrade(grade = "proof", grade_de = "Beleg")

	expect_invisible(print(grade))
})

# =============================================================================
# evidence_summary_table() Tests
# =============================================================================

test_that("evidence_summary_table works with EvidenceGrade objects", {
	grades <- list(
		OS = EvidenceGrade(
			grade = "proof",
			grade_de = "Beleg",
			direction = "benefit",
			certainty = 0.9,
			n_studies = 5L
		),
		PFS = EvidenceGrade(
			grade = "indication",
			grade_de = "Hinweis",
			direction = "benefit",
			certainty = 0.7,
			n_studies = 4L
		)
	)

	table_df <- evidence_summary_table(grades)

	expect_s3_class(table_df, "data.frame")
	expect_equal(nrow(table_df), 2)
	expect_equal(ncol(table_df), 7)
	expect_equal(table_df$outcome, c("OS", "PFS"))
	expect_equal(table_df$grade, c("proof", "indication"))
	expect_equal(table_df$grade_en, c("proof", "indication"))
	expect_equal(table_df$grade_de, c("Beleg", "Hinweis"))
})

test_that("evidence_summary_table works with German output", {
	grades <- list(
		OS = EvidenceGrade(
			grade = "proof",
			grade_de = "Beleg",
			direction = "benefit",
			certainty = 0.9,
			n_studies = 5L
		)
	)

	table_df <- evidence_summary_table(grades, language = "de")

	expect_s3_class(table_df, "data.frame")
	expect_equal(table_df$grade_de, "Beleg")
})

test_that("evidence_summary_table works with data frame input", {
	df <- data.frame(
		outcome = c("OS", "PFS"),
		grade_en = c("proof", "indication"),
		grade_de = c("Beleg", "Hinweis"),
		certainty = c(0.9, 0.7),
		n_studies = c(5L, 4L),
		direction = c("benefit", "benefit")
	)

	result <- evidence_summary_table(df)

	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 2)
})

test_that("evidence_summary_table handles mixed list/data frame inputs", {
	grades <- list(
		list(
			outcome = "OS",
			grade_en = "proof",
			grade_de = "Beleg",
			certainty = 0.9,
			n_studies = 5L,
			direction = "benefit"
		),
		EvidenceGrade(
			grade = "indication",
			grade_de = "Hinweis",
			direction = "benefit",
			certainty = 0.7,
			n_studies = 4L
		)
	)

	table_df <- evidence_summary_table(
		grades,
		outcomes = c("OS", "PFS"),
		language = "en"
	)

	expect_s3_class(table_df, "data.frame")
	expect_equal(nrow(table_df), 2)
})

test_that("evidence_summary_table requires names or outcomes", {
	grades <- list(
		EvidenceGrade(grade = "proof", grade_de = "Beleg"),
		EvidenceGrade(grade = "indication", grade_de = "Hinweis")
	)

	expect_error(
		evidence_summary_table(grades),
		"Either name the grades list or provide outcomes vector"
	)
})

test_that("evidence_summary_table rejects non-list/non-data.frame input", {
	expect_error(
		evidence_summary_table("not a list or data frame"),
		"grades must be a list of EvidenceGrade objects or a data frame"
	)

	expect_error(
		evidence_summary_table(123),
		"grades must be a list of EvidenceGrade objects or a data frame"
	)
})

# =============================================================================
# All Four Evidence Grades Integration Tests
# =============================================================================

test_that("All four evidence grades can be produced by grade_evidence", {
	meta_res_proof <- MetaResult(
		estimate = 0.68,
		ci = c(0.58, 0.80),
		p_value = 0.00001,
		n = 6L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 5, tau2 = 0.001)
	)

	meta_res_indication <- MetaResult(
		estimate = 0.78,
		ci = c(0.62, 0.98),
		p_value = 0.03,
		n = 4L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 35, tau2 = 0.02)
	)

	meta_res_hint <- MetaResult(
		estimate = 0.85,
		ci = c(0.65, 1.11),
		p_value = 0.15,
		n = 2L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 60, tau2 = 0.04)
	)

	meta_res_none <- MetaResult(
		estimate = 1.05,
		ci = c(0.80, 1.38),
		p_value = 0.70,
		n = 2L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 0, tau2 = 0)
	)

	rob_results_low <- list(
		assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study5", "Low", "Low", "Low", "Low", "Low")
	)

	rob_results_high <- list(
		assess_rob2("Study1", "High", "High", "High", "High", "High"),
		assess_rob2("Study2", "High", "High", "High", "High", "High")
	)

	grade_proof <- grade_evidence(
		meta_res_proof,
		rob_results = rob_results_low,
		direction = "benefit"
	)

	grade_indication <- grade_evidence(
		meta_res_indication,
		rob_results = rob_results_low[1:4],
		direction = "benefit"
	)

	grade_hint <- grade_evidence(
		meta_res_hint,
		rob_results = rob_results_high,
		direction = "benefit"
	)

	grade_none <- grade_evidence(
		meta_res_none,
		rob_results = rob_results_high,
		direction = "none"
	)

	expect_equal(grade_proof@grade, "proof")
	expect_equal(grade_indication@grade, "indication")
	expect_equal(grade_hint@grade, "hint")
	expect_equal(grade_none@grade, "none")

	expect_equal(grade_proof@grade_de, "Beleg")
	expect_equal(grade_indication@grade_de, "Hinweis")
	expect_equal(grade_hint@grade_de, "Anhaltspunkt")
	expect_equal(grade_none@grade_de, "Kein Beleg")
})
