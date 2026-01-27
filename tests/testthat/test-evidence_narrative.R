# Tests for Evidence Narrative Functions
# R/evidence_narrative.R

# =============================================================================
# Helper fixtures for narrative tests
# =============================================================================

.fixture_meta_result <- function() {
	MetaResult(
		estimate = 0.75,
		ci = c(0.62, 0.90),
		p_value = 0.002,
		n = 5L,
		effect_measure = "hr",
		heterogeneity = list(I2 = 25, tau2 = 0.01),
		method = "REML meta-analysis"
	)
}

.fixture_comparison_result <- function() {
	ComparisonResult(
		estimate = 0.78,
		ci = c(0.65, 0.93),
		p_value = 0.008,
		effect_measure = "hr",
		n = 245L
	)
}

.fixture_evidence_grade <- function() {
	EvidenceGrade(
		grade = "indication",
		grade_de = "Hinweis",
		direction = "benefit",
		n_studies = 5L
	)
}

.fixture_rob_results <- function() {
	list(
		assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study4", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study5", "Low", "Low", "Low", "Low", "Low")
	)
}

# =============================================================================
# generate_evidence_narrative() with MetaResult
# =============================================================================

describe("evidence narrative", {
	it("generate_evidence_narrative works with MetaResult (English)", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical",
			language = "en"
		)

		expect_type(narrative, "character")
		expect_true(nchar(narrative) > 0)
		expect_true(grepl("Overall Survival", narrative, fixed = TRUE))
		expect_true(grepl("5 studies", narrative, fixed = TRUE))
		expect_true(grepl("1,245", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative works with MetaResult (German)", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Gesamtüberleben",
			result = result,
			n_patients = 1245,
			template = "iqwig",
			language = "de"
		)

		expect_type(narrative, "character")
		expect_true(nchar(narrative) > 0)
		expect_true(grepl("Gesamtüberleben", narrative, fixed = TRUE))
		expect_true(grepl("5 Studien", narrative, fixed = TRUE))
		expect_true(grepl("1.245", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative includes evidence grade when provided", {
		result <- .fixture_meta_result()
		grade <- .fixture_evidence_grade()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			grade = grade,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl("benefit", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative includes heterogeneity info", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl("heterogeneity", narrative, ignore.case = TRUE))
	})

	it("generate_evidence_narrative includes risk of bias summary", {
		result <- .fixture_meta_result()
		rob_results <- .fixture_rob_results()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			rob_results = rob_results,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl("risk of bias", narrative, ignore.case = TRUE))
	})

	it("generate_evidence_narrative handles significant effect correctly", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl("significant", narrative, ignore.case = TRUE))
	})

	it("generate_evidence_narrative handles non-significant effect", {
		result <- MetaResult(
			estimate = 0.95,
			ci = c(0.75, 1.20),
			p_value = 0.67,
			n = 5L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 20, tau2 = 0.01)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl(
			"not demonstrate a statistically significant effect",
			narrative,
			fixed = TRUE
		))
	})

	it("generate_evidence_narrative formats CI correctly", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "plain"
		)

		expect_true(grepl("[", narrative, fixed = TRUE))
		expect_true(grepl("]", narrative, fixed = TRUE))
		expect_true(grepl("0.62", narrative, fixed = TRUE))
		expect_true(grepl("0.90", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative formats p-value correctly", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "plain"
		)

		expect_true(grepl("p=", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative formats very small p-values", {
		result <- MetaResult(
			estimate = 0.70,
			ci = c(0.62, 0.79),
			p_value = 0.00001,
			n = 5L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 10, tau2 = 0.005)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl("<", narrative, fixed = TRUE))
	})

	# =============================================================================
	# generate_evidence_narrative() with ComparisonResult
	# =============================================================================

	it("generate_evidence_narrative works with ComparisonResult", {
		result <- .fixture_comparison_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Progression-Free Survival",
			result = result,
			n_patients = 245,
			template = "clinical"
		)

		expect_type(narrative, "character")
		expect_true(nchar(narrative) > 0)
		expect_true(grepl("245 patients", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative uses result@n when not specified", {
		result <- .fixture_comparison_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Progression-Free Survival",
			result = result,
			template = "clinical"
		)

		expect_true(grepl("245 patients", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative uses single_study template", {
		result <- .fixture_comparison_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Progression-Free Survival",
			result = result,
			n_patients = 245,
			template = "clinical"
		)

		expect_true(
			grepl("study with", narrative, fixed = TRUE) ||
				grepl("245 patients", narrative, fixed = TRUE)
		)
	})

	it("generate_evidence_narrative with ComparisonResult includes grade", {
		result <- .fixture_comparison_result()
		grade <- .fixture_evidence_grade()
		narrative <- generate_evidence_narrative(
			endpoint = "Progression-Free Survival",
			result = result,
			grade = grade,
			n_patients = 245,
			template = "clinical"
		)

		expect_true(
			grepl("indication", narrative, fixed = TRUE) ||
				grepl("Indication", narrative, fixed = TRUE)
		)
	})

	it("generate_evidence_narrative with ComparisonResult includes RoB", {
		result <- .fixture_comparison_result()
		rob_result <- assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low")
		narrative <- generate_evidence_narrative(
			endpoint = "Progression-Free Survival",
			result = result,
			rob_results = rob_result,
			n_patients = 245,
			template = "clinical"
		)

		expect_true(grepl("risk of bias", narrative, ignore.case = TRUE))
	})

	# =============================================================================
	# Template Tests - All Templates
	# =============================================================================

	it("generate_evidence_narrative works with iqwig template", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Gesamtüberleben",
			result = result,
			n_patients = 1245,
			template = "iqwig",
			language = "de"
		)

		expect_true(grepl("Metaanalyse", narrative, fixed = TRUE))
		expect_true(grepl("statistisch signifikant", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative works with clinical template", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical",
			language = "en"
		)

		expect_true(grepl("meta-analysis", narrative, ignore.case = TRUE))
		expect_true(grepl("statistically significant", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative works with plain template", {
		result <- .fixture_meta_result()
		narrative <- generate_evidence_narrative(
			endpoint = "OS",
			result = result,
			n_patients = 1245,
			template = "plain",
			language = "en"
		)

		expect_true(grepl("OS:", narrative, fixed = TRUE))
		expect_true(grepl("studies", narrative, fixed = TRUE))
		expect_true(grepl("N=", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative templates format heterogeneity", {
		result_low_i2 <- MetaResult(
			estimate = 0.75,
			ci = c(0.62, 0.90),
			p_value = 0.002,
			n = 5L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 10, tau2 = 0.003)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result_low_i2,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl("low", narrative, ignore.case = TRUE))
	})

	it("generate_evidence_narrative templates format evidence grades", {
		result <- .fixture_meta_result()
		grade_proof <- EvidenceGrade(
			grade = "proof",
			grade_de = "Beleg",
			direction = "benefit",
			n_studies = 5L
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			grade = grade_proof,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(grepl("proof", narrative, fixed = TRUE))
	})

	# =============================================================================
	# German and English Output Tests
	# =============================================================================

	it("generate_evidence_narrative produces German output", {
		result <- .fixture_meta_result()
		narrative_de <- generate_evidence_narrative(
			endpoint = "Gesamtüberleben",
			result = result,
			n_patients = 1245,
			template = "iqwig",
			language = "de"
		)

		expect_true(grepl("Studien", narrative_de, fixed = TRUE))
		expect_true(
			grepl("Patienten", narrative_de, fixed = TRUE) ||
				grepl("N=1\\.245", narrative_de)
		)
		expect_true(grepl("signifikant", narrative_de, fixed = TRUE))
	})

	it("generate_evidence_narrative produces English output", {
		result <- .fixture_meta_result()
		narrative_en <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical",
			language = "en"
		)

		expect_true(grepl("studies", narrative_en, ignore.case = TRUE))
		expect_true(
			grepl("patients", narrative_en, ignore.case = TRUE) ||
				grepl("N=1,245", narrative_en, fixed = TRUE)
		)
		expect_true(grepl("significant", narrative_en, ignore.case = TRUE))
	})

	it("generate_evidence_narrative German/English differ", {
		result <- .fixture_meta_result()
		narrative_de <- generate_evidence_narrative(
			endpoint = "OS",
			result = result,
			n_patients = 1245,
			template = "iqwig",
			language = "de"
		)
		narrative_en <- generate_evidence_narrative(
			endpoint = "OS",
			result = result,
			n_patients = 1245,
			template = "clinical",
			language = "en"
		)

		expect_false(narrative_de == narrative_en)
	})

	# =============================================================================
	# generate_endpoint_narrative() Tests
	# =============================================================================

	it("generate_endpoint_narrative generates valid paragraph", {
		result <- .fixture_meta_result()
		narrative <- generate_endpoint_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical"
		)

		expect_type(narrative, "character")
		expect_true(nchar(narrative) > 0)
		expect_true(grepl("\\.$", narrative))
	})

	it("generate_endpoint_narrative passes all parameters correctly", {
		result <- .fixture_meta_result()
		grade <- .fixture_evidence_grade()
		rob_results <- .fixture_rob_results()

		narrative <- generate_endpoint_narrative(
			endpoint = "Overall Survival",
			result = result,
			evidence_grade = grade,
			rob_results = rob_results,
			n_patients = 1245,
			template = "clinical",
			language = "en",
			ci_level = 0.95
		)

		expect_true(grepl("Overall Survival", narrative, fixed = TRUE))
		expect_true(grepl("5 studies", narrative, fixed = TRUE))
		expect_true(grepl("benefit", narrative, fixed = TRUE))
		expect_true(grepl("risk of bias", narrative, ignore.case = TRUE))
	})

	it("generate_endpoint_narrative adds period if missing", {
		result <- .fixture_meta_result()
		grade <- .fixture_evidence_grade()

		narrative <- generate_endpoint_narrative(
			endpoint = "OS",
			result = result,
			evidence_grade = grade,
			n_patients = 1245,
			template = "plain"
		)

		expect_true(grepl("\\.$", narrative))
	})

	# =============================================================================
	# generate_full_evidence_report() Tests
	# =============================================================================

	it("generate_full_evidence_report returns rdocx object", {
		result_os <- .fixture_meta_result()
		result_pfs <- MetaResult(
			estimate = 0.82,
			ci = c(0.70, 0.96),
			p_value = 0.015,
			n = 4L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 30, tau2 = 0.012)
		)

		endpoints <- list(
			list(
				endpoint = "Overall Survival",
				result = result_os,
				evidence_grade = .fixture_evidence_grade(),
				n_patients = 1245
			),
			list(
				endpoint = "Progression-Free Survival",
				result = result_pfs,
				n_patients = 1180
			)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Clinical Study Report",
			template = "clinical",
			language = "en"
		)

		expect_s3_class(doc, "rdocx")
	})

	it("generate_full_evidence_report includes title and sections", {
		result <- .fixture_meta_result()
		endpoints <- list(
			list(
				endpoint = "Overall Survival",
				result = result,
				n_patients = 1245
			)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Evidence Report",
			template = "clinical",
			language = "en"
		)

		expect_s3_class(doc, "rdocx")
	})

	it("generate_full_evidence_report handles subtitle and author", {
		result <- .fixture_meta_result()
		endpoints <- list(
			list(
				endpoint = "Overall Survival",
				result = result,
				n_patients = 1245
			)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Evidence Report",
			subtitle = "Study ABC-123",
			author = "Dr. Smith",
			template = "clinical"
		)

		expect_s3_class(doc, "rdocx")
	})

	it("generate_full_evidence_report generates German report", {
		result <- .fixture_meta_result()
		endpoints <- list(
			list(
				endpoint = "Gesamtüberleben",
				result = result,
				n_patients = 1245
			)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Nutzenbewertung",
			template = "iqwig",
			language = "de"
		)

		expect_s3_class(doc, "rdocx")
	})

	it("generate_full_evidence_report calculates overall statistics", {
		result1 <- .fixture_meta_result()
		result2 <- MetaResult(
			estimate = 0.82,
			ci = c(0.70, 0.96),
			p_value = 0.015,
			n = 4L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 30, tau2 = 0.012)
		)

		endpoints <- list(
			list(endpoint = "OS", result = result1, n_patients = 1245),
			list(endpoint = "PFS", result = result2, n_patients = 1180)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Evidence Report",
			template = "clinical"
		)

		expect_s3_class(doc, "rdocx")
	})

	it("generate_full_evidence_report handles multiple endpoints", {
		result <- .fixture_meta_result()
		endpoints <- list(
			list(endpoint = "OS", result = result, n_patients = 1245),
			list(endpoint = "PFS", result = result, n_patients = 1180),
			list(endpoint = "ORR", result = result, n_patients = 1200)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Evidence Report",
			template = "plain"
		)

		expect_s3_class(doc, "rdocx")
	})

	# =============================================================================
	# narrative_template() Tests
	# =============================================================================

	it("narrative_template lists available templates", {
		templates <- narrative_template(list_templates = TRUE)

		expect_type(templates, "character")
		expect_setequal(templates, c("iqwig", "clinical", "plain"))
	})

	it("narrative_template retrieves predefined template component", {
		template <- narrative_template("iqwig", "effect_significant")

		expect_type(template, "character")
		expect_true(nchar(template) > 0)
	})

	it("narrative_template retrieves all components", {
		components <- c(
			"effect_significant",
			"effect_nonsignificant",
			"single_study",
			"heterogeneity_low",
			"heterogeneity_moderate",
			"heterogeneity_substantial",
			"heterogeneity_considerable",
			"heterogeneity_single",
			"rob_low",
			"rob_moderate",
			"rob_high",
			"rob_unknown",
			"grade_proof",
			"grade_indication",
			"grade_hint",
			"grade_none",
			"effect_direction_benefit",
			"effect_direction_harm",
			"effect_direction_neutral"
		)

		for (comp in components) {
			template <- narrative_template("iqwig", comp)
			expect_type(template, "character")
			expect_true(nchar(template) > 0)
		}
	})

	it("narrative_template returns custom template string", {
		custom_template <- "For {endpoint}, the effect is {estimate}"
		result <- narrative_template(custom_template)

		expect_equal(result, custom_template)
	})

	it("narrative_template errors on custom template without placeholders", {
		expect_error(
			narrative_template("No placeholders here"),
			"Template 'No placeholders here' not found"
		)
	})

	it("narrative_template works with all template names", {
		template_names <- narrative_template(list_templates = TRUE)

		for (name in template_names) {
			template <- narrative_template(name, "effect_significant")
			expect_type(template, "character")
		}
	})

	it("narrative_template works with language parameter", {
		template_en <- narrative_template(
			"iqwig",
			"effect_significant",
			language = "en"
		)
		template_de <- narrative_template(
			"iqwig",
			"effect_significant",
			language = "de"
		)

		expect_type(template_en, "character")
		expect_type(template_de, "character")
	})

	# =============================================================================
	# Error Handling Tests
	# =============================================================================

	it("generate_evidence_narrative rejects invalid result type", {
		expect_error(
			generate_evidence_narrative(
				endpoint = "OS",
				result = list(estimate = 0.75),
				n_patients = 1245
			),
			"must be a MetaResult or ComparisonResult object"
		)
	})

	it("generate_evidence_narrative uses result@n for ComparisonResult", {
		result <- .fixture_comparison_result()
		narrative <- generate_evidence_narrative(
			endpoint = "Progression-Free Survival",
			result = result,
			template = "clinical"
		)

		expect_true(grepl("245 patients", narrative, fixed = TRUE))
	})

	it("generate_evidence_narrative rejects invalid template", {
		result <- .fixture_meta_result()
		expect_error(
			generate_evidence_narrative(
				endpoint = "OS",
				result = result,
				n_patients = 1245,
				template = "invalid"
			),
			"Template 'invalid' not found"
		)
	})

	it("generate_evidence_narrative rejects invalid language", {
		result <- .fixture_meta_result()
		expect_error(
			generate_evidence_narrative(
				endpoint = "OS",
				result = result,
				n_patients = 1245,
				language = "fr"
			),
			"'arg' should be one of"
		)
	})

	it("generate_endpoint_narrative rejects invalid result type", {
		expect_error(
			generate_endpoint_narrative(
				endpoint = "OS",
				result = list(estimate = 0.75),
				n_patients = 1245
			),
			"must be a MetaResult or ComparisonResult object"
		)
	})

	it("generate_full_evidence_report rejects empty endpoints list", {
		expect_error(
			generate_full_evidence_report(
				endpoints = list(),
				title = "Report"
			),
			"must be a non-empty list"
		)
	})

	it("generate_full_evidence_report rejects non-list endpoints", {
		expect_error(
			generate_full_evidence_report(
				endpoints = "not a list",
				title = "Report"
			),
			"must be a non-empty list"
		)
	})

	it("generate_full_evidence_report rejects missing required fields", {
		result <- .fixture_meta_result()
		endpoints <- list(
			list(endpoint = "OS", result = result)
		)

		expect_error(
			generate_full_evidence_report(
				endpoints = endpoints,
				title = "Report"
			),
			"missing required fields"
		)
	})

	it("narrative_template requires component for predefined templates", {
		expect_error(
			narrative_template("iqwig"),
			"component must be specified"
		)
	})

	it("narrative_template rejects invalid template name", {
		expect_error(
			narrative_template("invalid", "effect_significant"),
			"Template 'invalid' not found"
		)
	})

	it("narrative_template rejects invalid component", {
		expect_error(
			narrative_template("iqwig", "invalid_component"),
			"Template component 'invalid_component' not found"
		)
	})

	it("narrative_template rejects invalid language", {
		expect_error(
			narrative_template("iqwig", "effect_significant", language = "fr"),
			"'arg' should be one of"
		)
	})

	# =============================================================================
	# Edge Cases and Special Scenarios
	# =============================================================================

	it("generate_evidence_narrative handles NA p_value", {
		result <- MetaResult(
			estimate = 0.75,
			ci = c(0.62, 0.90),
			p_value = NA_real_,
			n = 5L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 25, tau2 = 0.01)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "plain"
		)

		expect_true(
			grepl("NA", narrative, fixed = TRUE) ||
				grepl("N/A", narrative, fixed = TRUE)
		)
	})

	it("generate_evidence_narrative handles NA CI", {
		result <- MetaResult(
			estimate = 0.75,
			ci = c(NA_real_, NA_real_),
			p_value = 0.05,
			n = 5L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 25, tau2 = 0.01)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "plain"
		)

		expect_true(
			grepl("NA", narrative, fixed = TRUE) ||
				grepl("N/A", narrative, fixed = TRUE)
		)
	})

	it("generate_evidence_narrative handles NA heterogeneity", {
		result <- MetaResult(
			estimate = 0.75,
			ci = c(0.62, 0.90),
			p_value = 0.002,
			n = 5L,
			effect_measure = "hr",
			heterogeneity = list(I2 = NA_real_, tau2 = NA_real_)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical"
		)

		expect_type(narrative, "character")
	})

	it("generate_evidence_narrative handles single study in meta-analysis", {
		result <- MetaResult(
			estimate = 0.78,
			ci = c(0.65, 0.93),
			p_value = 0.008,
			n = 1L,
			effect_measure = "hr",
			heterogeneity = list(I2 = NA_real_, tau2 = NA_real_)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 245,
			template = "clinical"
		)

		expect_true(nchar(narrative) > 0)
	})

	it("generate_evidence_narrative infers direction from estimate", {
		result <- MetaResult(
			estimate = 0.75,
			ci = c(0.62, 0.90),
			p_value = 0.002,
			n = 5L,
			effect_measure = "hr",
			heterogeneity = list(I2 = 25, tau2 = 0.01)
		)

		narrative <- generate_evidence_narrative(
			endpoint = "Overall Survival",
			result = result,
			n_patients = 1245,
			template = "clinical"
		)

		expect_true(nchar(narrative) > 0)
	})

	it("generate_evidence_narrative handles different effect measures", {
		effect_measures <- c("hr", "or", "rr", "rd", "md", "smd")

		for (em in effect_measures) {
			result <- MetaResult(
				estimate = 0.75,
				ci = c(0.62, 0.90),
				p_value = 0.002,
				n = 5L,
				effect_measure = em,
				heterogeneity = list(I2 = 25, tau2 = 0.01)
			)

			narrative <- generate_evidence_narrative(
				endpoint = "Endpoint",
				result = result,
				n_patients = 1245,
				template = "plain"
			)

			expect_type(narrative, "character")
			expect_true(nchar(narrative) > 0)
		}
	})

	it("generate_full_evidence_report handles endpoints missing grade", {
		result <- .fixture_meta_result()
		endpoints <- list(
			list(
				endpoint = "Overall Survival",
				result = result,
				n_patients = 1245
			),
			list(
				endpoint = "PFS",
				result = result,
				n_patients = 1180
			)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Evidence Report",
			template = "clinical"
		)

		expect_s3_class(doc, "rdocx")
	})

	it("generate_full_evidence_report handles endpoints missing RoB", {
		result <- .fixture_meta_result()
		endpoints <- list(
			list(
				endpoint = "Overall Survival",
				result = result,
				n_patients = 1245
			)
		)

		doc <- generate_full_evidence_report(
			endpoints = endpoints,
			title = "Evidence Report",
			template = "clinical"
		)

		expect_s3_class(doc, "rdocx")
	})

	# =============================================================================
	# Regression Tests for Fixed Bugs
	# =============================================================================

	it(
		paste0(
			"uses MetaResult for multi-study analyses in batch ",
			"(issue: n_studies)"
		),
		{
			data <- data.frame(
				endpoint = c("OS", "PFS"),
				estimate = c(0.75, 0.80),
				ci_lower = c(0.60, 0.65),
				ci_upper = c(0.93, 0.98),
				p_value = c(0.008, 0.04),
				effect_measure = c("hr", "hr"),
				n_studies = c(5L, 3L), # Multi-study - use integer literals
				n_patients = c(1000, 800),
				stringsAsFactors = FALSE
			)

			narratives <- generate_batch_narratives(data)

			# Should mention the number of studies (German output:
			# "5 Studien", "3 Studien")
			expect_true(any(grepl(
				"5 Studien|3 Studien|5 studies|3 studies",
				narratives,
				ignore.case = TRUE
			)))
		}
	)
})
