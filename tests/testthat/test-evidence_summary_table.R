# Tests for Evidence Summary Tables
# R/evidence_summary_table.R

# =============================================================================
# create_evidence_summary_table() Tests
# =============================================================================

test_that("create_evidence_summary_table works with basic input", {
	endpoints <- list(
		"Overall Survival" = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				direction = "benefit",
				n_studies = 3L
			)
		),
		"Progression-Free Survival" = list(
			result = MetaResult(
				estimate = 0.68,
				ci = c(0.52, 0.89),
				p_value = 0.005,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 45.0, tau2 = 0.03)
			),
			grade = EvidenceGrade(
				grade = "proof",
				grade_de = "Beleg",
				direction = "benefit",
				n_studies = 3L
			)
		)
	)

	table <- create_evidence_summary_table(endpoints)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@type, "evidence_summary")
	expect_s3_class(table@data, "data.frame")
	expect_equal(nrow(table@data), 2)
	expect_true("Endpoint" %in% names(table@data))
	expect_true("Effect (95% CI)" %in% names(table@data))
	expect_true("Grade" %in% names(table@data))
})

test_that("create_evidence_summary_table formats effect estimate correctly", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	table <- create_evidence_summary_table(endpoints)

	expect_s7_class(table, ClinicalTable)
	expect_true(grepl("HR", table@data$`Effect (95% CI)`[1], fixed = TRUE))
	expect_true(grepl("0.75", table@data$`Effect (95% CI)`[1]))
})

test_that("create_evidence_summary_table formats I2 heterogeneity", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	table <- create_evidence_summary_table(endpoints)

	expect_equal(table@data$I2[1], "25%")
})

test_that("create_evidence_summary_table handles missing I2", {
	endpoints <- list(
		OS = list(
			result = ComparisonResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				effect_measure = "hr"
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 1L
			)
		)
	)

	table <- create_evidence_summary_table(endpoints)

	expect_equal(table@data$I2[1], "--")
})

test_that("create_evidence_summary_table formats p-values correctly", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	table <- create_evidence_summary_table(endpoints)

	expect_true(grepl("0\\.012", table@data$`p-value`[1]))
})

# =============================================================================
# create_evidence_summary_table() Language Support Tests
# =============================================================================

test_that("create_evidence_summary_table works with English output", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			),
			rob = assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low")
		)
	)

	table <- create_evidence_summary_table(endpoints, language = "en")

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@metadata$language, "en")
	expect_equal(table@data$Grade[1], "Indication")
	expect_equal(table@data$RoB[1], "Low")
})

test_that("create_evidence_summary_table works with German output", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			),
			rob = assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low")
		)
	)

	table <- create_evidence_summary_table(endpoints, language = "de")

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@metadata$language, "de")
	expect_equal(table@data$Grade[1], "Hinweis")
	expect_equal(table@data$RoB[1], "Niedrig")
})

test_that("create_evidence_summary_table uses German decimal separator", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	table <- create_evidence_summary_table(endpoints, language = "de")

	expect_s7_class(table, ClinicalTable)
	expect_true(grepl(",", table@data$`Effect (95% CI)`[1], fixed = TRUE))
})

# =============================================================================
# create_evidence_summary_table() Column Selection Tests
# =============================================================================

test_that("create_evidence_summary_table respects column selection", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	table <- create_evidence_summary_table(
		endpoints,
		columns = c("Endpoint", "Effect (95% CI)", "Grade")
	)

	expect_s7_class(table, ClinicalTable)
	expect_equal(names(table@data), c("Endpoint", "Effect (95% CI)", "Grade"))
})

test_that("create_evidence_summary_table warns on missing columns", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr",
				heterogeneity = list(I2 = 25.0, tau2 = 0.01)
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	expect_warning(
		table <- create_evidence_summary_table(
			endpoints,
			columns = c("Endpoint", "NonExistent", "Grade")
		),
		"Requested columns not available"
	)
})

# =============================================================================
# create_evidence_summary_table() Error Handling Tests
# =============================================================================

test_that("create_evidence_summary_table rejects empty list", {
	expect_error(
		create_evidence_summary_table(list()),
		"'data' must be a non-empty list"
	)
})

test_that("create_evidence_summary_table rejects non-list input", {
	expect_error(
		create_evidence_summary_table("not a list"),
		"'data' must be a non-empty list"
	)

	expect_error(
		create_evidence_summary_table(123),
		"'data' must be a non-empty list"
	)
})

test_that("create_evidence_summary_table rejects missing result", {
	endpoints <- list(
		OS = list(
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	expect_error(
		create_evidence_summary_table(endpoints),
		"Endpoint 'OS' is missing 'result' component"
	)
})

# =============================================================================
# create_study_characteristics_table() Tests
# =============================================================================

test_that("create_study_characteristics_table works with TwoArmStudy objects", {
	studies <- list(
		TwoArmStudy(
			study_id = "STUDY001",
			study_title = "Phase III Trial of Drug X vs Placebo",
			design = "rct",
			population = "ITT",
			treatment_var = "Drug X",
			comparator = "Placebo",
			metadata = list(n = 250)
		),
		TwoArmStudy(
			study_id = "STUDY002",
			study_title = "Phase III Trial of Drug X vs Standard",
			design = "rct",
			population = "FAS",
			treatment_var = "Drug X",
			comparator = "Standard of Care",
			metadata = list(n = 300)
		)
	)

	table <- create_study_characteristics_table(studies)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@type, "study_characteristics")
	expect_s3_class(table@data, "data.frame")
	expect_equal(nrow(table@data), 2)
	expect_true("Study" %in% names(table@data))
	expect_true("Design" %in% names(table@data))
	expect_true("N" %in% names(table@data))
	expect_equal(table@data$Study[1], "STUDY001")
	expect_equal(table@data$Design[1], "Randomized Controlled Trial")
	expect_equal(table@data$N[1], "250")
})

test_that("create_study_characteristics_table works with data frame input", {
	studies <- list(
		data.frame(
			study_id = "STUDY001",
			design = "rct",
			n = 250,
			treatment = "Drug X",
			comparator = "Placebo",
			population = "ITT"
		),
		data.frame(
			study_id = "STUDY002",
			design = "rct",
			n = 300,
			treatment = "Drug X",
			comparator = "Standard",
			population = "FAS"
		)
	)

	table <- create_study_characteristics_table(studies)

	expect_s7_class(table, ClinicalTable)
	expect_equal(nrow(table@data), 2)
	expect_equal(table@data$Study[1], "STUDY001")
})

test_that("create_study_characteristics_table works with list input", {
	studies <- list(
		list(
			study_id = "STUDY001",
			design = "rct",
			n = 250,
			treatment = "Drug X",
			comparator = "Placebo",
			population = "ITT"
		),
		list(
			study_id = "STUDY002",
			design = "observational",
			n = 300,
			treatment = "Drug X",
			comparator = "Standard",
			population = "FAS"
		)
	)

	table <- create_study_characteristics_table(studies)

	expect_s7_class(table, ClinicalTable)
	expect_equal(nrow(table@data), 2)
	expect_equal(table@data$Design[1], "Randomized Controlled Trial")
	expect_equal(table@data$Design[2], "Observational Study")
})

test_that("create_study_characteristics_table formats design labels", {
	studies <- list(
		list(
			study_id = "S1",
			design = "rct",
			n = 100,
			treatment = "T",
			comparator = "C",
			population = "ITT"
		),
		list(
			study_id = "S2",
			design = "observational",
			n = 100,
			treatment = "T",
			comparator = "C",
			population = "ITT"
		),
		list(
			study_id = "S3",
			design = "single-arm",
			n = 100,
			treatment = "T",
			comparator = "C",
			population = "ITT"
		),
		list(
			study_id = "S4",
			design = "crossover",
			n = 100,
			treatment = "T",
			comparator = "C",
			population = "ITT"
		)
	)

	table <- create_study_characteristics_table(studies)

	expect_equal(table@data$Design[1], "Randomized Controlled Trial")
	expect_equal(table@data$Design[2], "Observational Study")
	expect_equal(table@data$Design[3], "Single-Arm Study")
	expect_equal(table@data$Design[4], "Crossover Trial")
})

test_that("create_study_characteristics_table handles SingleArmStudy", {
	studies <- list(
		SingleArmStudy(
			study_id = "STUDY001",
			study_title = "Single Arm Trial",
			design = "single-arm",
			population = "ITT",
			treatment_var = "Drug X"
		)
	)

	table <- create_study_characteristics_table(studies)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@data$Comparator[1], "Single arm")
})

# =============================================================================
# create_study_characteristics_table() Error Handling Tests
# =============================================================================

test_that("create_study_characteristics_table rejects invalid study type", {
	studies <- list("not a study object")

	expect_error(
		create_study_characteristics_table(studies),
		"Each element of 'data' must be a Study object"
	)
})

test_that("create_study_characteristics_table warns on missing columns", {
	studies <- list(
		list(study_id = "S1", design = "rct", n = 100)
	)

	expect_warning(
		table <- create_study_characteristics_table(
			studies,
			columns = c("Study", "NonExistent", "Design")
		),
		"Requested columns not available"
	)
})

# =============================================================================
# export_evidence_table() Tests
# =============================================================================

test_that("export_evidence_table works with Word format", {
	withr::with_tempdir({
		endpoints <- list(
			OS = list(
				result = MetaResult(
					estimate = 0.75,
					ci = c(0.60, 0.94),
					p_value = 0.012,
					n = 3L,
					effect_measure = "hr",
					heterogeneity = list(I2 = 25.0, tau2 = 0.01)
				),
				grade = EvidenceGrade(
					grade = "indication",
					grade_de = "Hinweis",
					n_studies = 3L
				)
			)
		)

		table <- create_evidence_summary_table(endpoints)

		result <- export_evidence_table(table, "evidence.docx")

		expect_null(result)
		expect_true(file.exists("evidence.docx"))
	})
})

test_that("export_evidence_table works with HTML format", {
	withr::with_tempdir({
		endpoints <- list(
			OS = list(
				result = MetaResult(
					estimate = 0.75,
					ci = c(0.60, 0.94),
					p_value = 0.012,
					n = 3L,
					effect_measure = "hr",
					heterogeneity = list(I2 = 25.0, tau2 = 0.01)
				),
				grade = EvidenceGrade(
					grade = "indication",
					grade_de = "Hinweis",
					n_studies = 3L
				)
			)
		)

		table <- create_evidence_summary_table(endpoints)

		result <- export_evidence_table(table, "evidence.html")

		expect_null(result)
		expect_true(file.exists("evidence.html"))

		html_content <- readLines("evidence.html")
		expect_true(any(grepl("<table", html_content, fixed = TRUE)))
	})
})

test_that("export_evidence_table works with Excel format", {
	skip_if_not_installed("writexl")

	withr::with_tempdir({
		endpoints <- list(
			OS = list(
				result = MetaResult(
					estimate = 0.75,
					ci = c(0.60, 0.94),
					p_value = 0.012,
					n = 3L,
					effect_measure = "hr",
					heterogeneity = list(I2 = 25.0, tau2 = 0.01)
				),
				grade = EvidenceGrade(
					grade = "indication",
					grade_de = "Hinweis",
					n_studies = 3L
				)
			)
		)

		table <- create_evidence_summary_table(endpoints)

		result <- export_evidence_table(table, "evidence.xlsx")

		expect_null(result)
		expect_true(file.exists("evidence.xlsx"))
	})
})

test_that("export_evidence_table rejects unsupported format", {
	endpoints <- list(
		OS = list(
			result = MetaResult(
				estimate = 0.75,
				ci = c(0.60, 0.94),
				p_value = 0.012,
				n = 3L,
				effect_measure = "hr"
			),
			grade = EvidenceGrade(
				grade = "indication",
				grade_de = "Hinweis",
				n_studies = 3L
			)
		)
	)

	table <- create_evidence_summary_table(endpoints)

	expect_error(
		export_evidence_table(table, "evidence.pdf"),
		"Unsupported file extension"
	)
})

test_that("export_evidence_table rejects non-ClinicalTable", {
	expect_error(
		export_evidence_table(data.frame(x = 1), "evidence.docx"),
		"'table' must be a ClinicalTable object"
	)
})

test_that("export_evidence_table uses title parameter", {
	withr::with_tempdir({
		endpoints <- list(
			OS = list(
				result = MetaResult(
					estimate = 0.75,
					ci = c(0.60, 0.94),
					p_value = 0.012,
					n = 3L,
					effect_measure = "hr"
				),
				grade = EvidenceGrade(
					grade = "indication",
					grade_de = "Hinweis",
					n_studies = 3L
				)
			)
		)

		table <- create_evidence_summary_table(endpoints)

		export_evidence_table(table, "evidence.html", title = "Custom Title")

		html_content <- readLines("evidence.html")
		expect_true(any(grepl("Custom Title", html_content, fixed = TRUE)))
	})
})

# =============================================================================
# create_rob_summary_table() Tests
# =============================================================================

test_that("create_rob_summary_table works with RoB2Result objects", {
	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low"),
		assess_rob2("Study 2", "Low", "Some concerns", "Low", "Low", "Low")
	)

	table <- create_rob_summary_table(rob_results)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@type, "rob_summary")
	expect_s3_class(table@data, "data.frame")
	expect_equal(nrow(table@data), 2)
	expect_true("Study" %in% names(table@data))
	expect_true("Overall" %in% names(table@data))
})

test_that("create_rob_summary_table includes justification when requested", {
	rob_results <- list(
		assess_rob2("Study 1", "Low", "Low", "Low", "Low", "Low")
	)

	table <- create_rob_summary_table(
		rob_results,
		include_justification = TRUE
	)

	expect_true("Justification" %in% names(table@data))
})

test_that("create_rob_summary_table rejects non-RoB2Result objects", {
	expect_error(
		create_rob_summary_table(list("not a rob result")),
		"Element 1 of 'data' must be a RoB2Result object"
	)
})

test_that("create_rob_summary_table rejects empty list", {
	expect_error(
		create_rob_summary_table(list()),
		"'data' must be a non-empty list"
	)
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("Full workflow: create, export evidence summary table", {
	withr::with_tempdir({
		endpoints <- list(
			OS = list(
				result = MetaResult(
					estimate = 0.75,
					ci = c(0.60, 0.94),
					p_value = 0.012,
					n = 3L,
					effect_measure = "hr",
					heterogeneity = list(I2 = 25.0, tau2 = 0.01)
				),
				grade = EvidenceGrade(
					grade = "indication",
					grade_de = "Hinweis",
					n_studies = 3L
				),
				rob = list(
					assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
					assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
					assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low")
				)
			),
			PFS = list(
				result = MetaResult(
					estimate = 0.68,
					ci = c(0.52, 0.89),
					p_value = 0.005,
					n = 3L,
					effect_measure = "hr",
					heterogeneity = list(I2 = 45.0, tau2 = 0.03)
				),
				grade = EvidenceGrade(
					grade = "proof",
					grade_de = "Beleg",
					n_studies = 3L
				),
				rob = list(
					assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
					assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
					assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low")
				)
			)
		)

		table <- create_evidence_summary_table(endpoints, language = "en")

		expect_s7_class(table, ClinicalTable)
		expect_equal(nrow(table@data), 2)

		export_evidence_table(table, "summary.docx")
		expect_true(file.exists("summary.docx"))

		export_evidence_table(table, "summary.html")
		expect_true(file.exists("summary.html"))
	})
})

test_that("Full workflow: create study characteristics table", {
	withr::with_tempdir({
		studies <- list(
			TwoArmStudy(
				study_id = "STUDY001",
				design = "rct",
				population = "ITT",
				treatment_var = "Drug X",
				comparator = "Placebo",
				metadata = list(n = 250)
			),
			TwoArmStudy(
				study_id = "STUDY002",
				design = "rct",
				population = "FAS",
				treatment_var = "Drug X",
				comparator = "Standard",
				metadata = list(n = 300)
			)
		)

		table <- create_study_characteristics_table(studies)

		expect_s7_class(table, ClinicalTable)

		export_evidence_table(table, "studies.docx")
		expect_true(file.exists("studies.docx"))
	})
})
