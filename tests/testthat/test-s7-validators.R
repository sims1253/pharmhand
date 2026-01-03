# Tests for S7 Class Validators

library(testthat)
library(pharmhand)

# ComparisonResult validators ----

test_that("ComparisonResult accepts valid effect_measure", {
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		effect_measure = "hr"
	))

	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		effect_measure = "or"
	))

	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		effect_measure = "rr"
	))

	expect_no_error(ComparisonResult(
		estimate = -0.15,
		ci = c(-0.25, -0.05),
		effect_measure = "rd"
	))

	expect_no_error(ComparisonResult(
		estimate = 2.5,
		ci = c(1.0, 4.0),
		effect_measure = "md"
	))

	expect_no_error(ComparisonResult(
		estimate = 0.5,
		ci = c(0.2, 0.8),
		effect_measure = "smd"
	))

	expect_no_error(ComparisonResult(
		estimate = 0.85,
		ci = c(0.70, 1.03),
		effect_measure = "irr"
	))
})

test_that("ComparisonResult rejects invalid effect_measure", {
	expect_error(
		ComparisonResult(
			estimate = 0.75,
			ci = c(0.60, 0.93),
			effect_measure = "invalid"
		),
		"effect_measure must be one of"
	)
})

test_that("ComparisonResult validates treatment is scalar", {
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		treatment = "Drug A"
	))

	# Empty string is valid (scalar character)
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		treatment = ""
	))
})

test_that("ComparisonResult validates control is scalar", {
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		control = "Placebo"
	))

	# Empty string is valid (scalar character)
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		control = ""
	))
})

# StatResult validators (tested through ComparisonResult) ----

test_that("StatResult estimate must be length 1", {
	# Valid: single value
	expect_no_error(ComparisonResult(estimate = 0.75, ci = c(0.60, 0.93)))

	# Invalid: vector of length > 1
	expect_error(
		ComparisonResult(estimate = c(0.75, 0.80), ci = c(0.60, 0.93)),
		"estimate must be a single numeric value"
	)

	# Invalid: empty vector
	expect_error(
		ComparisonResult(estimate = numeric(), ci = c(0.60, 0.93)),
		"estimate must be a single numeric value"
	)
})

test_that("StatResult ci must be length 2", {
	# Valid: length 2
	expect_no_error(ComparisonResult(estimate = 0.75, ci = c(0.60, 0.93)))

	# Invalid: length 1
	expect_error(
		ComparisonResult(estimate = 0.75, ci = c(0.60)),
		"ci must be a numeric vector of length 2"
	)

	# Invalid: length 3
	expect_error(
		ComparisonResult(estimate = 0.75, ci = c(0.60, 0.75, 0.93)),
		"ci must be a numeric vector of length 2"
	)
})

test_that("StatResult ci lower must be <= upper", {
	# Valid: lower < upper
	expect_no_error(ComparisonResult(estimate = 0.75, ci = c(0.60, 0.93)))

	# Valid: lower == upper (edge case)
	expect_no_error(ComparisonResult(estimate = 0.75, ci = c(0.75, 0.75)))

	# Invalid: lower > upper
	expect_error(
		ComparisonResult(estimate = 0.75, ci = c(0.93, 0.60)),
		"ci lower bound must be <= upper bound"
	)
})

test_that("StatResult ci_level must be between 0 and 1", {
	# Valid: common values
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		ci_level = 0.95
	))
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		ci_level = 0.90
	))
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		ci_level = 0.99
	))

	# Invalid: ci_level <= 0
	expect_error(
		ComparisonResult(estimate = 0.75, ci = c(0.60, 0.93), ci_level = 0),
		"ci_level must be a single value between 0 and 1"
	)
	expect_error(
		ComparisonResult(estimate = 0.75, ci = c(0.60, 0.93), ci_level = -0.1),
		"ci_level must be a single value between 0 and 1"
	)

	# Invalid: ci_level >= 1
	expect_error(
		ComparisonResult(estimate = 0.75, ci = c(0.60, 0.93), ci_level = 1),
		"ci_level must be a single value between 0 and 1"
	)
	expect_error(
		ComparisonResult(estimate = 0.75, ci = c(0.60, 0.93), ci_level = 1.5),
		"ci_level must be a single value between 0 and 1"
	)
})

test_that("StatResult method must be scalar", {
	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		method = "Cox proportional hazards"
	))

	expect_no_error(ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		method = ""
	))
})

# MetaResult validators ----

test_that("MetaResult accepts valid model", {
	expect_no_error(MetaResult(
		estimate = 0.80,
		ci = c(0.70, 0.91),
		model = "fixed"
	))

	expect_no_error(MetaResult(
		estimate = 0.80,
		ci = c(0.70, 0.91),
		model = "random"
	))
})

test_that("MetaResult rejects invalid model", {
	expect_error(
		MetaResult(estimate = 0.80, ci = c(0.70, 0.91), model = "mixed"),
		"model must be 'fixed' or 'random'"
	)

	expect_error(
		MetaResult(estimate = 0.80, ci = c(0.70, 0.91), model = "invalid"),
		"model must be 'fixed' or 'random'"
	)
})

test_that("MetaResult effect_measure validation works", {
	# All valid effect measures should work
	valid_measures <- c("hr", "or", "rr", "rd", "md", "smd", "irr")

	for (measure in valid_measures) {
		expect_no_error(
			MetaResult(
				estimate = 0.80,
				ci = c(0.70, 0.91),
				effect_measure = measure
			)
		)
	}

	# Invalid effect measure should error
	expect_error(
		MetaResult(estimate = 0.80, ci = c(0.70, 0.91), effect_measure = "invalid"),
		"effect_measure must be one of"
	)
})

# EvidenceGrade validators ----

test_that("EvidenceGrade accepts valid grade", {
	valid_grades <- c("proof", "indication", "hint", "none")

	for (grade in valid_grades) {
		expect_no_error(
			EvidenceGrade(grade = grade, direction = "benefit")
		)
	}
})

test_that("EvidenceGrade rejects invalid grade", {
	expect_error(
		EvidenceGrade(grade = "invalid", direction = "benefit"),
		"grade must be one of"
	)
})

test_that("EvidenceGrade accepts valid direction", {
	valid_directions <- c("benefit", "harm", "none")

	for (dir in valid_directions) {
		expect_no_error(
			EvidenceGrade(grade = "proof", direction = dir)
		)
	}
})

test_that("EvidenceGrade rejects invalid direction", {
	expect_error(
		EvidenceGrade(grade = "proof", direction = "invalid"),
		"direction must be 'benefit', 'harm', or 'none'"
	)
})

# Study validators ----

test_that("Study accepts valid design", {
	valid_designs <- c("rct", "observational", "single-arm", "crossover")

	for (design in valid_designs) {
		expect_no_error(
			TwoArmStudy(
				data = data.frame(),
				study_id = "TEST",
				study_title = "Test Study",
				design = design
			)
		)
	}
})

test_that("Study rejects invalid design", {
	# TwoArmStudy doesn't override design property, so it inherits the validator
	expect_error(
		TwoArmStudy(
			data = data.frame(),
			study_id = "TEST",
			study_title = "Test Study",
			design = "invalid"
		),
		"design must be one of"
	)
})

test_that("Study accepts all valid design types individually", {
	# TwoArmStudy doesn't override design property, so it inherits the validator
	expect_no_error(TwoArmStudy(
		data = data.frame(),
		study_id = "TEST",
		study_title = "Test Study",
		design = "rct"
	))
	expect_no_error(TwoArmStudy(
		data = data.frame(),
		study_id = "TEST",
		study_title = "Test Study",
		design = "observational"
	))
	expect_no_error(TwoArmStudy(
		data = data.frame(),
		study_id = "TEST",
		study_title = "Test Study",
		design = "single-arm"
	))
	expect_no_error(TwoArmStudy(
		data = data.frame(),
		study_id = "TEST",
		study_title = "Test Study",
		design = "crossover"
	))
})

test_that("Study design defaults correctly", {
	study <- SingleArmStudy(
		data = data.frame(),
		study_id = "TEST",
		study_title = "Test Study"
	)

	expect_equal(study@design, "single-arm")
})

# Endpoint validators ----

test_that("Endpoint accepts valid type", {
	valid_types <- c("continuous", "binary", "tte", "count", "pro")

	for (type in valid_types) {
		expect_no_error(
			Endpoint(
				name = "Test Endpoint",
				variable = "AVAL",
				type = type
			)
		)
	}
})

test_that("Endpoint rejects invalid type", {
	expect_error(
		Endpoint(name = "Test", variable = "AVAL", type = "invalid"),
		"type must be one of"
	)
})

test_that("Endpoint accepts all valid type types individually", {
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		type = "continuous"
	))
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", type = "binary"))
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", type = "tte"))
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", type = "count"))
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", type = "pro"))
})

test_that("Endpoint accepts valid category", {
	valid_categories <- c("primary", "secondary", "safety", "exploratory")

	for (cat in valid_categories) {
		expect_no_error(
			Endpoint(
				name = "Test Endpoint",
				variable = "AVAL",
				category = cat
			)
		)
	}
})

test_that("Endpoint rejects invalid category", {
	expect_error(
		Endpoint(name = "Test", variable = "AVAL", category = "invalid"),
		"category must be one of"
	)
})

test_that("Endpoint accepts all valid category types individually", {
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		category = "primary"
	))
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		category = "secondary"
	))
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		category = "safety"
	))
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		category = "exploratory"
	))
})

test_that("Endpoint accepts valid hypothesis", {
	valid_hypotheses <- c("superiority", "non-inferiority", "equivalence")

	for (hyp in valid_hypotheses) {
		expect_no_error(
			Endpoint(
				name = "Test Endpoint",
				variable = "AVAL",
				hypothesis = hyp
			)
		)
	}
})

test_that("Endpoint rejects invalid hypothesis", {
	expect_error(
		Endpoint(name = "Test", variable = "AVAL", hypothesis = "invalid"),
		"hypothesis must be one of"
	)
})

test_that("Endpoint accepts all valid hypothesis types individually", {
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		hypothesis = "superiority"
	))
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		hypothesis = "non-inferiority"
	))
	expect_no_error(Endpoint(
		name = "Test",
		variable = "AVAL",
		hypothesis = "equivalence"
	))
})

test_that("Endpoint alpha must be between 0 and 1", {
	# Valid values
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", alpha = 0.05))
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", alpha = 0.01))
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", alpha = 0.10))
	expect_no_error(Endpoint(name = "Test", variable = "AVAL", alpha = 0.5))

	# Invalid: alpha <= 0
	expect_error(
		Endpoint(name = "Test", variable = "AVAL", alpha = 0),
		"alpha must be between 0 and 1"
	)
	expect_error(
		Endpoint(name = "Test", variable = "AVAL", alpha = -0.01),
		"alpha must be between 0 and 1"
	)

	# Invalid: alpha >= 1
	expect_error(
		Endpoint(name = "Test", variable = "AVAL", alpha = 1),
		"alpha must be between 0 and 1"
	)
	expect_error(
		Endpoint(name = "Test", variable = "AVAL", alpha = 1.5),
		"alpha must be between 0 and 1"
	)
})

# StudySet validators ----

test_that("StudySet accepts valid studies list", {
	study1 <- SingleArmStudy(
		data = data.frame(),
		study_id = "STUDY001",
		study_title = "Study 1"
	)
	study2 <- SingleArmStudy(
		data = data.frame(),
		study_id = "STUDY002",
		study_title = "Study 2"
	)

	expect_no_error(StudySet(
		studies = list(study1, study2),
		comparison_type = "direct"
	))
})

test_that("StudySet accepts empty studies list", {
	expect_no_error(StudySet(
		studies = list(),
		comparison_type = "direct"
	))
})

test_that("StudySet rejects non-Study objects in studies list", {
	study1 <- SingleArmStudy(
		data = data.frame(),
		study_id = "STUDY001",
		study_title = "Study 1"
	)

	expect_error(
		StudySet(studies = list(study1, "not a study"), comparison_type = "direct"),
		"must be a Study object"
	)

	expect_error(
		StudySet(studies = list(study1, data.frame()), comparison_type = "direct"),
		"must be a Study object"
	)
})

test_that("StudySet accepts valid comparison_type", {
	valid_types <- c("direct", "indirect", "network")

	for (type in valid_types) {
		expect_no_error(
			StudySet(comparison_type = type)
		)
	}
})

test_that("StudySet rejects invalid comparison_type", {
	expect_error(
		StudySet(comparison_type = "invalid"),
		"comparison_type must be one of"
	)
})

test_that("StudySet accepts all valid comparison types individually", {
	expect_no_error(StudySet(comparison_type = "direct"))
	expect_no_error(StudySet(comparison_type = "indirect"))
	expect_no_error(StudySet(comparison_type = "network"))
})

test_that("StudySet comparison_type defaults correctly", {
	study_set <- StudySet()

	expect_equal(study_set@comparison_type, "direct")
})

# Integration tests ----

test_that("Can create complete ComparisonResult with all properties", {
	result <- ComparisonResult(
		estimate = 0.75,
		ci = c(0.60, 0.93),
		ci_level = 0.95,
		p_value = 0.008,
		method = "Cox proportional hazards model",
		effect_measure = "hr",
		treatment = "Drug A",
		control = "Placebo"
	)

	expect_true(S7::S7_inherits(result, ComparisonResult))
	expect_equal(result@estimate, 0.75)
	expect_equal(result@ci, c(0.60, 0.93))
	expect_equal(result@effect_measure, "hr")
})

test_that("Can create complete MetaResult with all properties", {
	result <- MetaResult(
		estimate = 0.80,
		ci = c(0.70, 0.91),
		ci_level = 0.95,
		p_value = 0.001,
		method = "REML with Knapp-Hartung",
		effect_measure = "hr",
		model = "random",
		heterogeneity = list(Q = 15.2, I2 = 0.45, tau2 = 0.02),
		n = 5L
	)

	expect_true(S7::S7_inherits(result, MetaResult))
	expect_equal(result@estimate, 0.80)
	expect_equal(result@model, "random")
})

test_that("Can create complete EvidenceGrade with all properties", {
	grade <- EvidenceGrade(
		grade = "indication",
		grade_de = "Hinweis",
		direction = "benefit",
		certainty = 0.6,
		n_studies = 3L,
		justification = "Consistent results from 3 RCTs with moderate risk of bias"
	)

	expect_true(S7::S7_inherits(grade, EvidenceGrade))
	expect_equal(grade@grade, "indication")
	expect_equal(grade@direction, "benefit")
})

test_that("Can create complete Endpoint with all properties", {
	endpoint <- Endpoint(
		name = "Overall Survival",
		variable = "AVAL",
		type = "tte",
		category = "primary",
		description = "Time from randomization to death",
		hypothesis = "superiority",
		margin = NULL,
		alpha = 0.05,
		priority = 1
	)

	expect_true(S7::S7_inherits(endpoint, Endpoint))
	expect_equal(endpoint@type, "tte")
	expect_equal(endpoint@category, "primary")
})

test_that("Can create complete StudySet with all properties", {
	study1 <- SingleArmStudy(
		data = data.frame(),
		study_id = "STUDY001",
		study_title = "Study 1"
	)

	endpoint <- Endpoint(
		name = "OS",
		variable = "AVAL",
		type = "tte",
		category = "primary"
	)

	study_set <- StudySet(
		studies = list(study1),
		endpoint = endpoint,
		comparison_type = "direct",
		common_comparator = "Placebo",
		characteristics = data.frame(study_id = "STUDY001", n = 100)
	)

	expect_true(S7::S7_inherits(study_set, StudySet))
	expect_equal(study_set@comparison_type, "direct")
	expect_equal(study_set@n_studies, 1)
})
