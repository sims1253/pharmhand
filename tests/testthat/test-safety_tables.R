# Tests for the unified create_ae_summary_table() function

test_that("create_ae_summary_table works with type='overview'", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEREL = c("RELATED", "NONE", "POSSIBLE"),
		AESER = c("N", "N", "Y"),
		AEACN = c("NONE", "DRUG WITHDRAWN", "NONE"),
		AEOUT = c("RECOVERED", "RECOVERED", "FATAL")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		SAFFL = c("Y", "Y", "Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "overview")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_overview")
	expect_true(any(grepl(
		"Subjects with at least one TEAE",
		tbl@data$Category,
		fixed = TRUE
	)))
})

# ------------------------------------------------------------------------------
# Tests for exposure-adjusted incidence rates
# ------------------------------------------------------------------------------

describe("calculate_exposure_adjusted_rate()", {
	it("calculates incidence rate per 100 patient-years", {
		result <- calculate_exposure_adjusted_rate(
			n_events = 10,
			patient_years = 2
		)

		expect_equal(result$rate, 500)
	})

	it("calculates Poisson CI using chi-square bounds", {
		conf_level <- 0.95
		n_events <- 5
		patient_years <- 2

		result <- calculate_exposure_adjusted_rate(
			n_events = n_events,
			patient_years = patient_years,
			conf_level = conf_level,
			per = 100
		)

		alpha <- 1 - conf_level
		lower_count <- stats::qchisq(alpha / 2, 2 * n_events) / 2
		upper_count <- stats::qchisq(1 - alpha / 2, 2 * (n_events + 1)) / 2

		expect_equal(
			result$ci_lower,
			(lower_count / patient_years) * 100,
			tolerance = 1e-10
		)
		expect_equal(
			result$ci_upper,
			(upper_count / patient_years) * 100,
			tolerance = 1e-10
		)
	})
})

test_that("calculate_exposure_adjusted_rate handles zero events", {
	result <- calculate_exposure_adjusted_rate(0, 100)
	expect_equal(result$rate, 0)
	expect_equal(result$ci_lower, 0)
	expect_true(result$ci_upper > 0)
	expect_equal(result$n_events, 0)
})

test_that("calculate_exposure_adjusted_rate handles small patient_years", {
	# With 1 event in 0.1 patient-years = rate of 1000 per 100 PY
	result <- calculate_exposure_adjusted_rate(1, 0.1)
	expect_equal(result$rate, 1000)
	expect_true(result$ci_lower > 0)
	expect_true(result$ci_upper > result$rate)
})

test_that("calculate_exposure_adjusted_rate handles large event counts", {
	result <- calculate_exposure_adjusted_rate(500, 1000)
	expect_equal(result$rate, 50)
	expect_true(result$ci_lower < result$rate)
	expect_true(result$ci_upper > result$rate)
	# CI should be relatively narrow for large counts
	expect_true((result$ci_upper - result$ci_lower) < 20)
})

describe("create_ae_exposure_table()", {
	it("creates an exposure-adjusted AE table with IDR columns", {
		adsl <- data.frame(
			USUBJID = c("01", "02", "03", "04"),
			TRT01P = c("A", "A", "B", "B"),
			SAFFL = rep("Y", 4),
			TRTDURD = c(365.25, 365.25, 182.625, 182.625),
			stringsAsFactors = FALSE
		)
		adae <- data.frame(
			USUBJID = c("01", "01", "02", "03"),
			TRT01P = c("A", "A", "A", "B"),
			TRTEMFL = rep("Y", 4),
			AEDECOD = rep("Headache", 4),
			stringsAsFactors = FALSE
		)

		tbl <- create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			by = "pt",
			exposure_var = "TRTDURD",
			time_unit = "days"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_exposure")
		expect_true("Preferred Term" %in% names(tbl@data))

		events_col <- "A\nEvents (n)"
		py_col <- "A\nPatient-Years"
		idr_col <- "A\nIDR per 100 PY (95% CI)"

		term_row <- tbl@data$`Preferred Term` == "Headache"
		expect_equal(tbl@data[[events_col]][term_row], "3")
		expect_equal(tbl@data[[py_col]][term_row], "2.00")

		stats <- calculate_exposure_adjusted_rate(
			3,
			2,
			conf_level = 0.95,
			per = 100
		)
		expected_idr <- paste0(
			format_number(stats$rate, digits = 2),
			" (",
			format_number(stats$ci_lower, digits = 2),
			", ",
			format_number(stats$ci_upper, digits = 2),
			")"
		)
		expect_equal(tbl@data[[idr_col]][term_row], expected_idr)
	})

	it("converts exposure time units to patient-years", {
		adae <- data.frame(
			USUBJID = "01",
			TRT01P = "A",
			TRTEMFL = "Y",
			AEDECOD = "Headache",
			stringsAsFactors = FALSE
		)

		adsl_days <- data.frame(
			USUBJID = "01",
			TRT01P = "A",
			SAFFL = "Y",
			TRTDURD = 365.25,
			stringsAsFactors = FALSE
		)
		adsl_weeks <- data.frame(
			USUBJID = "01",
			TRT01P = "A",
			SAFFL = "Y",
			TRTDURD = 365.25 / 7,
			stringsAsFactors = FALSE
		)
		adsl_months <- data.frame(
			USUBJID = "01",
			TRT01P = "A",
			SAFFL = "Y",
			TRTDURD = 12,
			stringsAsFactors = FALSE
		)

		py_col <- "A\nPatient-Years"

		tbl_days <- create_ae_exposure_table(
			adae = adae,
			adsl = adsl_days,
			by = "pt",
			exposure_var = "TRTDURD",
			time_unit = "days"
		)
		expect_equal(tbl_days@data[[py_col]][1], "1.00")

		tbl_weeks <- create_ae_exposure_table(
			adae = adae,
			adsl = adsl_weeks,
			by = "pt",
			exposure_var = "TRTDURD",
			time_unit = "weeks"
		)
		expect_equal(tbl_weeks@data[[py_col]][1], "1.00")

		tbl_months <- create_ae_exposure_table(
			adae = adae,
			adsl = adsl_months,
			by = "pt",
			exposure_var = "TRTDURD",
			time_unit = "months"
		)
		expect_equal(tbl_months@data[[py_col]][1], "1.00")
	})
})


test_that("create_ae_summary_table works with type='soc_pt'", {
	adae <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		TRTEMFL = c("Y", "Y"),
		AEBODSYS = c("SOC1", "SOC2"),
		AEDECOD = c("PT1", "PT2")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		SAFFL = c("Y", "Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "soc_pt")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_soc_pt")
	expect_true("System Organ Class" %in% names(tbl@data))
	expect_true("Preferred Term" %in% names(tbl@data))
})

test_that("create_ae_summary_table works with type='common'", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("SOC1", "SOC1", "SOC1"),
		AEDECOD = c("PT1", "PT1", "PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		SAFFL = c("Y", "Y", "Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "common", n_top = 5)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_common")
	expect_true("Preferred Term" %in% names(tbl@data))
})

test_that("create_ae_summary_table works with type='severity'", {
	adae <- data.frame(
		USUBJID = c("01", "01"),
		TRT01P = c("A", "A"),
		TRTEMFL = c("Y", "Y"),
		AESEV = c("MILD", "SEVERE")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "severity")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_severity")
	# Should take SEVERE as max
	expect_true(any(tbl@data$`Maximum Severity` == "SEVERE"))
})

test_that("create_ae_summary_table works with type='relationship'", {
	adae <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "A"),
		TRTEMFL = c("Y", "Y"),
		AEREL = c("RELATED", "NOT RELATED")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "A"),
		SAFFL = c("Y", "Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "relationship")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_relationship")
	expect_true("Relationship to Study Drug" %in% names(tbl@data))
})

test_that("create_ae_summary_table works with type='sae'", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		TRTEMFL = c("Y"),
		AESER = c("Y"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "sae")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_sae")
})

test_that("create_ae_summary_table works with type='discontinuation'", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		TRTEMFL = c("Y"),
		AEACN = c("DRUG WITHDRAWN"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "discontinuation")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_discontinuation")
})

test_that("create_ae_summary_table works with type='deaths'", {
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		SAFFL = c("Y", "Y"),
		DTHFL = c("Y", "N")
	)

	tbl <- create_ae_summary_table(adae = NULL, adsl = adsl, type = "deaths")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_deaths")
	expect_true(any(tbl@data$Statistic == "Deaths n (%)"))
})

test_that("create_ae_summary_table works with type='pt' and soc filter", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("SOC1", "SOC1", "SOC2"),
		AEDECOD = c("PT1", "PT2", "PT3")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		SAFFL = c("Y", "Y", "Y")
	)

	# Filter to SOC1 only
	tbl <- create_ae_summary_table(adae, adsl, type = "pt", soc = "SOC1")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_pt")
	expect_true("Preferred Term" %in% names(tbl@data))
	# Should only have PT1 and PT2 from SOC1
	expect_equal(nrow(tbl@data), 2)
})

test_that("create_ae_summary_table auto-generates titles", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		TRTEMFL = c("Y"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		SAFFL = c("Y")
	)

	tbl_soc <- create_ae_summary_table(adae, adsl, type = "soc")
	expect_equal(tbl_soc@title, "Adverse Events by System Organ Class")

	tbl_common <- create_ae_summary_table(adae, adsl, type = "common", n_top = 10)
	expect_equal(tbl_common@title, "Most Common Adverse Events (Top 10)")
})

test_that("create_ae_summary_table respects soc_order for type='soc'", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03", "04"),
		TRT01P = c("A", "A", "A", "A"),
		TRTEMFL = c("Y", "Y", "Y", "Y"),
		AEBODSYS = c("Z-SOC", "A-SOC", "M-SOC", "B-SOC")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03", "04"),
		TRT01P = c("A", "A", "A", "A"),
		SAFFL = c("Y", "Y", "Y", "Y")
	)

	# Without soc_order, should be alphabetical
	tbl_alpha <- create_ae_summary_table(adae, adsl, type = "soc")
	socs_alpha <- tbl_alpha@data$`System Organ Class`
	expect_equal(socs_alpha, c("A-SOC", "B-SOC", "M-SOC", "Z-SOC"))

	# With soc_order, should follow custom order
	custom_order <- c("M-SOC", "A-SOC", "Z-SOC", "B-SOC")
	tbl_custom <- create_ae_summary_table(
		adae,
		adsl,
		type = "soc",
		soc_order = custom_order
	)
	socs_custom <- tbl_custom@data$`System Organ Class`
	expect_equal(socs_custom, custom_order)
})

test_that("create_ae_summary_table respects soc_order for type='soc_pt'", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03", "04"),
		TRT01P = c("A", "A", "A", "A"),
		TRTEMFL = c("Y", "Y", "Y", "Y"),
		AEBODSYS = c("Z-SOC", "A-SOC", "M-SOC", "M-SOC"),
		AEDECOD = c("PT1", "PT2", "PT3", "PT4")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03", "04"),
		TRT01P = c("A", "A", "A", "A"),
		SAFFL = c("Y", "Y", "Y", "Y")
	)

	# With soc_order, SOC groups should follow custom order
	custom_order <- c("M-SOC", "A-SOC", "Z-SOC")
	tbl_custom <- create_ae_summary_table(
		adae,
		adsl,
		type = "soc_pt",
		soc_order = custom_order
	)
	socs_custom <- unique(tbl_custom@data$`System Organ Class`)
	expect_equal(socs_custom, custom_order)
})

test_that("create_ae_summary_table respects custom title", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		TRTEMFL = c("Y"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_summary_table(
		adae,
		adsl,
		type = "soc",
		title = "Custom Title"
	)
	expect_equal(tbl@title, "Custom Title")
})

test_that("create_ae_summary_table works without adsl (derives trt_n)", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("SOC1", "SOC1", "SOC1"),
		AEDECOD = c("PT1", "PT1", "PT1")
	)

	# Should work without adsl
	tbl <- create_ae_summary_table(adae, type = "soc")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_soc")
})

test_that("create_ae_summary_table validates input for deaths type", {
	expect_error(
		create_ae_summary_table(adae = NULL, adsl = NULL, type = "deaths"),
		"'adsl' must be a data frame"
	)
})

test_that("create_ae_summary_table handles empty SAE data gracefully", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		TRTEMFL = c("Y"),
		AESER = c("N"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_summary_table(adae, adsl, type = "sae")

	expect_s7_class(tbl, ClinicalTable)
	# Should have a "no SAE" message
	expect_true(
		nrow(tbl@data) == 0 ||
			("Message" %in%
				names(tbl@data) &&
				any(grepl("No serious", tbl@data$Message, ignore.case = TRUE)))
	)
})

create_time_to_first_ae_test_data <- function() {
	adsl <- data.frame(
		USUBJID = c("01", "02", "03", "04"),
		TRT01P = c("A", "A", "B", "B"),
		SAFFL = c("Y", "Y", "Y", "Y"),
		TRTDURD = c(10, 10, 10, 10),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("01", "03", "04"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("Infections", "Infections", "Cardiac"),
		ASTDY = c(3, 5, 8),
		stringsAsFactors = FALSE
	)

	list(adsl = adsl, adae = adae)
}

test_that("create_time_to_first_ae returns KM summary table", {
	data <- create_time_to_first_ae_test_data()

	result <- create_time_to_first_ae(
		adae = data$adae,
		adsl = data$adsl,
		ae_filter = AEBODSYS == "Infections",
		ref_group = "A"
	)

	expect_s7_class(result$table, ClinicalTable)
	expect_equal(result$table@type, "ae_time_to_first")
	expect_true(any(result$table@data$Statistic == "Median (95% CI)"))
})

test_that("create_time_to_first_ae returns HR from Cox model", {
	data <- create_time_to_first_ae_test_data()

	result <- create_time_to_first_ae(
		adae = data$adae,
		adsl = data$adsl,
		ae_filter = AEBODSYS == "Infections",
		ref_group = "A"
	)

	expect_true(inherits(result$hr, "coxph"))
	expect_true(any(result$table@data$Statistic == "HR (95% CI)"))
})

test_that("create_time_to_first_ae returns KM plot", {
	data <- create_time_to_first_ae_test_data()

	result <- create_time_to_first_ae(
		adae = data$adae,
		adsl = data$adsl,
		ae_filter = AEBODSYS == "Infections",
		ref_group = "A"
	)

	expect_s7_class(result$plot, ClinicalPlot)
})

# ==============================================================================
# Tests for AE Comparison Functionality
# ==============================================================================

# Helper function to create mock data with known values for comparison tests
create_comparison_test_data <- function() {
	# Create ADSL with 100 subjects per arm for easy percentage calculations
	adsl <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:200),
		TRT01P = rep(c("Placebo", "Active"), each = 100),
		SAFFL = rep("Y", 200),
		stringsAsFactors = FALSE
	)

	# Create ADAE with known incidences:
	# - Headache: 20% in Active (20/100), 10% in Placebo (10/100)
	# - Nausea: 15% in Active (15/100), 15% in Placebo (15/100)
	# - Fatigue: 5% in Active (5/100), 10% in Placebo (10/100)
	# - Rash: 8% in Active (8/100), 0% in Placebo (0/100) - tests zero incidence
	adae <- rbind(
		# Headache - SOC: Nervous system
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:10),
			TRT01P = "Placebo",
			TRTEMFL = "Y",
			AEBODSYS = "Nervous system disorders",
			AEDECOD = "Headache",
			stringsAsFactors = FALSE
		),
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 101:120),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "Nervous system disorders",
			AEDECOD = "Headache",
			stringsAsFactors = FALSE
		),
		# Nausea - SOC: Gastrointestinal
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 11:25),
			TRT01P = "Placebo",
			TRTEMFL = "Y",
			AEBODSYS = "Gastrointestinal disorders",
			AEDECOD = "Nausea",
			stringsAsFactors = FALSE
		),
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 121:135),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "Gastrointestinal disorders",
			AEDECOD = "Nausea",
			stringsAsFactors = FALSE
		),
		# Fatigue - SOC: General disorders
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 26:35),
			TRT01P = "Placebo",
			TRTEMFL = "Y",
			AEBODSYS = "General disorders",
			AEDECOD = "Fatigue",
			stringsAsFactors = FALSE
		),
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 136:140),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "General disorders",
			AEDECOD = "Fatigue",
			stringsAsFactors = FALSE
		),
		# Rash - SOC: Skin disorders (only in Active)
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 141:148),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "Skin and subcutaneous tissue disorders",
			AEDECOD = "Rash",
			stringsAsFactors = FALSE
		)
	)

	list(adsl = adsl, adae = adae)
}

# ------------------------------------------------------------------------------
# Tests for calculate_ae_risk_difference() - Internal function
# ------------------------------------------------------------------------------

describe("calculate_ae_risk_difference()", {
	it("calculates correct risk difference with known values", {
		# 20/100 vs 10/100 -> RD = 0.10 (10%)
		result <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		expect_equal(result$rd, 0.10, tolerance = 1e-10)
	})

	it("calculates correct risk ratio with known values", {
		# 20/100 vs 10/100 -> RR = 2.0
		result <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		expect_equal(result$rr, 2.0, tolerance = 1e-10)
	})

	it("returns valid p-value from chi-square test", {
		# Large counts should use chi-square
		result <- calculate_ae_risk_difference(
			n1 = 30,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		expect_true(!is.na(result$p_value))
		expect_true(result$p_value >= 0 && result$p_value <= 1)
		# With 30% vs 10%, p-value should be small
		expect_true(result$p_value < 0.05)
	})

	it("uses Fisher's exact test for small counts", {
		# Small counts (< 5 expected) should trigger Fisher's exact
		result <- calculate_ae_risk_difference(
			n1 = 3,
			N1 = 50,
			n2 = 1,
			N2 = 50,
			conf_level = 0.95
		)

		expect_true(!is.na(result$p_value))
		expect_true(result$p_value >= 0 && result$p_value <= 1)
	})

	it("applies continuity correction for zero incidence in reference", {
		# Zero in reference group - should apply continuity correction
		result <- calculate_ae_risk_difference(
			n1 = 10,
			N1 = 100,
			n2 = 0,
			N2 = 100,
			conf_level = 0.95
		)

		# RR should be finite (not Inf) due to continuity correction
		expect_true(is.finite(result$rr))
		expect_true(result$rr > 1) # Active has higher incidence
	})

	it("applies continuity correction for zero incidence in treatment", {
		# Zero in treatment group - should apply continuity correction
		result <- calculate_ae_risk_difference(
			n1 = 0,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		# RR should be finite (not Inf) due to continuity correction
		expect_true(is.finite(result$rr))
		expect_true(result$rr < 1) # Treatment has lower incidence
	})

	it("calculates correct confidence interval bounds", {
		result <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		# RD CI should contain the point estimate
		expect_true(result$rd_lower <= result$rd)
		expect_true(result$rd_upper >= result$rd)

		# RR CI should contain the point estimate
		expect_true(result$rr_lower <= result$rr)
		expect_true(result$rr_upper >= result$rr)

		# CI should be reasonable (not too wide)
		expect_true(result$rd_upper - result$rd_lower < 0.5)
	})

	it("respects different confidence levels", {
		result_95 <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)
		result_90 <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.90
		)

		# 90% CI should be narrower than 95% CI
		width_95 <- result_95$rd_upper - result_95$rd_lower
		width_90 <- result_90$rd_upper - result_90$rd_lower
		expect_true(width_90 < width_95)
	})

	it("handles equal incidences correctly", {
		# 15/100 vs 15/100 -> RD = 0, RR = 1
		result <- calculate_ae_risk_difference(
			n1 = 15,
			N1 = 100,
			n2 = 15,
			N2 = 100,
			conf_level = 0.95
		)

		expect_equal(result$rd, 0, tolerance = 1e-10)
		expect_equal(result$rr, 1, tolerance = 1e-10)
		# p-value should be 1 (no difference)
		expect_true(result$p_value > 0.99)
	})

	it("handles both proportions zero", {
		result <- calculate_ae_risk_difference(0, 100, 0, 100)
		expect_equal(result$rd, 0)
		expect_true(is.na(result$rr))
		expect_true(is.na(result$rr_lower))
		expect_true(is.na(result$rr_upper))
	})
})

# ------------------------------------------------------------------------------
# Tests for create_ae_comparison_table() - Main function
# ------------------------------------------------------------------------------

describe("create_ae_comparison_table()", {
	test_data <- create_comparison_test_data()

	it("returns a ClinicalTable object", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_comparison")
	})

	it("includes NNH column by default", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		expect_true(nnh_col %in% names(tbl@data))
	})

	it("works with 'soc' grouping", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "soc"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have SOC-level rows
		expect_true("Term" %in% names(tbl@data))
		# Should have fewer rows than PT grouping (4 SOCs vs 4 PTs in our test data)
		expect_equal(nrow(tbl@data), 4)
	})

	it("works with 'pt' grouping", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_true("Term" %in% names(tbl@data))
		# Should have 4 PT-level rows (Headache, Nausea, Fatigue, Rash)
		expect_equal(nrow(tbl@data), 4)
	})

	it("calculates NNH as inverse of absolute RD", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		headache_nnh <- tbl@data[tbl@data$Term == "Headache", nnh_col]
		result <- calculate_ae_risk_difference(n1 = 20, N1 = 100, n2 = 10, N2 = 100)
		expected_nnh <- 1 / abs(result$rd)
		expected_prefix <- format_number(expected_nnh, digits = 1)
		expect_true(startsWith(headache_nnh, expected_prefix))
	})

	it("shows NE when RD CI crosses zero", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		nausea_nnh <- tbl@data[tbl@data$Term == "Nausea", nnh_col]
		expect_equal(nausea_nnh, "NE")
	})

	it("allows excluding NNH column", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			include_nnh = FALSE
		)

		expect_false(any(grepl("NNH", names(tbl@data), fixed = TRUE)))
	})

	it("calculates NNH correctly when RD is negative (treatment beneficial)", {
		# Create test data where Active has FEWER events than Placebo
		# This tests that NNH still works when the treatment is actually beneficial
		adae_beneficial <- data.frame(
			USUBJID = c(
				paste0("SUBJ-", sprintf("%03d", 1:30)), # Active with AE
				paste0("SUBJ-", sprintf("%03d", 101:160)) # Placebo with AE
			),
			TRT01P = c(rep("Active", 30), rep("Placebo", 60)),
			AEDECOD = "Headache",
			AEBODSYS = "Nervous system disorders",
			TRTEMFL = "Y",
			stringsAsFactors = FALSE
		)

		adsl_beneficial <- data.frame(
			USUBJID = c(
				paste0("SUBJ-", sprintf("%03d", 1:100)), # Active arm (100 subjects)
				paste0("SUBJ-", sprintf("%03d", 101:200)) # Placebo arm (100 subjects)
			),
			TRT01P = c(rep("Active", 100), rep("Placebo", 100)),
			SAFFL = "Y",
			stringsAsFactors = FALSE
		)

		tbl <- create_ae_comparison_table(
			adae = adae_beneficial,
			adsl = adsl_beneficial,
			ref_group = "Placebo",
			by = "pt"
		)

		# RD = 30/100 - 60/100 = -0.30 (negative = treatment beneficial)
		# NNH should still be calculated as 1/|RD| = 1/0.30 ≈ 3.3
		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		nnh_value <- tbl@data[tbl@data$Term == "Headache", nnh_col]

		# Should NOT be "NE" since the CI won't cross zero with this large effect
		expect_false(nnh_value == "NE")

		# NNH should be approximately 3.3 (1/0.30)
		# Extract the point estimate from the formatted string
		nnh_point <- as.numeric(sub(" .*", "", nnh_value))
		expect_true(nnh_point > 2 && nnh_point < 5)
	})

	it("works with 'overall' grouping", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "overall"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have exactly 1 row for "Any TEAE"
		expect_equal(nrow(tbl@data), 1)
		expect_true(any(grepl("Any TEAE", tbl@data$Term, fixed = TRUE)))
	})

	it("filters by threshold correctly", {
		# Set threshold to 10% - should exclude Fatigue (5% in Active) and Rash (8%)
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			threshold = 10
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have 3 rows: Headache (20%), Nausea (15%), Fatigue (10% in Placebo)
		expect_equal(nrow(tbl@data), 3)
		# Rash should be excluded (8% max)
		expect_false(any(grepl("Rash", tbl@data$Term, fixed = TRUE)))
	})

	it("sorts by 'rd' correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			sort_by = "rd"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Headache RD = +10%, Rash = +8%, Nausea = 0%, Fatigue = -5%
		# Should be sorted by absolute RD descending
		expect_equal(tbl@data$Term[1], "Headache")
	})

	it("sorts by 'rr' correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			sort_by = "rr"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Rash has highest RR (infinite without correction, very high with)
		# Should be sorted by RR descending
		expect_equal(tbl@data$Term[1], "Rash")
	})

	it("sorts by 'incidence' correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			sort_by = "incidence"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Headache has highest incidence (20% in Active)
		expect_equal(tbl@data$Term[1], "Headache")
	})

	it("errors when ref_group is NULL", {
		expect_error(
			create_ae_comparison_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				ref_group = NULL,
				by = "pt"
			),
			"ref_group.*provided"
		)
	})

	it("errors when ref_group is invalid", {
		expect_error(
			create_ae_comparison_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				ref_group = "NonexistentGroup",
				by = "pt"
			),
			"must be one of the treatment groups"
		)
	})

	it("errors when adsl is missing", {
		expect_error(
			create_ae_comparison_table(
				adae = test_data$adae,
				adsl = NULL,
				ref_group = "Placebo",
				by = "pt"
			),
			"adsl.*must be a data frame"
		)
	})

	it("stores metadata correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			threshold = 5,
			conf_level = 0.90
		)

		expect_equal(tbl@metadata$ref_group, "Placebo")
		expect_equal(tbl@metadata$by, "pt")
		expect_equal(tbl@metadata$threshold, 5)
		expect_equal(tbl@metadata$conf_level, 0.90)
	})

	it("generates appropriate title when not provided", {
		tbl_pt <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)
		expect_true(grepl("Preferred Term", tbl_pt@title, fixed = TRUE))

		tbl_soc <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "soc"
		)
		expect_true(grepl("System Organ Class", tbl_soc@title, fixed = TRUE))
	})

	it("respects custom title", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			title = "Custom Comparison Title"
		)

		expect_equal(tbl@title, "Custom Comparison Title")
	})
})

describe("create_ae_comparison_table() with multiple treatment groups", {
	it("handles three treatment groups correctly", {
		# Create ADSL with 3 arms
		adsl_3arm <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:300),
			TRT01P = rep(c("Placebo", "Low Dose", "High Dose"), each = 100),
			SAFFL = rep("Y", 300),
			stringsAsFactors = FALSE
		)

		# Create ADAE with different incidences per arm
		adae_3arm <- rbind(
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 1:10),
				TRT01P = "Placebo",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			),
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 101:115),
				TRT01P = "Low Dose",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			),
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 201:225),
				TRT01P = "High Dose",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			)
		)

		tbl <- create_ae_comparison_table(
			adae = adae_3arm,
			adsl = adsl_3arm,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have columns for each treatment group comparison
		expect_true(any(grepl(
			"RD Low Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"RR Low Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"P-value (Low Dose vs Placebo)",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"RD High Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"RR High Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"P-value (High Dose vs Placebo)",
			names(tbl@data),
			fixed = TRUE
		)))
	})
})

# ------------------------------------------------------------------------------
# Tests for create_ae_summary_table() integration with type = "comparison"
# ------------------------------------------------------------------------------

describe("create_ae_summary_table() with type = 'comparison'", {
	test_data <- create_comparison_test_data()

	it("dispatches to create_ae_comparison_table correctly", {
		tbl <- create_ae_summary_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			type = "comparison",
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_comparison")
	})

	it("errors when ref_group is not provided for comparison type", {
		expect_error(
			create_ae_summary_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				type = "comparison"
			),
			"ref_group.*required"
		)
	})

	it("errors when adsl is not provided for comparison type", {
		expect_error(
			create_ae_summary_table(
				adae = test_data$adae,
				adsl = NULL,
				type = "comparison",
				ref_group = "Placebo"
			),
			"adsl.*required"
		)
	})

	it("passes through all comparison parameters correctly", {
		tbl <- create_ae_summary_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			type = "comparison",
			ref_group = "Placebo",
			by = "soc",
			threshold = 5,
			sort_by = "rd",
			conf_level = 0.90,
			title = "Custom Title from create_ae_summary_table"
		)

		expect_equal(tbl@metadata$ref_group, "Placebo")
		expect_equal(tbl@metadata$by, "soc")
		expect_equal(tbl@metadata$threshold, 5)
		expect_equal(tbl@metadata$conf_level, 0.90)
		expect_equal(tbl@title, "Custom Title from create_ae_summary_table")
	})
})

# ------------------------------------------------------------------------------
# Edge case tests
# ------------------------------------------------------------------------------

describe("AE comparison edge cases", {
	it("handles case where events exist in both groups but different terms", {
		# Tests when one group has an AE the other doesn't, but both have AEs
		adsl <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:100),
			TRT01P = rep(c("Placebo", "Active"), each = 50),
			SAFFL = rep("Y", 100),
			stringsAsFactors = FALSE
		)

		# Placebo has Nausea, Active has Headache
		adae <- rbind(
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 1:5),
				TRT01P = "Placebo",
				TRTEMFL = "Y",
				AEDECOD = "Nausea",
				stringsAsFactors = FALSE
			),
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 51:60),
				TRT01P = "Active",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			)
		)

		tbl <- create_ae_comparison_table(
			adae = adae,
			adsl = adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have 2 rows: Headache and Nausea
		expect_equal(nrow(tbl@data), 2)
	})

	it("returns NULL with warning when threshold excludes all events", {
		test_data <- create_comparison_test_data()

		# Set very high threshold that excludes everything
		expect_warning(
			tbl <- create_ae_comparison_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				ref_group = "Placebo",
				by = "pt",
				threshold = 99
			),
			"No adverse events meet the specified threshold"
		)

		expect_null(tbl)
	})

	it("handles missing AEBODSYS gracefully for pt grouping", {
		adsl <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:100),
			TRT01P = rep(c("Placebo", "Active"), each = 50),
			SAFFL = rep("Y", 100),
			stringsAsFactors = FALSE
		)

		# ADAE without AEBODSYS column (only AEDECOD)
		adae <- data.frame(
			USUBJID = sprintf("SUBJ%03d", c(1:10, 51:60)),
			TRT01P = rep(c("Placebo", "Active"), each = 10),
			TRTEMFL = rep("Y", 20),
			AEDECOD = rep("Headache", 20),
			stringsAsFactors = FALSE
		)

		# Should work for PT grouping without AEBODSYS
		tbl <- create_ae_comparison_table(
			adae = adae,
			adsl = adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
	})

	it("errors appropriately for soc grouping without AEBODSYS", {
		adsl <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:100),
			TRT01P = rep(c("Placebo", "Active"), each = 50),
			SAFFL = rep("Y", 100),
			stringsAsFactors = FALSE
		)

		# ADAE without AEBODSYS column
		adae <- data.frame(
			USUBJID = sprintf("SUBJ%03d", c(1:10, 51:60)),
			TRT01P = rep(c("Placebo", "Active"), each = 10),
			TRTEMFL = rep("Y", 20),
			AEDECOD = rep("Headache", 20),
			stringsAsFactors = FALSE
		)

		expect_error(
			create_ae_comparison_table(
				adae = adae,
				adsl = adsl,
				ref_group = "Placebo",
				by = "soc"
			),
			"AEBODSYS"
		)
	})
})

describe("create_ae_hierarchy_table", {
	it("creates hierarchical AE table with SOC and PT", {
		adsl <- data.frame(
			USUBJID = paste0("SUBJ", 1:20),
			TRT01P = rep(c("Active", "Placebo"), each = 10)
		)

		adae <- data.frame(
			USUBJID = rep(paste0("SUBJ", 1:10), each = 2),
			TRT01P = rep("Active", 20),
			AEBODSYS = rep(c("SOC1", "SOC2"), 10),
			AEDECOD = paste0("PT", 1:20)
		)

		table <- create_ae_hierarchy_table(adae, adsl)

		expect_true(S7::S7_inherits(table, ClinicalTable))
		expect_equal(table@type, "ae_hierarchy")
	})
})
