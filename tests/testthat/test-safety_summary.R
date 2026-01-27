# Tests for Safety Summary Functions
# Tests for create_ae_summary_table, create_ae_exposure_table

# Tests for the unified create_ae_summary_table() function

describe("create_ae_summary_table()", {
	it("create_ae_summary_table works with type='overview'", {
		data <- create_mock_ae_summary_data(n = 3)
		adae <- data$adae
		adsl <- data$adsl

		tbl <- create_ae_summary_table(adae, adsl, type = "overview")

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_overview")
		expect_true(any(grepl(
			"Subjects with at least one TEAE",
			tbl@data$Category,
			fixed = TRUE
		)))
	})
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

	it("calculate_exposure_adjusted_rate handles zero events", {
		result <- calculate_exposure_adjusted_rate(0, 100)
		expect_equal(result$rate, 0)
		expect_equal(result$ci_lower, 0)
		expect_true(result$ci_upper > 0)
		expect_equal(result$n_events, 0)
	})

	it("calculate_exposure_adjusted_rate handles small patient_years", {
		# With 1 event in 0.1 patient-years = rate of 1000 per 100 PY
		result <- calculate_exposure_adjusted_rate(1, 0.1)
		expect_equal(result$rate, 1000)
		expect_true(result$ci_lower > 0)
		expect_true(result$ci_upper > result$rate)
	})

	it("calculate_exposure_adjusted_rate handles large event counts", {
		result <- calculate_exposure_adjusted_rate(500, 1000)
		expect_equal(result$rate, 50)
		expect_true(result$ci_lower < result$rate)
		expect_true(result$ci_upper > result$rate)
		# CI should be relatively narrow for large counts
		expect_true((result$ci_upper - result$ci_lower) < 20)
	})
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
			data = adae,
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
			data = adae,
			adsl = adsl_days,
			by = "pt",
			exposure_var = "TRTDURD",
			time_unit = "days"
		)
		expect_equal(tbl_days@data[[py_col]][1], "1.00")

		tbl_weeks <- create_ae_exposure_table(
			data = adae,
			adsl = adsl_weeks,
			by = "pt",
			exposure_var = "TRTDURD",
			time_unit = "weeks"
		)
		expect_equal(tbl_weeks@data[[py_col]][1], "1.00")

		tbl_months <- create_ae_exposure_table(
			data = adae,
			adsl = adsl_months,
			by = "pt",
			exposure_var = "TRTDURD",
			time_unit = "months"
		)
		expect_equal(tbl_months@data[[py_col]][1], "1.00")
	})
})


describe("create_ae_summary_table() - various types", {
	it("create_ae_summary_table works with type='soc_pt'", {
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

	it("create_ae_summary_table works with type='common'", {
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

	it("create_ae_summary_table works with type='severity'", {
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

	it("create_ae_summary_table works with type='relationship'", {
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

	it("create_ae_summary_table works with type='sae'", {
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

	it("create_ae_summary_table works with type='discontinuation'", {
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

	it("create_ae_summary_table works with type='deaths'", {
		adsl <- data.frame(
			USUBJID = c("01", "02"),
			TRT01P = c("A", "B"),
			SAFFL = c("Y", "Y"),
			DTHFL = c("Y", "N")
		)

		tbl <- create_ae_summary_table(data = NULL, adsl = adsl, type = "deaths")

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_deaths")
		expect_true(any(tbl@data$Statistic == "Deaths n (%)"))
	})

	it("create_ae_summary_table works with type='pt' and soc filter", {
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

	it("create_ae_summary_table auto-generates titles", {
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

		tbl_common <- create_ae_summary_table(
			adae,
			adsl,
			type = "common",
			n_top = 10
		)
		expect_equal(tbl_common@title, "Most Common Adverse Events (Top 10)")
	})

	it("create_ae_summary_table orders SOCs alphabetically by default", {
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

		tbl_alpha <- create_ae_summary_table(adae, adsl, type = "soc")
		socs_alpha <- tbl_alpha@data$`System Organ Class`
		expect_equal(socs_alpha, c("A-SOC", "B-SOC", "M-SOC", "Z-SOC"))
	})

	it("create_ae_summary_table respects soc_order for type='soc'", {
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

	it("create_ae_summary_table respects soc_order for type='soc_pt'", {
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

	it("create_ae_summary_table respects custom title", {
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

	it("create_ae_summary_table works without adsl (derives trt_n)", {
		adae <- data.frame(
			USUBJID = c("01", "02", "03"),
			TRT01P = c("A", "A", "B"),
			TRTEMFL = c("Y", "Y", "Y"),
			AEBODSYS = c("SOC1", "SOC1", "SOC1"),
			AEDECOD = c("PT1", "PT1", "PT1")
		)

		# Should work without adsl - expect warning about deriving trt_n
		expect_warning(
			tbl <- create_ae_summary_table(adae, type = "soc"),
			"adsl not provided"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_soc")
	})

	it("create_ae_summary_table errors on NULL adsl for deaths type", {
		expect_error(
			create_ae_summary_table(data = NULL, adsl = NULL, type = "deaths"),
			"'adsl' data frame is required for type = 'deaths'"
		)
	})

	it("create_ae_summary_table handles empty SAE data gracefully", {
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
})

describe("create_time_to_first_ae()", {
	it("create_time_to_first_ae returns KM summary table", {
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

	it("create_time_to_first_ae returns HR from Cox model", {
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

	it("create_time_to_first_ae returns KM plot", {
		data <- create_time_to_first_ae_test_data()

		result <- create_time_to_first_ae(
			adae = data$adae,
			adsl = data$adsl,
			ae_filter = AEBODSYS == "Infections",
			ref_group = "A"
		)

		expect_s7_class(result$plot, ClinicalPlot)
	})
})
