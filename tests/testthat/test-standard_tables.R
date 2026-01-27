describe("standard tables", {
	it("create_demographics_table works", {
		adsl <- data.frame(
			USUBJID = c("01", "02"),
			TRT01P = c("A", "B"),
			AGE = c(25, 30),
			SEX = c("M", "F"),
			RACE = c("WHITE", "BLACK"),
			SAFFL = c("Y", "Y")
		)
		adsl_data <- ADaMData(data = adsl, domain = "ADSL", population = "SAFFL")

		tbl <- create_demographics_table(data = adsl_data)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "demographics")
	})

	it("create_region_table works", {
		adsl <- data.frame(
			USUBJID = c("01", "02"),
			TRT01P = c("A", "B"),
			REGION1 = c("NA", "EU")
		)

		tbl <- create_region_table(data = adsl)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "enrollment")
	})

	it("create_medical_history_table works", {
		adsl <- data.frame(USUBJID = c("01"), TRT01P = c("A"))
		adsl_data <- ADaMData(data = adsl, domain = "ADSL", population = "ITT")
		admh <- data.frame(USUBJID = c("01"), TRT01P = c("A"), MHBODSYS = c("SOC1"))
		admh_data <- ADaMData(data = admh, domain = "ADMH", population = "ITT")

		tbl <- create_medical_history_table(data = admh_data, adsl = adsl_data)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "medical_history")
	})

	it("create_conmeds_table works", {
		adsl <- data.frame(USUBJID = c("01"), TRT01P = c("A"))
		adsl_data <- ADaMData(data = adsl, domain = "ADSL", population = "ITT")
		adcm <- data.frame(USUBJID = c("01"), TRT01P = c("A"), CMCLAS = c("ATC1"))
		adcm_data <- ADaMData(data = adcm, domain = "ADCM", population = "ITT")

		tbl <- create_conmeds_table(data = adcm_data, adsl = adsl_data)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "conmeds")
	})

	it("create_disposition_table works", {
		adsl <- data.frame(
			USUBJID = c("01"),
			TRT01P = c("A"),
			EOSSTT = c("COMPLETED")
		)

		tbl <- create_disposition_table(data = adsl)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "disposition")
	})

	it("create_population_summary_table works", {
		adsl <- data.frame(
			USUBJID = c("01", "02"),
			TRT01P = c("A", "B"),
			SAFFL = c("Y", "N")
		)

		tbl <- create_population_summary_table(data = adsl)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "populations")
		expect_true("Population" %in% names(tbl@data))
	})

	it("create_hta_module4_table works", {
		data <- data.frame(
			Endpoint = "Primary Endpoint",
			`Analysis Set` = "ITT",
			Treatment = "Drug A",
			Comparator = "Placebo",
			Effect = "0.5",
			`95% CI` = "(-0.2, 1.2)",
			`p-value` = "0.045",
			check.names = FALSE
		)

		tbl <- create_hta_module4_table(data = data)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "module4")
	})

	it(
		paste0(
			"create_demographics_table accepts data.frame and wraps to ",
			"ADaMData"
		),
		{
			adsl <- data.frame(
				USUBJID = c("01", "02"),
				TRT01P = c("A", "B"),
				AGE = c(25, 30),
				SEX = c("M", "F"),
				RACE = c("WHITE", "BLACK")
			)

			tbl <- create_demographics_table(data = adsl)

			expect_s7_class(tbl, ClinicalTable)
			expect_equal(tbl@type, "demographics")
		}
	)
})
