test_that("create_demographics_table works", {
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		AGE = c(25, 30),
		SEX = c("M", "F"),
		RACE = c("WHITE", "BLACK"),
		SAFFL = c("Y", "Y")
	)
	adsl_data <- ADaMData(data = adsl, trt_var = "TRT01P", population = "SAFFL")

	tbl <- create_demographics_table(adsl_data)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "demographics")
})

test_that("create_region_table works", {
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		REGION1 = c("NA", "EU")
	)

	tbl <- create_region_table(adsl)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "enrollment")
})

test_that("create_medical_history_table works", {
	adsl <- data.frame(USUBJID = c("01"), TRT01P = c("A"))
	admh <- data.frame(USUBJID = c("01"), TRT01P = c("A"), MHBODSYS = c("SOC1"))

	tbl <- create_medical_history_table(adsl, admh)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "medical_history")
})

test_that("create_conmeds_table works", {
	adsl <- data.frame(USUBJID = c("01"), TRT01P = c("A"))
	adcm <- data.frame(USUBJID = c("01"), TRT01P = c("A"), CMCLAS = c("ATC1"))

	tbl <- create_conmeds_table(adsl, adcm)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "conmeds")
})

test_that("create_disposition_table works", {
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01P = c("A"),
		EOSSTT = c("COMPLETED")
	)

	tbl <- create_disposition_table(adsl)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "disposition")
})

test_that("create_population_summary_table works", {
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		SAFFL = c("Y", "N")
	)

	tbl <- create_population_summary_table(adsl)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "populations")
	expect_true("Population" %in% names(tbl@data))
})
