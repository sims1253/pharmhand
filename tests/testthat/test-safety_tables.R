test_that("create_ae_overview_table works", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEREL = c("RELATED", "NONE", "POSSIBLE"),
		AESER = c("N", "N", "Y"),
		AEACN = c("NONE", "DRUG WITHDRAWN", "NONE"),
		AEOUT = c("RECOVERED", "RECOVERED", "FATAL")
	)
	trt_n <- data.frame(TRT01A = c("A", "B"), N = c(2, 1))

	tbl <- create_ae_overview_table(adae, trt_n)

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
	expect_equal(tbl@type, "ae_overview")
	expect_true(any(grepl(
		"Subjects with at least one TEAE",
		tbl@data$Category,
		fixed = TRUE
	)))
})

test_that("create_ae_soc_table works", {
	adae <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "B"),
		TRTEMFL = c("Y", "Y"),
		AEBODSYS = c("SOC1", "SOC2")
	)
	trt_n <- data.frame(TRT01A = c("A", "B"), N = c(1, 1))

	tbl <- create_ae_soc_table(adae, trt_n)

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
	expect_equal(tbl@type, "ae_soc")
	expect_true("System Organ Class" %in% names(tbl@data))
})

test_that("create_common_ae_table works", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("SOC1", "SOC1", "SOC1"),
		AEDECOD = c("PT1", "PT1", "PT1")
	)
	trt_n <- data.frame(TRT01A = c("A", "B"), N = c(2, 1))

	tbl <- create_common_ae_table(adae, trt_n, n_top = 5)

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
	expect_equal(tbl@type, "ae_common")
	expect_true("Preferred Term" %in% names(tbl@data))
})

test_that("create_ae_severity_table works", {
	adae <- data.frame(
		USUBJID = c("01", "01"),
		TRT01A = c("A", "A"),
		TRTEMFL = c("Y", "Y"),
		AESEV = c("MILD", "SEVERE")
	)
	trt_n <- data.frame(TRT01A = c("A"), N = c(1))

	tbl <- create_ae_severity_table(adae, trt_n)

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
	expect_equal(tbl@type, "ae_severity")
	# Should take SEVERE as max
	expect_true(any(tbl@data$`Maximum Severity` == "SEVERE"))
})

test_that("create_sae_table works", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		TRTEMFL = c("Y"),
		AESER = c("Y"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	trt_n <- data.frame(TRT01A = c("A"), N = c(1))

	tbl <- create_sae_table(adae, trt_n)

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
	expect_equal(tbl@type, "sae")
})

test_that("create_deaths_table works", {
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "B"),
		SAFFL = c("Y", "Y"),
		DTHFL = c("Y", "N")
	)

	tbl <- create_deaths_table(adsl)

	expect_s7_class(tbl, FunctionReport::ClinicalTable)
	expect_equal(tbl@type, "deaths")
	expect_true(any(tbl@data$Statistic == "Deaths n (%)"))
})
