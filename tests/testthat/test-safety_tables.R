# Tests for the unified create_ae_table() function

test_that("create_ae_table works with type='overview'", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEREL = c("RELATED", "NONE", "POSSIBLE"),
		AESER = c("N", "N", "Y"),
		AEACN = c("NONE", "DRUG WITHDRAWN", "NONE"),
		AEOUT = c("RECOVERED", "RECOVERED", "FATAL")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		SAFFL = c("Y", "Y", "Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "overview")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_overview")
	expect_true(any(grepl(
		"Subjects with at least one TEAE",
		tbl@data$Category,
		fixed = TRUE
	)))
})

test_that("create_ae_table works with type='soc'", {
	adae <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "B"),
		TRTEMFL = c("Y", "Y"),
		AEBODSYS = c("SOC1", "SOC2")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "B"),
		SAFFL = c("Y", "Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "soc")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_soc")
	expect_true("System Organ Class" %in% names(tbl@data))
})

test_that("create_ae_table works with type='soc_pt'", {
	adae <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "B"),
		TRTEMFL = c("Y", "Y"),
		AEBODSYS = c("SOC1", "SOC2"),
		AEDECOD = c("PT1", "PT2")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "B"),
		SAFFL = c("Y", "Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "soc_pt")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_soc_pt")
	expect_true("System Organ Class" %in% names(tbl@data))
	expect_true("Preferred Term" %in% names(tbl@data))
})

test_that("create_ae_table works with type='common'", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("SOC1", "SOC1", "SOC1"),
		AEDECOD = c("PT1", "PT1", "PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		SAFFL = c("Y", "Y", "Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "common", n_top = 5)

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_common")
	expect_true("Preferred Term" %in% names(tbl@data))
})

test_that("create_ae_table works with type='severity'", {
	adae <- data.frame(
		USUBJID = c("01", "01"),
		TRT01A = c("A", "A"),
		TRTEMFL = c("Y", "Y"),
		AESEV = c("MILD", "SEVERE")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "severity")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_severity")
	# Should take SEVERE as max
	expect_true(any(tbl@data$`Maximum Severity` == "SEVERE"))
})

test_that("create_ae_table works with type='relationship'", {
	adae <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "A"),
		TRTEMFL = c("Y", "Y"),
		AEREL = c("RELATED", "NOT RELATED")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "A"),
		SAFFL = c("Y", "Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "relationship")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_relationship")
	expect_true("Relationship to Study Drug" %in% names(tbl@data))
})

test_that("create_ae_table works with type='sae'", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		TRTEMFL = c("Y"),
		AESER = c("Y"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "sae")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_sae")
})

test_that("create_ae_table works with type='discontinuation'", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		TRTEMFL = c("Y"),
		AEACN = c("DRUG WITHDRAWN"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "discontinuation")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_discontinuation")
})

test_that("create_ae_table works with type='deaths'", {
	adsl <- data.frame(
		USUBJID = c("01", "02"),
		TRT01A = c("A", "B"),
		SAFFL = c("Y", "Y"),
		DTHFL = c("Y", "N")
	)

	tbl <- create_ae_table(adae = NULL, adsl = adsl, type = "deaths")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_deaths")
	expect_true(any(tbl@data$Statistic == "Deaths n (%)"))
})

test_that("create_ae_table works with type='pt' and soc filter", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("SOC1", "SOC1", "SOC2"),
		AEDECOD = c("PT1", "PT2", "PT3")
	)
	adsl <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		SAFFL = c("Y", "Y", "Y")
	)

	# Filter to SOC1 only
	tbl <- create_ae_table(adae, adsl, type = "pt", soc = "SOC1")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_pt")
	expect_true("Preferred Term" %in% names(tbl@data))
	# Should only have PT1 and PT2 from SOC1
	expect_equal(nrow(tbl@data), 2)
})

test_that("create_ae_table auto-generates titles", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		TRTEMFL = c("Y"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		SAFFL = c("Y")
	)

	tbl_soc <- create_ae_table(adae, adsl, type = "soc")
	expect_equal(tbl_soc@title, "Adverse Events by System Organ Class")

	tbl_common <- create_ae_table(adae, adsl, type = "common", n_top = 10)
	expect_equal(tbl_common@title, "Most Common Adverse Events (Top 10)")
})

test_that("create_ae_table respects custom title", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		TRTEMFL = c("Y"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "soc", title = "Custom Title")
	expect_equal(tbl@title, "Custom Title")
})

test_that("create_ae_table works without adsl (derives trt_n from adae)", {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01A = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("SOC1", "SOC1", "SOC1"),
		AEDECOD = c("PT1", "PT1", "PT1")
	)

	# Should work without adsl
	tbl <- create_ae_table(adae, type = "soc")

	expect_s7_class(tbl, ClinicalTable)
	expect_equal(tbl@type, "ae_soc")
})

test_that("create_ae_table validates input for deaths type", {
	expect_error(
		create_ae_table(adae = NULL, adsl = NULL, type = "deaths"),
		"adsl.*required"
	)
})

test_that("create_ae_table handles empty SAE data gracefully", {
	adae <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		TRTEMFL = c("Y"),
		AESER = c("N"),
		AEBODSYS = c("SOC1"),
		AEDECOD = c("PT1")
	)
	adsl <- data.frame(
		USUBJID = c("01"),
		TRT01A = c("A"),
		SAFFL = c("Y")
	)

	tbl <- create_ae_table(adae, adsl, type = "sae")

	expect_s7_class(tbl, ClinicalTable)
	# Should have a "no SAE" message
	expect_true(
		nrow(tbl@data) == 0 ||
			("Message" %in%
				names(tbl@data) &&
				any(grepl("No serious", tbl@data$Message, ignore.case = TRUE)))
	)
})
