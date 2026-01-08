# Test file for safety_exposure.R
# Tests for exposure-adjusted adverse event analysis functions

test_that("calculate_exposure_adjusted_rate: normal case", {
	result <- calculate_exposure_adjusted_rate(
		n_events = 10,
		patient_years = 5,
		conf_level = 0.95,
		per = 100
	)

	expect_type(result, "list")
	expect_named(
		result,
		c("rate", "ci_lower", "ci_upper", "n_events", "patient_years")
	)

	# Check basic calculations
	expect_equal(result$n_events, 10)
	expect_equal(result$patient_years, 5)

	# Rate should be (10 / 5) * 100 = 200
	expect_equal(result$rate, 200)

	# CI should be positive and reasonable
	expect_gt(result$ci_lower, 0)
	expect_gt(result$ci_upper, result$rate)
	expect_lt(result$ci_lower, result$rate)
})

test_that("calculate_exposure_adjusted_rate: zero events", {
	result <- calculate_exposure_adjusted_rate(
		n_events = 0,
		patient_years = 5,
		conf_level = 0.95,
		per = 100
	)

	expect_equal(result$rate, 0)
	expect_equal(result$ci_lower, 0)
	expect_gt(result$ci_upper, 0) # Upper CI should still be > 0
})

test_that("calculate_exposure_adjusted_rate: small patient_years warning", {
	# Expect warning for patient_years < 0.01
	expect_warning(
		calculate_exposure_adjusted_rate(
			n_events = 5,
			patient_years = 0.005,
			per = 100
		),
		"Very small patient_years value"
	)
})

test_that("calculate_exposure_adjusted_rate: invalid n_events", {
	# Negative n_events should error
	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = -1,
			patient_years = 5
		),
		"'n_events' must be a non-negative number"
	)

	# NA n_events should error
	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = NA_real_,
			patient_years = 5
		),
		"'n_events' must be a non-negative number"
	)
})

test_that("calculate_exposure_adjusted_rate: invalid patient_years", {
	# Zero patient_years should error
	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = 5,
			patient_years = 0
		),
		"'patient_years' must be a positive number"
	)

	# Negative patient_years should error
	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = 5,
			patient_years = -1
		),
		"'patient_years' must be a positive number"
	)

	# NA patient_years should error
	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = 5,
			patient_years = NA_real_
		),
		"'patient_years' must be a positive number"
	)
})

test_that("calculate_exposure_adjusted_rate: invalid conf_level", {
	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = 5,
			patient_years = 5,
			conf_level = 1.5
		),
		"must be greater than 0 and less than 1"
	)
})

test_that("calculate_exposure_adjusted_rate: invalid per parameter", {
	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = 5,
			patient_years = 5,
			per = 0
		),
		"'per' must be a positive number"
	)

	expect_error(
		calculate_exposure_adjusted_rate(
			n_events = 5,
			patient_years = 5,
			per = -1
		),
		"'per' must be a positive number"
	)
})

test_that("calculate_exposure_adjusted_rate: different per values", {
	result_100 <- calculate_exposure_adjusted_rate(
		n_events = 10,
		patient_years = 5,
		per = 100
	)

	result_1000 <- calculate_exposure_adjusted_rate(
		n_events = 10,
		patient_years = 5,
		per = 1000
	)

	# Rate should be 10x larger when per is 1000 vs 100
	expect_equal(result_1000$rate, result_100$rate * 10)
})

test_that("calculate_exposure_adjusted_rate: different confidence levels", {
	result_90 <- calculate_exposure_adjusted_rate(
		n_events = 10,
		patient_years = 5,
		conf_level = 0.90
	)

	result_99 <- calculate_exposure_adjusted_rate(
		n_events = 10,
		patient_years = 5,
		conf_level = 0.99
	)

	# 99% CI should be wider than 90% CI
	expect_gt(result_90$ci_lower, result_99$ci_lower) # 90% CI lower bound
})

test_that("create_ae_exposure_table: by = 'pt' (preferred term)", {
	set.seed(123)
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1) # Add exposure duration in days

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- sample(
		c("Y", "N"),
		nrow(adae),
		replace = TRUE,
		prob = c(0.7, 0.3)
	)

	result <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "pt",
		time_unit = "days"
	)

	expect_true(S7::S7_inherits(result, ClinicalTable))
	expect_s3_class(result@data, "data.frame")
	expect_true("Preferred Term" %in% names(result@data))
})

test_that("create_ae_exposure_table: by = 'soc' (system organ class)", {
	set.seed(123)
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- sample(
		c("Y", "N"),
		nrow(adae),
		replace = TRUE,
		prob = c(0.7, 0.3)
	)

	result <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "soc",
		time_unit = "days"
	)

	expect_true(S7::S7_inherits(result, ClinicalTable))
	expect_s3_class(result@data, "data.frame")
	expect_true("System Organ Class" %in% names(result@data))
})

test_that("create_ae_exposure_table: by = 'overall'", {
	set.seed(123)
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- sample(
		c("Y", "N"),
		nrow(adae),
		replace = TRUE,
		prob = c(0.7, 0.3)
	)

	result <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "overall",
		time_unit = "days"
	)

	expect_true(S7::S7_inherits(result, ClinicalTable))
	expect_s3_class(result@data, "data.frame")
	expect_true("Term" %in% names(result@data))
	expect_true("Any TEAE" %in% result@data$Term)
})

test_that("create_ae_exposure_table: time_unit options", {
	set.seed(123)
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- sample(
		c("Y", "N"),
		nrow(adae),
		replace = TRUE,
		prob = c(0.7, 0.3)
	)

	# Test different time units
	result_days <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "pt",
		time_unit = "days"
	)

	result_weeks <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "pt",
		time_unit = "weeks"
	)

	result_months <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "pt",
		time_unit = "months"
	)

	expect_true(S7::S7_inherits(result_days, ClinicalTable))
	expect_true(S7::S7_inherits(result_weeks, ClinicalTable))
	expect_true(S7::S7_inherits(result_months, ClinicalTable))
})

test_that("create_ae_exposure_table: threshold filters results", {
	set.seed(123)
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- sample(
		c("Y", "N"),
		nrow(adae),
		replace = TRUE,
		prob = c(0.7, 0.3)
	)

	result_all <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "pt",
		threshold = 0
	)

	# Set a high threshold that should filter out many terms
	result_filtered <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "pt",
		threshold = 50
	)

	expect_true(S7::S7_inherits(result_all, ClinicalTable))
	expect_true(S7::S7_inherits(result_filtered, ClinicalTable))
	# Either empty table or smaller than unfiltered
	expect_true(
		(isTRUE(result_filtered@metadata$empty)) ||
			nrow(result_filtered@data) <= nrow(result_all@data)
	)
})

test_that("create_ae_exposure_table: missing AEDECOD column when by = 'pt'", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"
	adae$AEDECOD <- NULL # Remove required column

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			by = "pt"
		),
		"'adae' is missing required column"
	)
})

test_that("create_ae_exposure_table: missing AEBODSYS column when by = 'soc'", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"
	adae$AEBODSYS <- NULL # Remove required column

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			by = "soc"
		),
		"'adae' is missing required column"
	)
})

test_that("create_ae_exposure_table: missing TRTDURD column in adsl", {
	adsl <- create_mock_adsl(n = 10)
	# Don't add TRTDURD

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			exposure_var = "TRTDURD"
		),
		"'adsl' is missing required column"
	)
})

test_that("create_ae_exposure_table: missing TRTEMFL column", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- NULL # Remove required column

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl
		),
		"'adae' is missing required column"
	)
})

test_that("create_ae_exposure_table: no TEAEs returns empty table", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "N" # All non-TEAEs

	expect_warning(
		result <- create_ae_exposure_table(
			adae = adae,
			adsl = adsl
		),
		"No treatment-emergent adverse events found"
	)

	# Should return empty ClinicalTable, not NULL
	expect_s7_class(result, ClinicalTable)
	expect_true(result@metadata$empty)
	expect_equal(
		result@metadata$empty_reason,
		"No treatment-emergent adverse events found"
	)
	expect_equal(nrow(result@data), 0)
})

test_that("create_ae_exposure_table: invalid by parameter", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			by = "invalid"
		),
		"'arg' should be one of"
	)
})

test_that("create_ae_exposure_table: invalid time_unit parameter", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			time_unit = "years"
		),
		"'arg' should be one of"
	)
})

test_that("create_ae_exposure_table: invalid per parameter", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			per = 0
		),
		"'per' must be a positive number"
	)
})

test_that("create_ae_exposure_table: invalid threshold parameter", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl,
			threshold = -1
		),
		"'threshold' must be a non-negative number"
	)
})

test_that("create_ae_exposure_table: negative exposure values error", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- c(round(runif(9, 30, 365), 1), -10) # Add negative value

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	expect_error(
		create_ae_exposure_table(
			adae = adae,
			adsl = adsl
		),
		"must be non-negative"
	)
})

test_that("create_ae_exposure_table: custom per value", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	result_100 <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		per = 100
	)

	result_1000 <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		per = 1000
	)

	expect_true(S7::S7_inherits(result_100, ClinicalTable))
	expect_true(S7::S7_inherits(result_1000, ClinicalTable))
})

test_that("create_ae_exposure_table: custom conf_level", {
	set.seed(123)
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	result <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		conf_level = 0.90
	)

	expect_true(S7::S7_inherits(result, ClinicalTable))
})

test_that("create_ae_exposure_table returns correct ClinicalTable", {
	set.seed(123)
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- round(runif(10, 30, 365), 1)

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- sample(
		c("Y", "N"),
		nrow(adae),
		replace = TRUE,
		prob = c(0.7, 0.3)
	)

	result <- create_ae_exposure_table(
		adae = adae,
		adsl = adsl,
		by = "pt"
	)

	# Check ClinicalTable structure
	expect_true(S7::S7_inherits(result, ClinicalTable))
	expect_no_error(result@flextable)
	expect_no_error(result@title)
	expect_no_error(result@metadata)
	expect_no_error(result@type)

	# Check metadata
	expect_equal(result@metadata$exposure_var, "TRTDURD")
	expect_equal(result@metadata$by, "pt")
	expect_equal(result@metadata$time_unit, "days")
	expect_equal(result@metadata$per, 100)
	expect_equal(result@metadata$conf_level, 0.95)
	expect_equal(result@metadata$threshold, 0)

	# Check type
	expect_equal(result@type, "ae_exposure")
})

test_that("create_ae_exposure_table: NA exposure values warn and exclude", {
	adsl <- create_mock_adsl(n = 10)
	adsl$TRTDURD <- c(round(runif(8, 30, 365), 1), NA, NA) # Add NA values

	adae <- create_mock_adae(n = 10)
	adae$TRTEMFL <- "Y"

	expect_warning(
		result <- create_ae_exposure_table(
			adae = adae,
			adsl = adsl
		),
		"subjects have missing TRTDURD"
	)

	expect_true(S7::S7_inherits(result, ClinicalTable))
})
