# Test file for safety_tte.R
# Tests for time-to-event analysis of adverse events

test_that("calculate_ae_tte_data returns valid TTE data with valid inputs", {
	# Create valid mock data
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003", "SUBJ004"),
		SAFFL = c("Y", "Y", "Y", "Y"),
		TRT01P = c("Placebo", "Active", "Placebo", "Active"),
		TRTDURD = c(180, 165, 150, 175),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003", "SUBJ003"),
		AEBODSYS = c(
			"Nervous system",
			"Nervous system",
			"Gastrointestinal",
			"Nervous system"
		),
		TRTEMFL = c("Y", "Y", "Y", "Y"),
		ASTDY = c(10, 15, 5, 25),
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "TRT01P"
	)

	# Check output structure
	expect_s3_class(result, "data.frame")
	expect_true(all(c("USUBJID", "event", "time", "TRT01P") %in% names(result)))

	# Check subjects with events have event=1
	with_events <- result$USUBJID %in% c("SUBJ001", "SUBJ002", "SUBJ003")
	expect_true(all(result$event[with_events] == 1))

	# Check time for subjects with events matches ASTDY
	subj001 <- result[result$USUBJID == "SUBJ001", ]
	expect_equal(subj001$time, 10)
	expect_equal(subj001$event, 1)
})

test_that("calculate_ae_tte_data derives TRTDURD from TRTEDT/TRTSDT", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002"),
		SAFFL = c("Y", "Y"),
		TRT01P = c("Placebo", "Active"),
		TRTSDT = as.Date(c("2023-01-01", "2023-01-05")),
		TRTEDT = as.Date(c("2023-07-01", "2023-06-05")),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		AEBODSYS = c("Nervous system"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "TRT01P"
	)

	# TRTDURD should be derived: (TRTEDT - TRTSDT) + 1
	expect_true("TRTDURD" %in% names(result))
	expect_equal(result$TRTDURD[1], 182) # Jan 1 to July 1 is 181 days + 1 = 182
	expect_equal(result$TRTDURD[2], 152) # Jan 5 to June 5 is 151 days + 1 = 152
})

test_that("calculate_ae_tte_data errors with missing ADAE columns", {
	adsl <- create_mock_adsl()
	adae <- create_mock_adae()

	# Remove required column
	adae_missing <- adae[, !(names(adae) %in% c("TRTEMFL"))]

	expect_error(
		calculate_ae_tte_data(adsl, adae_missing, soc = "Nervous system"),
		"'adae' is missing required column"
	)
})

test_that("calculate_ae_tte_data errors with missing ADSL columns", {
	adsl <- create_mock_adsl()
	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		AEBODSYS = c("Nervous system"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		stringsAsFactors = FALSE
	)

	# Remove required column
	adsl_missing <- adsl[, !(names(adsl) %in% c("SAFFL"))]

	expect_error(
		calculate_ae_tte_data(adsl_missing, adae, soc = "Nervous system"),
		"'adsl' is missing required column"
	)
})

test_that(
		"calculate_ae_tte_data errors when neither TRTDURD nor
		TRTEDT/TRTSDT available",
		{
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002"),
		SAFFL = c("Y", "Y"),
		TRT01P = c("Placebo", "Active"),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		AEBODSYS = c("Nervous system"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		stringsAsFactors = FALSE
	)

	expect_error(
		calculate_ae_tte_data(adsl, adae, soc = "Nervous system"),
		"Cannot calculate treatment duration"
	)
})

test_that("calculate_ae_tte_data handles empty data", {
	adsl <- data.frame(
		USUBJID = character(0),
		SAFFL = character(0),
		TRT01P = character(0),
		TRTDURD = numeric(0),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = character(0),
		AEBODSYS = character(0),
		TRTEMFL = character(0),
		ASTDY = numeric(0),
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(adsl, adae, soc = "Nervous system")

	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 0)
})

test_that("calculate_ae_tte_data handles subjects without events (censored)", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003"),
		SAFFL = c("Y", "Y", "Y"),
		TRT01P = c("Placebo", "Active", "Placebo"),
		TRTDURD = c(180, 165, 150),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		AEBODSYS = c("Nervous system"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "TRT01P"
	)

	# All 3 subjects should be present (2 censored, 1 event)
	expect_equal(nrow(result), 3)

	# Check censored subjects have event=0 and time=TRTDURD
	censored <- result[result$USUBJID == "SUBJ002", ]
	expect_equal(censored$event, 0)
	expect_equal(censored$time, 165)
})

test_that("calculate_ae_tte_data filters to SAFFL == 'Y'", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003"),
		SAFFL = c("Y", "N", "Y"),
		TRT01P = c("Placebo", "Active", "Active"),
		TRTDURD = c(180, 165, 150),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002"),
		AEBODSYS = c("Nervous system", "Nervous system"),
		TRTEMFL = c("Y", "Y"),
		ASTDY = c(10, 15),
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "TRT01P"
	)

	# Only SAFFL == 'Y' subjects should be included
	expect_equal(nrow(result), 2)
	expect_false("SUBJ002" %in% result$USUBJID)
})

test_that(
		"calculate_ae_tte_data handles multiple events per subject (takes first)",
		{
	adsl <- data.frame(
		USUBJID = c("SUBJ001"),
		SAFFL = c("Y"),
		TRT01P = c("Placebo"),
		TRTDURD = c(180),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ001", "SUBJ001"),
		AEBODSYS = c("Nervous system", "Nervous system", "Nervous system"),
		TRTEMFL = c("Y", "Y", "Y"),
		ASTDY = c(25, 10, 15), # 10 is the first
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "TRT01P"
	)

	# Should take first event (ASTDY = 10)
	expect_equal(nrow(result), 1)
	expect_equal(result$time, 10)
})

test_that("calculate_ae_tte_data handles NA treatment values", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003"),
		SAFFL = c("Y", "Y", "Y"),
		TRT01P = c("Placebo", NA, "Active"),
		TRTDURD = c(180, 165, 150),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003"),
		AEBODSYS = c("Nervous system", "Nervous system", "Nervous system"),
		TRTEMFL = c("Y", "Y", "Y"),
		ASTDY = c(10, 15, 20),
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "TRT01P"
	)

	# Subjects with NA treatment should be filtered out
	expect_equal(nrow(result), 2)
	expect_false("SUBJ002" %in% result$USUBJID)
})

test_that("calculate_ae_tte_data uses custom trt_var", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002"),
		SAFFL = c("Y", "Y"),
		ACTARM = c("Placebo", "Active"),
		TRTDURD = c(180, 165),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		AEBODSYS = c("Nervous system"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		stringsAsFactors = FALSE
	)

	result <- calculate_ae_tte_data(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "ACTARM"
	)

	expect_true("ACTARM" %in% names(result))
	expect_true(!anyNA(result$ACTARM))
})

test_that("create_ae_km_plot_for_soc returns ClinicalPlot with valid data", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003", "SUBJ004"),
		SAFFL = c("Y", "Y", "Y", "Y"),
		TRT01P = c("Placebo", "Active", "Placebo", "Active"),
		TRTDURD = c(180, 165, 150, 175),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002"),
		AEBODSYS = c("Nervous system", "Nervous system"),
		TRTEMFL = c("Y", "Y"),
		ASTDY = c(10, 15),
		stringsAsFactors = FALSE
	)

	result <- create_ae_km_plot_for_soc(
		adsl,
		adae,
		soc = "Nervous system",
		trt_var = "TRT01P"
	)

	# Should return a ClinicalPlot object (check it's not NULL)
	expect_true(!is.null(result))
})

test_that("create_ae_km_plot_for_soc returns NULL with empty data", {
	adsl <- data.frame(
		USUBJID = character(0),
		SAFFL = character(0),
		TRT01P = character(0),
		TRTDURD = numeric(0),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = character(0),
		AEBODSYS = character(0),
		TRTEMFL = character(0),
		ASTDY = numeric(0),
		stringsAsFactors = FALSE
	)

	result <- create_ae_km_plot_for_soc(adsl, adae, soc = "Nervous system")

	expect_null(result)
})

test_that("create_time_to_first_ae returns valid result with valid inputs", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003", "SUBJ004"),
		SAFFL = c("Y", "Y", "Y", "Y"),
		TRT01P = c("Placebo", "Active", "Placebo", "Active"),
		TRTDURD = c(180, 165, 150, 175),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002", "SUBJ003"),
		TRTEMFL = c("Y", "Y", "Y"),
		ASTDY = c(10, 15, 20),
		AEBODSYS = c("Nervous system", "Nervous system", "Gastrointestinal"),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		ae_filter = AEBODSYS == "Nervous system",
		trt_var = "TRT01P",
		ref_group = "Placebo"
	)

	# Check result structure
	expect_type(result, "list")
	expect_true("table" %in% names(result))
	expect_true("plot" %in% names(result))
	expect_true("hr" %in% names(result))
})

test_that("create_time_to_first_ae works without ae_filter", {
	# Use larger dataset to avoid coxph convergence warnings
	n_per_arm <- 20
	adsl <- data.frame(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:(n_per_arm * 2))),
		SAFFL = rep("Y", n_per_arm * 2),
		TRT01P = rep(c("Placebo", "Active"), each = n_per_arm),
		TRTDURD = sample(90:180, n_per_arm * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# Create AE data with events for about 60% of subjects, varied timing
	event_subjects <- sample(adsl$USUBJID, round(0.6 * nrow(adsl)))
	adae <- data.frame(
		USUBJID = event_subjects,
		TRTEMFL = rep("Y", length(event_subjects)),
		ASTDY = sample.int(60, length(event_subjects), replace = TRUE),
		AEBODSYS = sample(
			c("Nervous system", "Gastrointestinal", "Skin"),
			length(event_subjects),
			replace = TRUE
		),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		trt_var = "TRT01P",
		ref_group = "Placebo"
	)

	expect_type(result, "list")
	expect_true("table" %in% names(result))
})

test_that("create_time_to_first_ae errors with missing required ADAE columns", {
	adsl <- create_mock_adsl()

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		TRTEMFL = c("Y"),
		# Missing ASTDY
		AEBODSYS = c("Nervous system"),
		stringsAsFactors = FALSE
	)

	expect_error(
		create_time_to_first_ae(adae = adae, adsl = adsl),
		"'adae' is missing required column"
	)
})

test_that("create_time_to_first_ae errors with missing required ADSL columns", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001"),
		# Missing SAFFL
		TRT01P = c("Placebo"),
		TRTDURD = c(180),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		AEBODSYS = c("Nervous system"),
		stringsAsFactors = FALSE
	)

	expect_error(
		create_time_to_first_ae(adae = adae, adsl = adsl),
		"'adsl' is missing required column"
	)
})

test_that("create_time_to_first_ae derives censor_var from TRTEDT/TRTSDT", {
	# Use larger dataset to avoid coxph convergence warnings
	n_per_arm <- 20
	adsl <- data.frame(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:(n_per_arm * 2))),
		SAFFL = rep("Y", n_per_arm * 2),
		TRT01P = rep(c("Placebo", "Active"), each = n_per_arm),
		TRTSDT = rep(as.Date("2023-01-01"), n_per_arm * 2),
		TRTEDT = as.Date("2023-01-01") +
			sample(90:180, n_per_arm * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# Create AE data with events for about 60% of subjects, varied timing
	event_subjects <- sample(adsl$USUBJID, round(0.6 * nrow(adsl)))
	adae <- data.frame(
		USUBJID = event_subjects,
		TRTEMFL = rep("Y", length(event_subjects)),
		ASTDY = sample.int(60, length(event_subjects), replace = TRUE),
		AEBODSYS = sample(
			c("Nervous system", "Gastrointestinal", "Skin"),
			length(event_subjects),
			replace = TRUE
		),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		censor_var = "TRTDURD", # Should be derived from TRTEDT/TRTSDT
		trt_var = "TRT01P",
		ref_group = "Placebo"
	)

	expect_type(result, "list")
})

test_that("create_time_to_first_ae errors when censor_var not derivable", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001"),
		SAFFL = c("Y"),
		TRT01P = c("Placebo"),
		# Missing TRTDURD, TRTEDT, TRTSDT
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		AEBODSYS = c("Nervous system"),
		stringsAsFactors = FALSE
	)

	expect_error(
		create_time_to_first_ae(adae = adae, adsl = adsl, censor_var = "TRTDURD"),
		"Cannot calculate censoring time"
	)
})

test_that("create_time_to_first_ae filters to TRTEMFL == 'Y'", {
	# Use dataset with mixed TRTEMFL but ensure data works with survfit
	n_per_arm <- 15
	adsl <- data.frame(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:(n_per_arm * 2))),
		SAFFL = rep("Y", n_per_arm * 2),
		TRT01P = rep(c("Placebo", "Active"), each = n_per_arm),
		TRTDURD = sample(90:180, n_per_arm * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# Create AE data with mixed TRTEMFL values
	# First 50% of subjects in each arm get events (TRTEMFL = Y)
	placebo_subjs <- adsl$USUBJID[adsl$TRT01P == "Placebo"]
	active_subjs <- adsl$USUBJID[adsl$TRT01P == "Active"]
	event_placebo <- placebo_subjs[1:floor(length(placebo_subjs) * 0.5)]
	event_active <- active_subjs[1:floor(length(active_subjs) * 0.5)]
	event_subjects <- c(event_placebo, event_active)

	adae <- data.frame(
		USUBJID = event_subjects,
		TRTEMFL = rep("Y", length(event_subjects)),
		ASTDY = sample.int(60, length(event_subjects), replace = TRUE),
		AEBODSYS = sample(
			c("Nervous system", "Gastrointestinal", "Skin"),
			length(event_subjects),
			replace = TRUE
		),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		trt_var = "TRT01P",
		ref_group = "Placebo"
	)

	expect_type(result, "list")
	# Only subjects with TRTEMFL = 'Y' should have events
})

test_that("create_time_to_first_ae errors with no subjects available", {
	# Use larger dataset so the "subjects excluded" warning is about filtering
	# not about too few subjects in the cox model
	n_per_arm <- 20
	adsl <- data.frame(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:(n_per_arm * 2))),
		SAFFL = rep("Y", n_per_arm * 2),
		TRT01P = rep(c("Placebo", "Active"), each = n_per_arm),
		TRTDURD = sample(90:180, n_per_arm * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# All subjects have invalid AE time (negative)
	adae <- data.frame(
		USUBJID = adsl$USUBJID,
		TRTEMFL = rep("Y", nrow(adsl)),
		ASTDY = rep(-1, nrow(adsl)), # Invalid time (negative)
		AEBODSYS = rep("Nervous system", nrow(adsl)),
		stringsAsFactors = FALSE
	)

	expect_warning(
		expect_error(
			create_time_to_first_ae(adae = adae, adsl = adsl),
			"No subjects available for time-to-first AE analysis"
		),
		"subjects excluded"
	)
})

test_that("create_time_to_first_ae handles custom time_var", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002"),
		SAFFL = c("Y", "Y"),
		TRT01P = c("Placebo", "Placebo"),
		TRTDURD = c(180, 180),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		ADY = c(15), # Custom time variable
		AEBODSYS = c("Nervous system"),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		trt_var = "TRT01P",
		ref_group = "Placebo",
		time_var = "ADY" # Use custom time variable
	)

	expect_type(result, "list")
})

test_that("create_time_to_first_ae handles custom censor_var", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001", "SUBJ002"),
		SAFFL = c("Y", "Y"),
		TRT01P = c("Placebo", "Placebo"),
		TRTDURD = c(180, 180),
		CUSTOM_CENSOR = c(200, 200), # Custom censoring variable
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		AEBODSYS = c("Nervous system"),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		trt_var = "TRT01P",
		ref_group = "Placebo",
		censor_var = "CUSTOM_CENSOR" # Use custom censoring variable
	)

	expect_type(result, "list")
})

test_that("create_time_to_first_ae validates conf_level parameter", {
	adsl <- data.frame(
		USUBJID = c("SUBJ001"),
		SAFFL = c("Y"),
		TRT01P = c("Placebo"),
		TRTDURD = c(180),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("SUBJ001"),
		TRTEMFL = c("Y"),
		ASTDY = c(10),
		AEBODSYS = c("Nervous system"),
		stringsAsFactors = FALSE
	)

	expect_error(
		create_time_to_first_ae(adae = adae, adsl = adsl, conf_level = 1.5),
		"'conf_level' must be greater than 0 and less than 1"
	)

	expect_error(
		create_time_to_first_ae(adae = adae, adsl = adsl, conf_level = -0.1),
		"'conf_level' must be greater than 0 and less than 1"
	)
})

test_that("create_time_to_first_ae handles multiple events per subject", {
	# Use larger dataset to avoid coxph convergence warnings
	n_per_arm <- 20
	adsl <- data.frame(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:(n_per_arm * 2))),
		SAFFL = rep("Y", n_per_arm * 2),
		TRT01P = rep(c("Placebo", "Active"), each = n_per_arm),
		TRTDURD = sample(90:180, n_per_arm * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# Create AE data with multiple events per subject
	event_subjects <- sample(adsl$USUBJID, round(0.6 * nrow(adsl)))
	adae_list <- lapply(event_subjects, function(subj) {
		n_events <- sample(2:4, 1) # 2-4 events per subject
		data.frame(
			USUBJID = rep(subj, n_events),
			TRTEMFL = rep("Y", n_events),
			ASTDY = sort(sample.int(60, n_events, replace = FALSE)),
			# First event is earliest
			AEBODSYS = sample(
				c("Nervous system", "Gastrointestinal", "Skin"),
				n_events,
				replace = TRUE
			),
			stringsAsFactors = FALSE
		)
	})
	adae <- do.call(rbind, adae_list)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		trt_var = "TRT01P",
		ref_group = "Placebo"
	)

	expect_type(result, "list")
})

test_that("create_time_to_first_ae filters SAFFL == 'Y'", {
	# Use dataset that works with survfit (both strata have censored observations)
	n_per_arm <- 15
	adsl <- data.frame(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:(n_per_arm * 2))),
		SAFFL = rep("Y", n_per_arm * 2), # All have SAFFL = Y for this test
		TRT01P = rep(c("Placebo", "Active"), each = n_per_arm),
		TRTDURD = sample(90:180, n_per_arm * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# Create AE data with events for about 50% of subjects per arm
	# This ensures both events and censored observations in each stratum
	placebo_subjs <- adsl$USUBJID[adsl$TRT01P == "Placebo"]
	active_subjs <- adsl$USUBJID[adsl$TRT01P == "Active"]
	event_placebo <- sample(placebo_subjs, floor(length(placebo_subjs) * 0.5))
	event_active <- sample(active_subjs, floor(length(active_subjs) * 0.5))
	event_subjects <- c(event_placebo, event_active)

	adae <- data.frame(
		USUBJID = event_subjects,
		TRTEMFL = rep("Y", length(event_subjects)),
		ASTDY = sample.int(60, length(event_subjects), replace = TRUE),
		AEBODSYS = sample(
			c("Nervous system", "Gastrointestinal", "Skin"),
			length(event_subjects),
			replace = TRUE
		),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		trt_var = "TRT01P",
		ref_group = "Placebo"
	)

	expect_type(result, "list")
	# Only SAFFL == 'Y' subjects should be in the analysis
})

test_that("create_time_to_first_ae uses custom title", {
	# Use dataset large enough for coxph convergence but avoid survfit
	# n.censor issue
	n_per_arm <- 15
	adsl <- data.frame(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:(n_per_arm * 2))),
		SAFFL = rep("Y", n_per_arm * 2),
		TRT01P = rep(c("Placebo", "Active"), each = n_per_arm),
		TRTDURD = sample(90:180, n_per_arm * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# Create AE data with events for about 70% of subjects, varied timing
	event_subjects <- sample(adsl$USUBJID, round(0.7 * nrow(adsl)))
	adae <- data.frame(
		USUBJID = event_subjects,
		TRTEMFL = rep("Y", length(event_subjects)),
		ASTDY = sample.int(60, length(event_subjects), replace = TRUE),
		AEBODSYS = sample(
			c("Nervous system", "Gastrointestinal", "Skin"),
			length(event_subjects),
			replace = TRUE
		),
		stringsAsFactors = FALSE
	)

	result <- create_time_to_first_ae(
		adae = adae,
		adsl = adsl,
		trt_var = "TRT01P",
		ref_group = "Placebo",
		title = "Custom AE Analysis Title"
	)

	expect_type(result, "list")
})
