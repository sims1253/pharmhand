# Tests for competing risk analysis functions (R/competing_risks.R)
# Issue #172: Competing risk analysis (Fine-Gray)

# =============================================================================
# competing_risk_analysis tests
# =============================================================================

describe("competing_risk_analysis", {
	it("returns a CompetingRiskResult object", {
		skip_if_not_installed("cmprsk")

		data <- create_competing_risk_test_data(n = 100, seed = 123)
		result <- competing_risk_analysis(
			data,
			time_var = "time",
			event_var = "event",
			trt_var = "TRT01P",
			main_event = 1,
			competing_events = c(2, 3)
		)

		expect_true(S7::S7_inherits(result, CompetingRiskResult))
	})

	it("validates input data", {
		skip_if_not_installed("cmprsk")

		expect_error(
			competing_risk_analysis(
				"not_dataframe",
				time_var = "time",
				event_var = "event",
				trt_var = "TRT01P",
				main_event = 1,
				competing_events = c(2, 3)
			),
			"data frame"
		)
	})

	it("validates required variables exist", {
		skip_if_not_installed("cmprsk")

		data <- create_competing_risk_test_data()

		expect_error(
			competing_risk_analysis(
				data,
				time_var = "MISSING_VAR",
				event_var = "event",
				trt_var = "TRT01P",
				main_event = 1,
				competing_events = c(2, 3)
			),
			"MISSING_VAR"
		)
	})

	it("estimates cumulative incidence function", {
		skip_if_not_installed("cmprsk")

		data <- create_competing_risk_test_data(n = 100, seed = 456)
		result <- competing_risk_analysis(
			data,
			time_var = "time",
			event_var = "event",
			trt_var = "TRT01P",
			main_event = 1,
			competing_events = c(2, 3)
		)

		expect_true("cif_main" %in% S7::prop_names(result))
		expect_true("cif_competing" %in% S7::prop_names(result))
		expect_true("cif_by_treatment" %in% S7::prop_names(result))
	})

	it("calculates treatment effects", {
		skip_if_not_installed("cmprsk")

		data <- create_competing_risk_test_data(n = 100, seed = 789)
		result <- competing_risk_analysis(
			data,
			time_var = "time",
			event_var = "event",
			trt_var = "TRT01P",
			main_event = 1,
			competing_events = c(2, 3)
		)

		expect_true("treatment_comparison" %in% S7::prop_names(result))
		expect_true("subhazard_ratio" %in% S7::prop_names(result))
	})

	it("handles multiple competing events", {
		skip_if_not_installed("cmprsk")

		set.seed(111)
		n <- 100
		times <- rexp(n, 0.1)
		events <- sample(
			c(0, 1, 2, 3),
			n,
			replace = TRUE,
			prob = c(0.6, 0.2, 0.1, 0.1)
		)
		data <- data.frame(
			time = times,
			event = events,
			TRT01P = sample(c("A", "B"), n, replace = TRUE)
		)

		result <- competing_risk_analysis(
			data,
			time_var = "time",
			event_var = "event",
			trt_var = "TRT01P",
			main_event = 1,
			competing_events = c(2, 3)
		)

		expect_true(S7::S7_inherits(result, CompetingRiskResult))
	})

	it("handles long format data with repeated observations", {
		skip_if_not_installed("cmprsk")

		set.seed(222)
		n <- 100
		visits <- 3

		subjects <- expand.grid(
			USUBJID = 1:n,
			AVISITN = 0:(visits - 1)
		)

		subjects$time <- (subjects$AVISITN + 1) * 30
		subjects$TRT01P <- rep(
			sample(c("A", "B"), n, replace = TRUE),
			each = visits
		)
		subjects$covariate <- rnorm(n * visits)
		subjects$event <- 0
		subjects$censored <- 1

		# Set some events
		subjects$event[sample.int((n * visits), 30)] <- 1
		subjects$censored[subjects$event == 0] <- 0

		result <- competing_risk_analysis(
			subjects,
			time_var = "time",
			event_var = "event",
			trt_var = "TRT01P",
			main_event = 1,
			competing_events = integer(0),
			covariates = "covariate"
		)

		expect_true(S7::S7_inherits(result, CompetingRiskResult))
	})

	it("handles missing data appropriately", {
		skip_if_not_installed("cmprsk")

		set.seed(333)
		n <- 100
		times <- rexp(n, 0.1)
		events <- sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.7, 0.2, 0.1))

		# Add some missing values
		times[sample.int(n, 10)] <- NA
		events[sample.int(n, 5)] <- NA

		data <- data.frame(
			time = times,
			event = events,
			TRT01P = sample(c("A", "B"), n, replace = TRUE)
		)

		result <- competing_risk_analysis(
			data,
			time_var = "time",
			event_var = "event",
			trt_var = "TRT01P",
			main_event = 1,
			competing_events = c(2)
		)

		expect_true(S7::S7_inherits(result, CompetingRiskResult))
	})

	it("fails if competing_events contains censor code 0", {
		skip_if_not_installed("cmprsk")
		data <- create_competing_risk_test_data()
		expect_error(
			competing_risk_analysis(data, "time", "event", "TRT01P", 1, c(0, 2)),
			"competing_events cannot contain censor code 0"
		)
	})

	it("handles crr failure gracefully", {
		skip_if_not_installed("cmprsk")
		# Data that will likely fail crr (constant time)
		data <- data.frame(
			time = rep(10, 10),
			event = rep(1, 10),
			TRT01P = rep("A", 10)
		)
		expect_warning(
			expect_error(
				competing_risk_analysis(data, "time", "event", "TRT01P", 1, integer(0)),
				"Fine-Gray model fitting failed"
			),
			"Only one treatment level found",
			fixed = TRUE
		)
	})
})

# =============================================================================
# plot_cif tests
# =============================================================================

describe("plot_cif", {
	it("returns a ClinicalPlot object", {
		skip_if_not_installed("cmprsk")
		data <- create_competing_risk_test_data()
		result <- competing_risk_analysis(
			data,
			"time",
			"event",
			"TRT01P",
			1,
			c(2, 3)
		)
		p <- plot_cif(result)
		expect_true(S7::S7_inherits(p, ClinicalPlot))
	})

	it("handles empty CIF data", {
		# Mock an empty result
		result <- CompetingRiskResult(
			main_event = 1L,
			competing_events = 2L,
			time_points = numeric(0),
			n_obs = 0L,
			n_events = numeric(0)
		)
		expect_warning(p <- plot_cif(result), "No CIF data available")
		expect_true(S7::S7_inherits(p, ClinicalPlot))
	})
})

# =============================================================================
# create_competing_risk_table tests
# =============================================================================

describe("create_competing_risk_table", {
	it("returns a ClinicalTable object", {
		skip_if_not_installed("cmprsk")
		data <- create_competing_risk_test_data()
		result <- competing_risk_analysis(
			data,
			"time",
			"event",
			"TRT01P",
			1,
			c(2, 3)
		)
		tab <- create_competing_risk_table(result)
		expect_true(S7::S7_inherits(tab, ClinicalTable))
	})

	it("errors if only one treatment level for Fine-Gray model", {
		skip_if_not_installed("cmprsk")
		# Single treatment arm - cannot fit Fine-Gray model
		data <- data.frame(time = 1:10, event = rep(c(0, 1), 5), TRT01P = "A")
		# This should error because Fine-Gray requires >1 treatment level
		suppressWarnings(
			expect_error(
				competing_risk_analysis(
					data,
					"time",
					"event",
					"TRT01P",
					1,
					integer(0)
				),
				"Fine-Gray model fitting failed"
			)
		)
	})
})

# =============================================================================
# CompetingRiskResult class tests
# =============================================================================

describe("CompetingRiskResult class", {
	it("has expected properties", {
		skip_if_not_installed("cmprsk")

		data <- create_competing_risk_test_data(n = 100, seed = 444)
		result <- competing_risk_analysis(
			data,
			time_var = "time",
			event_var = "event",
			trt_var = "TRT01P",
			main_event = 1,
			competing_events = c(2, 3)
		)

		# Check all expected properties exist
		expect_setequal(
			names(S7::props(result)),
			c(
				"model",
				"cif_main",
				"cif_competing",
				"cif_by_treatment",
				"treatment_comparison",
				"subhazard_ratio",
				"main_event",
				"competing_events",
				"time_points",
				"n_obs",
				"n_events",
				"metadata"
			)
		)
	})
})
