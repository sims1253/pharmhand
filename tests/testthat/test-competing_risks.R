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

	it("supports time-varying covariates", {
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
			competing_events = c(0),
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
		expect_true("model" %in% names(S7::props(result)))
		expect_true("cif_main" %in% names(S7::props(result)))
		expect_true("cif_competing" %in% names(S7::props(result)))
		expect_true("cif_by_treatment" %in% names(S7::props(result)))
		expect_true("treatment_comparison" %in% names(S7::props(result)))
		expect_true("subhazard_ratio" %in% names(S7::props(result)))
	})
})

# =============================================================================
# Helper function for test data
# =============================================================================

create_competing_risk_test_data <- function(n = 100, seed = 123) {
	set.seed(seed)

	# Generate event times
	times <- rexp(n, 0.1)

	# Generate events with competing risks
	# Main event (1): 20%, Competing events (2,3): 10% each, Censored (0): 60%
	events <- sample(
		c(0, 1, 2, 3),
		n,
		replace = TRUE,
		prob = c(0.6, 0.2, 0.1, 0.1)
	)

	# Treatment effect
	trt <- sample(c("A", "B"), n, replace = TRUE)
	effect_A <- 0.8 # Lower hazard for main event
	effect_B <- 1.2 # Higher hazard for main event

	# Adjust event probabilities based on treatment
	for (i in 1:n) {
		if (trt[i] == "A") {
			if (runif(1) < 0.2) events[i] <- 1 # Main event
		} else {
			if (runif(1) < 0.25) events[i] <- 1 # Main event
		}
	}

	data.frame(
		time = times,
		event = events,
		TRT01P = trt,
		stringsAsFactors = FALSE
	)
}
