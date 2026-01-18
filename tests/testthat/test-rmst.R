# Tests for RMST functions (R/rmst.R)
# Issue #152: Restricted mean survival time (RMST)

# =============================================================================
# rmst_analysis tests
# =============================================================================

describe("rmst_analysis", {
	it("returns an RMSTResult object", {
		skip_if_not_installed("survRM2")

		data <- create_rmst_test_data(n = 100, seed = 123)
		result <- rmst_analysis(
			data,
			time_var = "time",
			event_var = "status",
			trt_var = "TRT01P",
			tau = 12
		)

		expect_true(S7::S7_inherits(result, RMSTResult))
	})

	it("validates input data", {
		skip_if_not_installed("survRM2")

		expect_error(
			rmst_analysis("not_dataframe"),
			"data frame"
		)
	})

	it("validates required variables exist", {
		skip_if_not_installed("survRM2")

		data <- create_rmst_test_data()

		expect_error(
			rmst_analysis(
				data,
				time_var = "MISSING_VAR"
			),
			"MISSING_VAR"
		)
	})

	it("calculates RMST and confidence intervals", {
		skip_if_not_installed("survRM2")

		data <- create_rmst_test_data(n = 100, seed = 456)
		result <- rmst_analysis(
			data,
			time_var = "time",
			event_var = "status",
			trt_var = "TRT01P",
			tau = 12
		)

		expect_true("rmst_by_group" %in% names(result))
		expect_true("rmst_difference" %in% names(result))
		expect_true("ci" %in% names(result))
	})

	it("compares treatment groups", {
		skip_if_not_installed("survRM2")

		data <- create_rmst_test_data(n = 100, seed = 789)
		result <- rmst_analysis(
			data,
			time_var = "time",
			event_var = "status",
			trt_var = "TRT01P",
			tau = 12
		)

		expect_true("treatment_comparison" %in% names(result))
		expect_true(nrow(result@treatment_comparison) > 0)
	})

	it("handles different time restrictions", {
		skip_if_not_installed("survRM2")

		data <- create_rmst_test_data(n = 100, seed = 111)

		# Short time restriction
		result_short <- rmst_analysis(
			data,
			time_var = "time",
			event_var = "status",
			trt_var = "TRT01P",
			tau = 6
		)

		# Long time restriction
		result_long <- rmst_analysis(
			data,
			time_var = "time",
			event_var = "status",
			trt_var = "TRT01P",
			tau = 24
		)

		expect_true(S7::S7_inherits(result_short, RMSTResult))
		expect_true(S7::S7_inherits(result_long, RMSTResult))
	})

	it("handles missing data appropriately", {
		skip_if_not_installed("survRM2")

		set.seed(222)
		n <- 100
		times <- rexp(n, 0.1)
		status <- sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7))

		# Add some missing values
		times[sample.int(n, 10)] <- NA
		status[sample.int(n, 5)] <- NA

		data <- data.frame(
			time = times,
			status = status,
			TRT01P = sample(c("A", "B"), n, replace = TRUE)
		)

		result <- rmst_analysis(
			data,
			time_var = "time",
			event_var = "status",
			trt_var = "TRT01P",
			tau = 12
		)

		expect_true(S7::S7_inherits(result, RMSTResult))
	})
})

# =============================================================================
# RMSTResult class tests
# =============================================================================

describe("RMSTResult class", {
	it("has expected properties", {
		skip_if_not_installed("survRM2")

		data <- create_rmst_test_data(n = 100, seed = 333)
		result <- rmst_analysis(
			data,
			time_var = "time",
			event_var = "status",
			trt_var = "TRT01P",
			tau = 12
		)

		# Check all expected properties exist
		expect_true("rmst_by_group" %in% names(S7::props(result)))
		expect_true("rmst_difference" %in% names(S7::props(result)))
		expect_true("ci" %in% names(S7::props(result)))
		expect_true("p_value" %in% names(S7::props(result)))
		expect_true("tau" %in% names(S7::props(result)))
		expect_true("treatment_comparison" %in% names(S7::props(result)))
	})
})

# =============================================================================
# Helper function for test data
# =============================================================================

create_rmst_test_data <- function(n = 100, seed = 123) {
	set.seed(seed)

	# Generate survival times with treatment effect
	trt_grp <- sample(c("A", "B"), n, replace = TRUE)

	# Treatment A has better survival (lower hazard)
	times_A <- rexp(sum(trt_grp == "A"), 0.1) # Lower hazard
	times_B <- rexp(sum(trt_grp == "B"), 0.15) # Higher hazard

	times <- c(times_A, times_B)

	# Generate censoring times
	censor_times <- rexp(n, 0.05)

	# Determine observed times and status
	observed_times <- pmin(times, censor_times)
	status <- ifelse(times <= censor_times, 1, 0) # 1=event, 0=censored

	data.frame(
		time = observed_times,
		status = status,
		TRT01P = trt_grp,
		stringsAsFactors = FALSE
	)
}
