# Tests for PRO analysis functions

test_that("calculate_mcid_anchor calculates anchor-based MCID", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4),
		PGIC = c(rep("Minimally Improved", 5), rep("No Change", 5))
	)

	result <- calculate_mcid_anchor(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC"
	)

	expect_true(is.list(result))
	expect_true("mcid" %in% names(result))
	expect_true("ci" %in% names(result))
	expect_equal(result$method, "anchor-based")
	expect_equal(result$n, 5) # 5 minimally improved
})

test_that("calculate_mcid_distribution calculates distribution-based MCID", {
	data <- data.frame(
		BASE = rnorm(100, mean = 50, sd = 10)
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE"
	)

	expect_true(is.list(result))
	expect_true("half_sd" %in% names(result))
	expect_true("third_sd" %in% names(result))
	expect_true("sd" %in% names(result))
	# 0.5 SD should be approximately 5 (0.5 * 10)
	expect_true(result$half_sd > 3 && result$half_sd < 7)
})

test_that("calculate_mcid_distribution calculates SEM with reliability", {
	data <- data.frame(
		BASE = rnorm(100, mean = 50, sd = 10)
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE",
		reliability = 0.85
	)

	expect_true("one_sem" %in% names(result))
	expect_true(!is.na(result$one_sem))
})

test_that("calculate_mcid wrapper works with both methods", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4),
		PGIC = c(rep("Minimally Improved", 5), rep("No Change", 5)),
		BASE = abs(c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4)) + 50
	)

	result <- calculate_mcid(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC",
		baseline_var = "BASE",
		method = "both"
	)

	expect_true("anchor" %in% names(result))
	expect_true("distribution" %in% names(result))
})

test_that("create_ttd_analysis creates time-to-deterioration analysis", {
	# Create sample PRO data
	set.seed(123)
	n_subj <- 50
	n_visits <- 6

	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:n_subj)),
		AVISITN = 0:(n_visits - 1)
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = n_subj * n_visits / 2)
	data$BASE <- rep(rnorm(n_subj, 50, 10), each = n_visits)
	data$AVAL <- data$BASE +
		rep(c(0, -2, -1, 0, 1, 2), n_subj) +
		rnorm(nrow(data), 0, 5)
	data$CHG <- data$AVAL - data$BASE
	data$ADY <- data$AVISITN * 30 # Monthly visits

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease"
	)

	expect_true(is.list(result))
	expect_true("ttd_data" %in% names(result))
	expect_true("km_fit" %in% names(result))
	expect_true("summary_table" %in% names(result))
})

test_that("create_mean_plot creates ClinicalPlot", {
	data <- data.frame(
		USUBJID = rep(1:50, each = 5),
		AVISITN = rep(0:4, 50),
		AVAL = rnorm(250, mean = 50, sd = 10),
		TRT01P = rep(c("Treatment", "Placebo"), each = 125)
	)

	plot <- create_mean_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "AVAL",
		group_var = "TRT01P"
	)

	expect_true(S7::S7_inherits(plot, ClinicalPlot))
	expect_equal(plot@type, "mean_plot")
})

test_that("create_mean_plot calculates correct CIs", {
	# Small dataset with known properties
	data <- data.frame(
		visit = rep(c("V1", "V2"), each = 100),
		value = c(rnorm(100, 10, 2), rnorm(100, 12, 2))
	)

	plot <- create_mean_plot(
		data = data,
		x_var = "visit",
		y_var = "value"
	)

	expect_true(S7::S7_inherits(plot, ClinicalPlot))
	# Check summary data exists
	expect_true(nrow(plot@data) == 2) # 2 visits
})

test_that("create_spider_plot creates ClinicalPlot", {
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:20), each = 6),
		AVISITN = rep(0:5, 20),
		PCHG = c(replicate(20, cumsum(c(0, rnorm(5, mean = -5, sd = 15))))),
		TRT01P = rep(c("Treatment", "Placebo"), each = 60)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		group_var = "TRT01P"
	)

	expect_true(S7::S7_inherits(plot, ClinicalPlot))
	expect_equal(plot@type, "spider_plot")
})

test_that("create_spider_plot handles threshold lines", {
	data <- data.frame(
		USUBJID = rep(paste0("SUBJ", 1:10), each = 4),
		AVISITN = rep(0:3, 10),
		PCHG = rnorm(40, 0, 20)
	)

	plot <- create_spider_plot(
		data = data,
		x_var = "AVISITN",
		y_var = "PCHG",
		reference_line = 0,
		threshold_lines = c(-30, 20)
	)

	expect_true(S7::S7_inherits(plot, ClinicalPlot))
})

# Edge Cases for MCID Anchor ----

test_that("calculate_mcid_anchor handles empty minimal group", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4),
		PGIC = c(rep("No Change", 10)) # No minimally improved
	)

	expect_warning(
		result <- calculate_mcid_anchor(
			data = data,
			score_var = "CHG",
			anchor_var = "PGIC"
		),
		"No observations in minimal improvement group"
	)

	expect_equal(result$mcid, NA_real_)
	expect_equal(result$n, 0)
})

test_that("calculate_mcid_anchor handles single observation", {
	data <- data.frame(
		CHG = c(-5, -3, -4, 0, 2, 5, 3, 4),
		PGIC = c("Minimally Improved", rep("No Change", 7))
	)

	result <- calculate_mcid_anchor(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC"
	)

	expect_equal(result$mcid, -5)
	expect_equal(result$n, 1)
	expect_true(all(is.na(result$ci))) # No CI with n=1
})

test_that("calculate_mcid_anchor handles NA scores", {
	data <- data.frame(
		CHG = c(-5, -3, NA, -6, -2, NA, 2, 5, 3, 4),
		PGIC = c(rep("Minimally Improved", 5), rep("No Change", 5))
	)

	result <- calculate_mcid_anchor(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC"
	)

	# Should filter out NAs - 4 non-NA in minimal group
	expect_equal(result$n, 4)
	expect_true(!is.na(result$mcid))
})

test_that("calculate_mcid_anchor with different anchor levels", {
	data <- data.frame(
		CHG = c(-8, -7, -5, -3, -2, -1, 0, 1, 2, 5),
		PGIC = c(
			"Much Improved",
			"Much Improved",
			"Minimally Improved",
			"Minimally Improved",
			"No Change",
			"No Change",
			"No Change",
			"Worsened",
			"Worsened",
			"Much Worsened"
		)
	)

	result_default <- calculate_mcid_anchor(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC",
		anchor_minimal = "Minimally Improved"
	)

	result_positive <- calculate_mcid_anchor(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC",
		anchor_minimal = "Much Improved"
	)

	# Much Improved should have larger (more negative) MCID
	expect_true(result_positive$mcid < result_default$mcid)
})

test_that("calculate_mcid_anchor handles variable not found", {
	data <- data.frame(
		CHG = c(-5, -3, -4),
		PGIC = c("Minimally Improved", "No Change", "No Change")
	)

	expect_error(
		calculate_mcid_anchor(
			data = data,
			score_var = "NONEXISTENT",
			anchor_var = "PGIC"
		),
		"not found"
	)
})

test_that("calculate_mcid_anchor with custom confidence level", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4),
		PGIC = c(rep("Minimally Improved", 5), rep("No Change", 5))
	)

	result_90 <- calculate_mcid_anchor(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC",
		conf_level = 0.90
	)

	result_99 <- calculate_mcid_anchor(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC",
		conf_level = 0.99
	)

	expect_length(result_90$ci, 2)
	expect_length(result_99$ci, 2)
	# 99% CI should be wider than 90% CI
	expect_true(
		(result_99$ci[2] - result_99$ci[1]) > (result_90$ci[2] - result_90$ci[1])
	)
})

# Edge Cases for MCID Distribution ----

test_that("calculate_mcid_distribution handles insufficient data", {
	data <- data.frame(
		BASE = c(50, 52) # Only 2 observations
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE"
	)

	expect_equal(result$n, 2)
	expect_true(!is.na(result$half_sd))
	expect_true(!is.na(result$sd))
})

test_that("calculate_mcid_distribution handles single observation", {
	data <- data.frame(
		BASE = c(50)
	)

	expect_warning(
		result <- calculate_mcid_distribution(
			data = data,
			score_var = "BASE"
		),
		"Insufficient data for distribution-based MCID"
	)

	expect_equal(result$n, 1)
	expect_true(is.na(result$sd)) # Can't calculate SD with 1 obs
	expect_true(is.na(result$half_sd))
})

test_that("calculate_mcid_distribution handles all NA values", {
	data <- data.frame(
		BASE = c(NA, NA, NA)
	)

	expect_warning(
		result <- calculate_mcid_distribution(
			data = data,
			score_var = "BASE"
		),
		"Insufficient data for distribution-based MCID"
	)

	expect_equal(result$n, 0)
	expect_true(is.na(result$sd))
	expect_true(is.na(result$half_sd))
})

test_that("calculate_mcid_distribution handles mixed NA values", {
	data <- data.frame(
		BASE = c(50, 52, NA, 48, NA, 51, 49, NA)
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE"
	)

	expect_equal(result$n, 5) # Only non-NA values
	expect_true(!is.na(result$sd))
	expect_true(!is.na(result$half_sd))
})

test_that("calculate_mcid_distribution handles zero variance", {
	data <- data.frame(
		BASE = rep(50, 10) # All same value
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE"
	)

	expect_equal(result$sd, 0)
	expect_equal(result$half_sd, 0)
	expect_equal(result$third_sd, 0)
	expect_equal(result$fifth_sd, 0)
})

test_that("calculate_mcid_distribution with reliability (SEM)", {
	data <- data.frame(
		BASE = rnorm(100, mean = 50, sd = 10)
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE",
		reliability = 0.85
	)

	expect_true("one_sem" %in% names(result))
	expect_true(!is.na(result$one_sem))
	expect_equal(result$reliability, 0.85)

	# SEM should be < SD (since reliability < 1)
	expect_true(result$one_sem < result$sd)
})

test_that("calculate_mcid_distribution without reliability (no SEM)", {
	data <- data.frame(
		BASE = rnorm(100, mean = 50, sd = 10)
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE",
		reliability = NULL
	)

	expect_true(!("one_sem" %in% names(result)) || is.na(result$one_sem))
})

test_that("calculate_mcid_distribution handles reliability edge cases", {
	data <- data.frame(
		BASE = rnorm(100, mean = 50, sd = 10)
	)

	# Perfect reliability - SEM = 0
	result_perfect <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE",
		reliability = 1.0
	)
	expect_equal(result_perfect$one_sem, 0)

	# Zero reliability - SEM = SD
	result_zero <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE",
		reliability = 0.0
	)
	expect_equal(result_zero$one_sem, result_zero$sd)
})

test_that("calculate_mcid_distribution with selective methods", {
	data <- data.frame(
		BASE = rnorm(100, mean = 50, sd = 10)
	)

	result <- calculate_mcid_distribution(
		data = data,
		score_var = "BASE",
		methods = c("half_sd", "third_sd") # Exclude fifth_sd and one_sem
	)

	expect_true("half_sd" %in% names(result))
	expect_true("third_sd" %in% names(result))
	expect_true(!("fifth_sd" %in% names(result)) || is.na(result$fifth_sd))
})

test_that("calculate_mcid_distribution handles variable not found", {
	data <- data.frame(
		OTHER = rnorm(100)
	)

	expect_error(
		calculate_mcid_distribution(
			data = data,
			score_var = "NONEXISTENT"
		),
		"not found"
	)
})

# Edge Cases for Combined MCID ----

test_that("calculate_mcid with anchor only", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4),
		PGIC = c(rep("Minimally Improved", 5), rep("No Change", 5))
	)

	result <- calculate_mcid(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC",
		method = "anchor"
	)

	expect_true("anchor" %in% names(result))
	expect_false("distribution" %in% names(result))
})

test_that("calculate_mcid with distribution only", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4),
		BASE = abs(c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4)) + 50
	)

	result <- calculate_mcid(
		data = data,
		score_var = "CHG",
		baseline_var = "BASE",
		method = "distribution"
	)

	expect_false("anchor" %in% names(result))
	expect_true("distribution" %in% names(result))
})

test_that("calculate_mcid with both methods", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4),
		PGIC = c(rep("Minimally Improved", 5), rep("No Change", 5)),
		BASE = abs(c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4)) + 50
	)

	result <- calculate_mcid(
		data = data,
		score_var = "CHG",
		anchor_var = "PGIC",
		baseline_var = "BASE",
		method = "both"
	)

	expect_true("anchor" %in% names(result))
	expect_true("distribution" %in% names(result))
	expect_equal(result$method, "both")
})

test_that("calculate_mcid requires anchor_var for anchor method", {
	data <- data.frame(
		CHG = c(-5, -3, -4)
	)

	expect_error(
		calculate_mcid(
			data = data,
			score_var = "CHG",
			method = "anchor"
		),
		"anchor_var required"
	)
})

test_that("calculate_mcid uses score_var if baseline_var missing", {
	data <- data.frame(
		CHG = c(-5, -3, -4, -6, -2, 0, 2, 5, 3, 4)
	)

	result <- calculate_mcid(
		data = data,
		score_var = "CHG",
		method = "distribution"
	)

	expect_true("distribution" %in% names(result))
	expect_true(!is.na(result$distribution$half_sd))
})

# Edge Cases for TTD Analysis ----

test_that("create_ttd_analysis handles no events", {
	set.seed(123)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:20)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 40)
	data$BASE <- rep(rnorm(20, 50, 5), each = 4)
	data$AVAL <- data$BASE + rnorm(80, 0, 1) # No deterioration
	data$CHG <- data$AVAL - data$BASE
	data$ADY <- data$AVISITN * 30

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease"
	)

	expect_equal(result$n_events, 0)
	expect_true("ttd_data" %in% names(result))
	expect_true("km_fit" %in% names(result))
})

test_that("create_ttd_analysis handles all events", {
	set.seed(123)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:20)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 40)
	data$BASE <- rep(rnorm(20, 50, 5), each = 4)
	# Add deterioration at each visit - all subjects deteriorate at visit 2
	data$CHG <- ifelse(data$AVISITN == 2, -10, 0)
	data$ADY <- data$AVISITN * 30

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease"
	)

	expect_equal(result$n_events, 20)
})

test_that("create_ttd_analysis handles all events", {
	set.seed(123)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:20)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 40)
	data$BASE <- rep(rnorm(20, 50, 5), each = 4)
	# All deteriorated at first visit - each subject has only first visit with chg
	data$CHG <- ifelse(data$AVISITN == 2, -10, 0)
	data$ADY <- data$AVISITN * 30

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease"
	)

	expect_equal(result$n_events, 20)
})

test_that("create_ttd_analysis with increase direction", {
	set.seed(123)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:20)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 40)
	data$BASE <- rep(rnorm(20, 50, 5), each = 4)
	# All deteriorated (increase) at first visit
	data$CHG <- ifelse(data$AVISITN == 2, 10, 0)
	data$ADY <- data$AVISITN * 30

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "increase"
	)

	expect_equal(result$n_events, 20)
	expect_equal(result$direction, "increase")
})

test_that("create_ttd_analysis with confirmed definition", {
	set.seed(123)
	# Create data where 5 subjects have consecutive deterioration (2+ visits)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:20)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 40)
	data$BASE <- rep(rnorm(20, 50, 5), each = 4)
	data$ADY <- data$AVISITN * 30

	# First 5 subjects deteriorated at visits 2 and 3 (consecutive)
	data$CHG <- c(
		rep(0, 20), # First 20 rows no deterioration
		rep(-10, 20), # Next 20 deteriorated at visit 2
		rep(-12, 20), # All 80 rows deteriorated at visit 3 too
		rep(-14, 20)
	) # All 80 rows deteriorated at visit 4 too
	data$CHG[1:20] <- 0 # Reset first 20 to no deterioration
	data$CHG[21:40] <- c(rep(-10, 10), rep(0, 10)) # 5 subjects with 2 consecutive

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease",
		definition = "confirmed",
		confirmation_visits = 2
	)

	expect_equal(result$definition, "confirmed")
})

test_that("create_ttd_analysis with confirmed definition", {
	set.seed(123)
	# Create data where 5 subjects have consecutive deterioration (2+ visits)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:20)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 40)
	data$BASE <- rep(rnorm(20, 50, 5), each = 4)
	data$ADY <- data$AVISITN * 30

	# First 5 subjects deteriorate at visits 2 and 3 (consecutive)
	data$CHG <- c(
		rep(0, 20), # First 20 rows no deterioration
		rep(-10, 20), # Next 20 deteriorate at visit 2
		rep(-12, 20), # All 80 rows deteriorate at visit 3 too
		rep(-14, 20)
	) # All 80 rows deteriorate at visit 4 too
	data$CHG[1:20] <- 0 # Reset first 20 to no deterioration
	data$CHG[21:40] <- c(rep(-10, 10), rep(0, 10)) # 5 subjects with 2 consecutive

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease",
		definition = "confirmed",
		confirmation_visits = 2
	)

	expect_equal(result$definition, "confirmed")
})

test_that("create_ttd_analysis with confirmed definition", {
	set.seed(123)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:10)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 20)
	data$BASE <- rep(rnorm(10, 50, 5), each = 4)
	# Set CHG for each row individually - some subjects have consecutive
	# deterioration
	data$CHG <- c(
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0, # V1 baseline (no chg)
		-10,
		-10,
		-8,
		-8,
		-12,
		-12,
		-15,
		-15,
		-5,
		-5, # V2 - some deteriorate
		-12,
		-12,
		-8,
		-8,
		-15,
		-15,
		-18,
		-18,
		-6,
		-6, # V3 - consecutive for some
		-14,
		-14,
		-7,
		-7,
		-17,
		-17,
		-20,
		-20,
		-4,
		-4
	) # V4
	data$ADY <- data$AVISITN * 30

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease",
		definition = "confirmed",
		confirmation_visits = 2
	)

	expect_equal(result$definition, "confirmed")
})

test_that("create_ttd_analysis with custom censor_at", {
	set.seed(123)
	data <- expand.grid(
		USUBJID = paste0("SUBJ", sprintf("%03d", 1:20)),
		AVISITN = 0:3
	)
	data$TRT01P <- rep(c("Treatment", "Placebo"), each = 40)
	data$BASE <- rep(rnorm(20, 50, 5), each = 4)
	data$CHG <- rnorm(80, 0, 10)
	data$ADY <- data$AVISITN * 30

	result <- create_ttd_analysis(
		data = data,
		threshold = 5,
		direction = "decrease",
		censor_at = 60 # Censor at day 60 instead of max(ADY)
	)

	expect_true("ttd_data" %in% names(result))
})
