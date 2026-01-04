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
		cumsum(rep(c(0, -2, -1, 0, 1, 2), n_subj)) +
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
