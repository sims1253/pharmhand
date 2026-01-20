#' Test Data Fixtures for pharmhand
#'
#' Helper functions to create mock data for testing clinical study functions.

# ==============================================================================
# Shared Meta-Analysis Test Data
# ==============================================================================

# Standard meta-analysis test vectors for hazard ratio (HR) analyses
.meta_yi_hr <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
.meta_sei_hr <- c(0.12, 0.15, 0.18, 0.14, 0.11)

# Standard meta-analysis test vectors for mean difference (MD) analyses
.meta_yi_md <- c(0.5, 0.8, 0.3, 1.0, 0.6)
.meta_sei_md <- c(0.2, 0.2, 0.2, 0.2, 0.2)

# Smaller dataset for quick tests (3 studies)
.meta_yi_hr_small <- log(c(0.75, 0.82, 0.68))
.meta_sei_hr_small <- c(0.12, 0.15, 0.18)

# Variable effect sizes for heterogeneity tests
.meta_yi_md_variable <- c(0.5, 0.8, 0.3, 1.0, 0.6)
.meta_sei_md_variable <- c(0.2, 0.2, 0.2, 0.2, 0.2)

# Different effect measures for testing
.meta_yi_or <- c(0.3, 0.5, 0.7, 0.4, 0.6)
.meta_sei_or <- c(0.15, 0.2, 0.18, 0.22, 0.17)

# Standard NMA test data (3-arm network)
.nma_test_data_3arm <- data.frame(
	study = c("S1", "S2", "S3"),
	treat1 = c("A", "B", "A"),
	treat2 = c("B", "C", "C"),
	effect = log(c(0.75, 0.90, 0.80)),
	se = c(0.12, 0.15, 0.18),
	stringsAsFactors = FALSE
)

# Incomplete network for edge case testing
.nma_test_data_incomplete <- data.frame(
	study = c("S1", "S2", "S3", "S4"),
	treat1 = c("A", "B", "A", "D"),
	treat2 = c("B", "C", "C", "E"),
	effect = log(c(0.75, 0.90, 0.80, 0.95)),
	se = c(0.12, 0.15, 0.18, 0.20),
	stringsAsFactors = FALSE
)

# ==============================================================================
# Shared Efficacy Test Data
# ==============================================================================

fixture_adlb_lab_summary <- function() {
	data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAM = c("Alanine Aminotransferase", "Alanine Aminotransferase"),
		PARAMCD = c("ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24"),
		AVAL = c(20, 30),
		stringsAsFactors = FALSE
	)
}

fixture_adlb_lab_shift <- function() {
	data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		PARAMCD = c("ALT", "ALT", "ALT"),
		AVISIT = c("Week 24", "Week 24", "Week 24"),
		BNRIND = c("NORMAL", "NORMAL", "HIGH"),
		ANRIND = c("NORMAL", "HIGH", "HIGH"),
		stringsAsFactors = FALSE
	)
}

fixture_advs_cfb <- function() {
	data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAM = c("Systolic Blood Pressure", "Systolic Blood Pressure"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("End of Treatment", "End of Treatment"),
		AVAL = c(120, 140),
		CHG = c(-5, -10),
		stringsAsFactors = FALSE
	)
}

fixture_advs_vs_by_visit <- function() {
	data.frame(
		USUBJID = c("01", "02"),
		TRT01P = c("A", "B"),
		PARAMCD = c("SYSBP", "SYSBP"),
		AVISIT = c("Baseline", "Week 2"),
		AVAL = c(120, 140),
		stringsAsFactors = FALSE
	)
}

# ==============================================================================
# Shared Test Configuration Data
# ==============================================================================

# Cached shared test configuration
.shared_test_config <- NULL

#' Get shared test configuration (cached for performance)
#'
#' @return Test configuration list
get_shared_test_config <- function() {
	if (is.null(.shared_test_config)) {
		.shared_test_config <<- create_test_config()
	}
	.shared_test_config
}

# ==============================================================================
# Shared Mock Clinical Trial Data (Cached for Performance)
# ==============================================================================

# Cached shared datasets for common test scenarios
.shared_adsl_10 <- NULL
.shared_adae_10 <- NULL
.shared_adsl_20 <- NULL

#' Get shared ADSL dataset (cached for performance)
#'
#' @param n Number of subjects. Currently supports n=10 (cached) or custom sizes
#' @return Mock ADSL data frame
get_shared_adsl <- function(n = 10) {
	if (n == 10 && is.null(.shared_adsl_10)) {
		.shared_adsl_10 <<- create_mock_adsl(n = 10)
	}
	if (n == 10) {
		return(.shared_adsl_10)
	}

	if (n == 20 && is.null(.shared_adsl_20)) {
		.shared_adsl_20 <<- create_mock_adsl(n = 20)
	}
	if (n == 20) {
		return(.shared_adsl_20)
	}

	# For non-standard sizes, create fresh
	create_mock_adsl(n = n)
}

#' Get shared ADAE dataset (cached for performance)
#'
#' @param n Number of subjects. Currently supports n=10 (cached) or custom sizes
#' @return Mock ADAE data frame
get_shared_adae <- function(n = 10) {
	if (n == 10 && is.null(.shared_adae_10)) {
		set.seed(123) # Ensure reproducibility
		.shared_adae_10 <<- create_mock_adae(n = 10)
	}
	if (n == 10) {
		return(.shared_adae_10)
	}

	# For non-standard sizes, create fresh
	set.seed(123)
	create_mock_adae(n = n)
}

# ==============================================================================
# Mock Clinical Trial Data Functions
# ==============================================================================

#' Create mock ADSL data
#'
#' @param n Integer specifying number of subjects. Default: 10
#' @return A data frame with mock ADSL data
create_mock_adsl <- function(n = 10) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		SUBJID = sprintf("%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		TRT01PN = rep(c(0, 1), length.out = n),
		AGE = round(runif(n, 25, 75)),
		SEX = sample(c("M", "F"), n, replace = TRUE),
		RACE = sample(c("White", "Asian", "Black", "Other"), n, replace = TRUE),
		ETHNIC = sample(c("Hispanic", "Not Hispanic"), n, replace = TRUE),
		SAFFL = rep("Y", n),
		FASFL = rep("Y", n),
		ITTFL = rep("Y", n),
		PPSFL = sample(c("Y", "N"), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create mock ADAE data
#'
#' @param n Integer specifying number of subjects. Default: 10
#' @return A data frame with mock ADAE data
create_mock_adae <- function(n = 10) {
	subjects <- sprintf("SUBJ%03d", 1:n)
	n_ae <- sample.int(3, n, replace = TRUE)

	data_list <- lapply(1:n, function(i) {
		n_events <- n_ae[i]
		data.frame(
			USUBJID = rep(subjects[i], n_events),
			TRT01P = rep(ifelse(i <= n / 2, "Placebo", "Active"), n_events),
			AEBODSYS = sample(
				c("Nervous system", "Gastrointestinal", "Cardiac", "Respiratory"),
				n_events,
				replace = TRUE
			),
			AEDECOD = sample(
				c("Headache", "Nausea", "Dizziness", "Palpitations", "Cough"),
				n_events,
				replace = TRUE
			),
			AESTDTC = sample(
				c("2023-01-01", "2023-01-15", "2023-02-01"),
				n_events,
				replace = TRUE
			),
			AESEV = sample(c("Mild", "Moderate", "Severe"), n_events, replace = TRUE),
			stringsAsFactors = FALSE
		)
	})

	do.call(rbind, data_list)
}

#' Create mock ADRS data
#'
#' @param n Integer specifying number of subjects. Default: 10
#' @return A data frame with mock ADRS data
create_mock_adrs <- function(n = 10) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		PARAM = rep("Tumor Size", n),
		PARAMCD = rep("TUMSZ", n),
		AVAL = round(runif(n, 1, 10), 1),
		AVALC = sample(c("Decreased", "Stable", "Increased"), n, replace = TRUE),
		AVISIT = sample(c("Baseline", "Week 4", "Week 8"), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create mock ADTTE data
#'
#' @param n Integer specifying number of subjects. Default: 10
#' @return A data frame with mock ADTTE data
create_mock_adtte <- function(n = 10) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		PARAM = rep("Overall Survival", n),
		PARAMCD = rep("OS", n),
		AVAL = round(runif(n, 5, 50), 1),
		CNSR = sample(c(0, 1), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create mock demographics data
#'
#' @param n Integer specifying number of subjects. Default: 20
#' @return A data frame with mock demographics data
create_mock_demographics <- function(n = 20) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		AGE = round(runif(n, 30, 70)),
		SEX = sample(c("M", "F"), n, replace = TRUE),
		RACE = sample(c("White", "Asian", "Black", "Other"), n, replace = TRUE),
		ETHNIC = sample(c("Hispanic", "Not Hispanic"), n, replace = TRUE),
		WEIGHT = round(runif(n, 50, 100), 1),
		HEIGHT = round(runif(n, 150, 190), 1),
		stringsAsFactors = FALSE
	)
}

#' Create mock efficacy data
#'
#' @param n Integer specifying number of subjects. Default: 20
#' @return A data frame with mock efficacy data
create_mock_efficacy <- function(n = 20) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		AVAL = round(runif(n, 1, 10), 1),
		PARAM = "Tumor Size",
		RESP = sample(c(0, 1), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create mock survival data
#'
#' @param n Integer specifying number of subjects. Default: 20
#' @return A data frame with mock survival data
create_mock_survival <- function(n = 20) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		time = round(runif(n, 5, 50), 1),
		status = sample(c(0, 1), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create mock waterfall plot data
#'
#' @param n Integer specifying number of subjects. Default: 15
#' @return A data frame with mock waterfall plot data
create_mock_waterfall <- function(n = 15) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT = rep(c("Active", "Placebo"), length.out = n),
		PCTCHG = round(runif(n, -60, 30), 1),
		stringsAsFactors = FALSE
	)
}

#' Create mock spider plot data
#'
#' @param n Integer specifying number of subjects. Default: 10
#' @return A data frame with mock spider plot data
create_mock_spider <- function(n = 10) {
	subjects <- sprintf("SUBJ%03d", 1:n)
	visits <- c("Baseline", "Week 4", "Week 8", "Week 12")

	data_list <- lapply(subjects, function(subj) {
		data.frame(
			USUBJID = rep(subj, length(visits)),
			AVISIT = visits,
			AVAL = round(runif(length(visits), 50, 150), 1),
			TRT = rep(sample(c("Active", "Placebo"), 1), length(visits)),
			stringsAsFactors = FALSE
		)
	})

	do.call(rbind, data_list)
}

#' Create mock forest plot data
#'
#' @return A data frame with mock forest plot data
create_mock_forest <- function() {
	data.frame(
		label = c(
			"Age < 65",
			"Age >= 65",
			"Male",
			"Female",
			"White",
			"Non-White",
			"Overall"
		),
		estimate = c(0.5, 0.8, 0.6, 0.7, 0.5, 0.9, 0.65),
		lower = c(0.2, 0.5, 0.3, 0.4, 0.2, 0.6, 0.45),
		upper = c(0.8, 1.1, 0.9, 1.0, 0.8, 1.2, 0.85),
		pval = c(0.05, 0.02, 0.03, 0.01, 0.04, 0.01, 0.01),
		stringsAsFactors = FALSE
	)
}

#' Create mock population data
#'
#' @param n Integer specifying number of subjects. Default: 20
#' @return A data frame with mock population data
create_mock_population <- function(n = 20) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		SAF = rep("Y", n),
		FAS = sample(c("Y", "N"), n, replace = TRUE),
		PPS = sample(c("Y", "N"), n, replace = TRUE),
		AGE = round(runif(n, 30, 70)),
		stringsAsFactors = FALSE
	)
}

#' Create mock subgroup data
#'
#' @param n Integer specifying number of subjects. Default: 20
#' @return A data frame with mock subgroup data
create_mock_subgroup <- function(n = 20) {
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), length.out = n),
		AGE = round(runif(n, 30, 70)),
		SEX = sample(c("M", "F"), n, replace = TRUE),
		RACE = sample(c("White", "Asian", "Black", "Other"), n, replace = TRUE),
		AGEGR1 = sample(c("<65", ">=65"), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create simple test data frame
#'
#' @param n Integer specifying number of rows. Default: 10
#' @return A simple data frame for testing
create_simple_test_data <- function(n = 10) {
	data.frame(
		ID = 1:n,
		group = rep(c("A", "B"), length.out = n),
		value = round(runif(n, 1, 100), 1),
		category = sample(c("X", "Y", "Z"), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create test data for statistical tests
#'
#' @return A data frame suitable for statistical testing
create_statistical_test_data <- function() {
	data.frame(
		group = c(rep("A", 20), rep("B", 20)),
		value = c(rnorm(20, mean = 50, sd = 10), rnorm(20, mean = 55, sd = 10)),
		response = c(
			sample(c(0, 1), 20, replace = TRUE),
			sample(c(0, 1), 20, replace = TRUE)
		),
		stringsAsFactors = FALSE
	)
}

#' Create mock ADVS data for vital signs testing
#'
#' @param n Integer specifying number of subjects. Default: 20
#' @return A data frame with mock ADVS data
create_mock_advs <- function(n = 20) {
	set.seed(123)
	subjects <- sprintf("SUBJ%03d", 1:n)
	visits <- c("Baseline", "Week 4", "Week 8", "End of Treatment")

	data_list <- lapply(subjects, function(subj) {
		trt <- if (as.integer(gsub("SUBJ", "", subj, fixed = TRUE)) <= n / 2) {
			"Placebo"
		} else {
			"Active"
		}
		base_val <- runif(1, 110, 140)
		data.frame(
			USUBJID = rep(subj, length(visits)),
			TRT01P = rep(trt, length(visits)),
			PARAMCD = rep("SYSBP", length(visits)),
			PARAM = rep("Systolic Blood Pressure", length(visits)),
			AVISIT = visits,
			AVAL = round(base_val + rnorm(length(visits), 0, 5), 1),
			BASE = rep(round(base_val, 1), length(visits)),
			CHG = c(0, round(rnorm(length(visits) - 1, -3, 4), 1)),
			stringsAsFactors = FALSE
		)
	})

	do.call(rbind, data_list)
}

#' Create mock TTE data for subgroup analysis
#'
#' @param n Integer specifying number of subjects. Default: 40
#' @return A data frame with mock TTE data including subgroup variables
create_mock_tte_subgroup <- function(n = 40) {
	if (n %% 2 != 0) {
		stop("n must be even for balanced treatment groups")
	}
	set.seed(123)
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), each = n / 2),
		AVAL = c(rexp(n / 2, 0.05), rexp(n / 2, 0.03)),
		CNSR = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)),
		SEX = rep(c("M", "F"), n / 2),
		AGEGR1 = sample(c("<65", ">=65"), n, replace = TRUE),
		RACE = sample(c("White", "Black", "Asian"), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create mock forest plot TTE data
#'
#' @param n Integer specifying number of subjects per arm. Default: 30
#' @return A data frame suitable for forest plot generation from TTE data
create_mock_forest_tte <- function(n = 30) {
	if (n %% 2 != 0) {
		stop("n must be even for balanced treatment groups")
	}
	set.seed(123)
	data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:(n * 2)),
		TRT01P = rep(c("Placebo", "Active"), each = n),
		AVAL = c(rexp(n, 0.05), rexp(n, 0.035)),
		CNSR = sample(0:1, n * 2, replace = TRUE, prob = c(0.6, 0.4)),
		SEX = sample(c("M", "F"), n * 2, replace = TRUE),
		AGEGR1 = sample(c("<65", ">=65"), n * 2, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create mock meta-analysis study data
#'
#' @param k Integer specifying number of studies. Default: 5
#' @return A data frame with mock meta-analysis data
create_mock_meta_studies <- function(k = 5) {
	set.seed(123)
	data.frame(
		study = paste("Study", 1:k),
		yi = log(runif(k, 0.6, 0.9)),
		sei = runif(k, 0.08, 0.20),
		ni = sample(50:200, k, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

#' Create test data for time-to-first-AE analysis
#'
#' @return A list with adsl and adae data frames for time-to-first-AE testing
create_time_to_first_ae_test_data <- function() {
	adsl <- data.frame(
		USUBJID = c("01", "02", "03", "04"),
		TRT01P = c("A", "A", "B", "B"),
		SAFFL = c("Y", "Y", "Y", "Y"),
		TRTDURD = c(10, 10, 10, 10),
		stringsAsFactors = FALSE
	)

	adae <- data.frame(
		USUBJID = c("01", "03", "04"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEBODSYS = c("Infections", "Infections", "Cardiac"),
		ASTDY = c(3, 5, 8),
		stringsAsFactors = FALSE
	)

	list(adsl = adsl, adae = adae)
}

#' Create test data for safety comparison analysis
#'
#' @return A list with adsl and adae data frames with known incidences for
#'   testing
create_comparison_test_data <- function() {
	# Create ADSL with 100 subjects per arm for easy percentage calculations
	adsl <- data.frame(
		USUBJID = sprintf("SUBJ%03d", 1:200),
		TRT01P = rep(c("Placebo", "Active"), each = 100),
		SAFFL = rep("Y", 200),
		stringsAsFactors = FALSE
	)

	# Create ADAE with known incidences:
	# - Headache: 20% in Active (20/100), 10% in Placebo (10/100)
	# - Nausea: 15% in Active (15/100), 15% in Placebo (15/100)
	# - Fatigue: 5% in Active (5/100), 10% in Placebo (10/100)
	# - Rash: 8% in Active (8/100), 0% in Placebo (0/100) - tests zero incidence
	adae <- rbind(
		# Headache - SOC: Nervous system
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:10),
			TRT01P = "Placebo",
			TRTEMFL = "Y",
			AEBODSYS = "Nervous system disorders",
			AEDECOD = "Headache",
			stringsAsFactors = FALSE
		),
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 101:120),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "Nervous system disorders",
			AEDECOD = "Headache",
			stringsAsFactors = FALSE
		),
		# Nausea - SOC: Gastrointestinal
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 11:25),
			TRT01P = "Placebo",
			TRTEMFL = "Y",
			AEBODSYS = "Gastrointestinal disorders",
			AEDECOD = "Nausea",
			stringsAsFactors = FALSE
		),
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 121:135),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "Gastrointestinal disorders",
			AEDECOD = "Nausea",
			stringsAsFactors = FALSE
		),
		# Fatigue - SOC: General disorders
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 26:35),
			TRT01P = "Placebo",
			TRTEMFL = "Y",
			AEBODSYS = "General disorders",
			AEDECOD = "Fatigue",
			stringsAsFactors = FALSE
		),
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 136:140),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "General disorders",
			AEDECOD = "Fatigue",
			stringsAsFactors = FALSE
		),
		# Rash - SOC: Skin disorders (only in Active)
		data.frame(
			USUBJID = sprintf("SUBJ%03d", 141:148),
			TRT01P = "Active",
			TRTEMFL = "Y",
			AEBODSYS = "Skin and subcutaneous tissue disorders",
			AEDECOD = "Rash",
			stringsAsFactors = FALSE
		)
	)

	list(adsl = adsl, adae = adae)
}

#' Create test configuration for config API tests
#'
#' @return A list representing a valid configuration for testing
create_test_config <- function() {
	# Note: We use priority = 0L explicitly to avoid integer type issues
	list(
		subgroups = list(
			test_sg = list(
				variable = "TESTVAR",
				labels = list(A = "Alpha", B = "Beta"),
				order = NULL,
				filter_values = NULL
			)
		),
		populations = list(
			test_pop = list(
				variable = "POPVAR",
				label = "Test Population",
				description = "A test population",
				flag_value = "Y"
			)
		),
		soc_config = list(
			variable = "AEBODSYS",
			include_all = TRUE,
			custom_order = NULL,
			sort_by = "frequency",
			min_subjects = 1,
			top_n = NULL
		),
		pt_config = list(
			variable = "AEDECOD",
			include_all = TRUE,
			sort_by = "frequency",
			min_subjects = 1,
			top_n_per_soc = NULL,
			show_pt_codes = FALSE
		),
		performance = list(
			docx = list(batch_size = 50)
		),
		report_types = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)
}

# ==============================================================================
# Efficacy Plotting Test Fixtures
# ==============================================================================

#' Create mock spider plot data for efficacy tests
#'
#' Creates data for spider plot testing with configurable parameters.
#' Used in test-plotting_efficacy.R
#'
#' @param n_subjects Integer specifying number of subjects. Default: 20
#' @param n_visits Integer specifying number of visits per subject. Default: 4
#' @param include_trt Logical indicating whether to include TRT01P column.
#'   Default: FALSE
#' @param use_cumsum Logical indicating whether to use cumulative sum for PCHG.
#'   Default: FALSE
#' @param include_na Logical indicating whether to include NA values at end.
#'   Default: FALSE
#' @return A data frame with mock spider plot data
create_mock_spider_data <- function(
	n_subjects = 20,
	n_visits = 4,
	include_trt = FALSE,
	use_cumsum = FALSE,
	include_na = FALSE
) {
	set.seed(123)
	subjects <- sprintf("SUBJ%03d", 1:n_subjects)
	avisitn <- 0:(n_visits - 1)

	if (use_cumsum) {
		# Generate cumulative percent change values
		pchg_list <- replicate(
			n_subjects,
			cumsum(c(0, rnorm(n_visits - 1, mean = -5, sd = 15))),
			simplify = FALSE
		)
		pchg <- unlist(pchg_list)
	} else {
		pchg <- rnorm(n_subjects * n_visits, 0, 20)
	}

	if (include_na) {
		pchg[(length(pchg) - 4):length(pchg)] <- NA
	}

	data <- data.frame(
		USUBJID = rep(subjects, each = n_visits),
		AVISITN = rep(avisitn, n_subjects),
		PCHG = pchg,
		stringsAsFactors = FALSE
	)

	if (include_trt) {
		data$TRT01P <- rep(
			c("Treatment", "Placebo"),
			each = n_visits * n_subjects / 2
		)
	}

	data
}

#' Create mock mean plot data for efficacy tests
#'
#' Creates data for mean plot testing with optional group variable.
#' Used in test-plotting_efficacy.R
#'
#' @param n_subjects Integer specifying number of subjects. Default: 20
#' @param include_group Logical indicating whether to include group column.
#'   Default: FALSE
#' @return A data frame with mock mean plot data
create_mock_mean_data <- function(n_subjects = 20, include_group = FALSE) {
	set.seed(123)

	data <- data.frame(
		USUBJID = rep(1:n_subjects, each = 2),
		visit = rep(c("V1", "V2"), n_subjects),
		value = rnorm(n_subjects * 2, 10, 2),
		stringsAsFactors = FALSE
	)

	if (include_group) {
		data$group <- rep(c("A", "B"), n_subjects)
	}

	data
}

# ==============================================================================
# Survival Plotting Test Fixtures
# ==============================================================================

#' Create mock KM plot data
#'
#' Creates data for Kaplan-Meier plot testing.
#' Used in test-plotting_survival.R
#'
#' @param n Integer specifying number of subjects. Default: 40
#' @param rate Numeric specifying rate for rexp. Default: 0.05
#' @param prob_event Numeric specifying probability of event. Default: 0.7
#' @param use_adam_names Logical indicating whether to use ADaM column names
#'   (AVAL, CNSR, TRT01P). Default: TRUE
#' @return A data frame with mock KM plot data
create_mock_km_data <- function(
	n = 40,
	rate = 0.05,
	prob_event = 0.7,
	use_adam_names = TRUE
) {
	set.seed(42)

	if (use_adam_names) {
		data.frame(
			USUBJID = sprintf("SUBJ%02d", 1:n),
			AVAL = rexp(n, rate),
			CNSR = sample(
				0:1,
				n,
				replace = TRUE,
				prob = c(prob_event, 1 - prob_event)
			),
			TRT01P = rep(c("Placebo", "Active"), each = n / 2),
			stringsAsFactors = FALSE
		)
	} else {
		data.frame(
			time = rexp(n, rate),
			event = sample(
				0:1,
				n,
				replace = TRUE,
				prob = c(prob_event, 1 - prob_event)
			),
			trt = rep(c("A", "B"), each = n / 2),
			stringsAsFactors = FALSE
		)
	}
}

# ==============================================================================
# Responder Analysis Test Fixtures
# ==============================================================================

#' Create mock responder analysis data
#'
#' Creates data for responder analysis table testing.
#' Used in test-efficacy_responder.R
#'
#' @param n Integer specifying number of subjects. Default: 40
#' @param zero_ref Logical indicating whether all reference subjects are
#'   non-responders. Default: FALSE
#' @param full_trt Logical indicating whether all treatment subjects are
#'   responders. Default: FALSE
#' @param ref_group Character specifying reference group. Default: "Placebo"
#' @return A data frame with mock responder analysis data
create_mock_responder_data <- function(
	n = 40,
	zero_ref = FALSE,
	full_trt = FALSE,
	ref_group = "Placebo"
) {
	set.seed(42)

	avalc <- sample(c("CR", "PR", "SD", "PD"), n, replace = TRUE)

	if (zero_ref) {
		# All placebo subjects are non-responders
		n_ref <- n / 2
		avalc[1:n_ref] <- sample(c("SD", "PD"), n_ref, replace = TRUE)
	}

	if (full_trt) {
		# All treatment subjects are responders
		n_trt <- n / 2
		avalc[(n_trt + 1):n] <- rep("CR", n_trt)
	}

	data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:n),
		TRT01P = rep(c(ref_group, "Active"), each = n / 2),
		AVALC = avalc,
		stringsAsFactors = FALSE
	)
}

# ==============================================================================
# Subgroup Analysis Test Fixtures
# ==============================================================================

#' Create mock small subgroup data
#'
#' Creates minimal test data for subgroup analysis with small sample sizes.
#' Useful for testing edge cases and warnings.
#' Used in test-efficacy_subgroup.R
#'
#' @param n Integer specifying number of subjects. Default: 4
#' @param include_sex Logical indicating whether to include SEX column.
#'   Default: TRUE
#' @return A data frame with mock small subgroup data
create_mock_small_subgroup_data <- function(n = 4, include_sex = TRUE) {
	set.seed(42)

	data <- data.frame(
		USUBJID = c("001", "002", "003", "004"),
		TRT01P = c("Placebo", "Placebo", "Active", "Active"),
		AVALC = c("CR", "SD", "CR", "PR"),
		stringsAsFactors = FALSE
	)

	if (include_sex) {
		data$SEX <- c("M", "F", "M", "F")
	}

	data
}

# ==============================================================================
# TTE Analysis Test Fixtures
# ==============================================================================

#' Create mock TTE summary data
#'
#' Creates TTE data for summary table testing.
#' Used in test-efficacy_tte.R
#'
#' @param n Integer specifying number of subjects. Default: 40
#' @param include_subject_id Logical indicating whether to include USUBJID.
#'   Default: TRUE
#' @return A data frame with mock TTE data
create_mock_tte_summary_data <- function(n = 40, include_subject_id = TRUE) {
	set.seed(42)

	data <- data.frame(
		TRT01P = rep(c("Placebo", "Active"), each = n / 2),
		AVAL = c(rexp(n / 2, 0.05), rexp(n / 2, 0.03)),
		CNSR = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)),
		stringsAsFactors = FALSE
	)

	if (include_subject_id) {
		data$USUBJID <- sprintf("SUBJ%02d", 1:n)
		# Reorder columns
		data <- data[, c("USUBJID", "TRT01P", "AVAL", "CNSR")]
	}

	data
}

# ==============================================================================
# Forest Plot Test Fixtures
# ==============================================================================

#' Create mock forest plot binary data
#'
#' Creates data for forest plot testing with binary endpoints.
#' Used in test-plotting_forest.R
#'
#' @param n Integer specifying number of subjects. Default: 60
#' @param response_values Character vector specifying responder values.
#'   Default: c("CR", "PR")
#' @return A data frame with mock forest plot binary data
create_mock_forest_binary_data <- function(
	n = 60,
	response_values = c("CR", "PR")
) {
	set.seed(42)

	data.frame(
		USUBJID = sprintf("SUBJ%02d", 1:n),
		TRT01P = rep(c("Placebo", "Active"), each = n / 2),
		AVALC = sample(c("CR", "PR", "SD", "PD"), n, replace = TRUE),
		SEX = rep(c("M", "F"), n / 2),
		stringsAsFactors = FALSE
	)
}

# ==============================================================================
# AE Summary Test Fixtures
# ==============================================================================

#' Create mock AE summary data
#'
#' Creates minimal test data for AE summary table testing.
#' Used in test-safety_summary.R
#'
#' @param n Integer specifying number of subjects. Default: 3
#' @return A list with adae and adsl data frames
create_mock_ae_summary_data <- function(n = 3) {
	adae <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		TRTEMFL = c("Y", "Y", "Y"),
		AEREL = c("RELATED", "NONE", "POSSIBLE"),
		AESER = c("N", "N", "Y"),
		AEACN = c("NONE", "DRUG WITHDRAWN", "NONE"),
		AEOUT = c("RECOVERED", "RECOVERED", "FATAL"),
		stringsAsFactors = FALSE
	)

	adsl <- data.frame(
		USUBJID = c("01", "02", "03"),
		TRT01P = c("A", "A", "B"),
		SAFFL = rep("Y", n),
		stringsAsFactors = FALSE
	)

	list(adae = adae, adsl = adsl)
}

# ==============================================================================
# Multiple Imputation Test Data
# ==============================================================================

#' Create test data for imputation tests
#'
#' @param n Number of rows
#' @param seed Random seed
#' @return Data frame with missing values
create_imputation_test_data <- function(n = 30, seed = 123) {
	if (n < 5) {
		stop("n must be at least 5 for create_imputation_test_data()")
	}
	set.seed(seed)
	data.frame(
		x = c(rnorm(n - 5, mean = 50, sd = 10), rep(NA, 5)),
		y = c(rep(NA, 3), rnorm(n - 3, mean = 100, sd = 20)),
		z = sample(c("A", "B"), n, replace = TRUE),
		stringsAsFactors = FALSE
	)
}

# =============================================================================
# Helper functions for Bayesian tests
# =============================================================================

#' Skip test if brms package is not available
skip_if_brms_unavailable <- function() {
	if (!requireNamespace("brms", quietly = TRUE)) {
		skip("brms package not available")
	}
}

# =============================================================================
# MMRM Test Data
# =============================================================================

#' Create test data for MMRM analysis
#'
#' @param n_subjects Number of subjects
#' @param n_visits Number of visits per subject
#' @param seed Random seed
#' @return Data frame suitable for MMRM testing
create_mmrm_test_data <- function(n_subjects = 30, n_visits = 4, seed = 123) {
	set.seed(seed)

	# Generate subject-level data
	subjects <- sprintf("SUBJ%03d", 1:n_subjects)
	trt_grp <- sample(c("A", "B"), n_subjects, replace = TRUE)
	base_vals <- rnorm(n_subjects, mean = 50, sd = 10)

	# Generate visit-level data (keep numeric for now)
	visit_nums <- rep(0:(n_visits - 1), n_subjects)
	subject_ids <- rep(subjects, each = n_visits)
	subject_idx <- rep(1:n_subjects, each = n_visits)

	data <- data.frame(
		USUBJID = subject_ids,
		TRT01P = rep(trt_grp, each = n_visits),
		AVISITN = visit_nums,
		BASE = rep(base_vals, each = n_visits),
		stringsAsFactors = FALSE
	)

	# Generate response values with treatment effect
	# Create effect vectors sized to n_visits
	effect_A <- seq(0, by = 2, length.out = n_visits) # Linear improvement for A
	effect_B <- seq(0, by = 1, length.out = n_visits) # Smaller improvement for B

	# Use numeric visit index for effect lookup
	data$AVAL <- data$BASE +
		ifelse(
			data$TRT01P == "A",
			effect_A[data$AVISITN + 1],
			effect_B[data$AVISITN + 1]
		) +
		rnorm(nrow(data), 0, 3) + # Random error
		rnorm(n_subjects, 0, 5)[subject_idx] # Random subject effect

	# Convert to factors AFTER calculations (required by mmrm)
	data$USUBJID <- factor(data$USUBJID)
	data$AVISITN <- factor(data$AVISITN)

	data
}

# =============================================================================
# RMST Test Data
# =============================================================================

#' Create test data for RMST analysis
#'
#' @param n Number of subjects
#' @param seed Random seed
#' @return Data frame suitable for RMST testing
create_rmst_test_data <- function(n = 100, seed = 123) {
	set.seed(seed)

	# Generate survival times with treatment effect
	trt_grp <- sample(c("A", "B"), n, replace = TRUE)

	# Initialize times vector
	times <- numeric(n)
	# Assign times based on actual treatment assignment
	times[trt_grp == "A"] <- rexp(sum(trt_grp == "A"), 0.1) # Lower hazard
	times[trt_grp == "B"] <- rexp(sum(trt_grp == "B"), 0.15) # Higher hazard

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
