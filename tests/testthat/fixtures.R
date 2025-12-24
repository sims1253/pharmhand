#' Test Data Fixtures for FunctionReport
#'
#' Helper functions to create mock data for testing clinical study functions.

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
