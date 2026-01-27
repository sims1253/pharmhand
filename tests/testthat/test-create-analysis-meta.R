# Tests for create_analysis_meta helper function

describe("create-analysis-meta", {
	it("create_analysis_meta returns AnalysisMeta object", {
		meta <- create_analysis_meta()

		expect_true(S7::S7_inherits(meta, AnalysisMeta))
	})

	it("create_analysis_meta populates timestamp with current time", {
		before <- Sys.time()
		meta <- create_analysis_meta()
		after <- Sys.time()

		expect_true(meta@timestamp >= before)
		expect_true(meta@timestamp <= after)
		expect_true(inherits(meta@timestamp, "POSIXct"))
	})

	it("create_analysis_meta populates package_version", {
		meta <- create_analysis_meta()

		expect_true(nchar(meta@package_version) > 0)
	})

	it("create_analysis_meta populates r_version", {
		meta <- create_analysis_meta()

		expect_true(nchar(meta@r_version) > 0)
		# r_version format is "major.minor.patch" e.g., "4.3.2"
		expect_true(grepl("^\\d+\\.\\d+\\.\\d+$", meta@r_version))
	})

	it("create_analysis_meta sets source_vars correctly", {
		meta <- create_analysis_meta(source_vars = c("AGE", "SEX", "TRT"))

		expect_equal(meta@source_vars, c("AGE", "SEX", "TRT"))
	})

	it("create_analysis_meta defaults source_vars to empty", {
		meta <- create_analysis_meta()

		expect_equal(meta@source_vars, character())
	})

	it("create_analysis_meta sets filters correctly", {
		filters <- list(
			population = "ITT",
			safl = "Y"
		)
		meta <- create_analysis_meta(filters = filters)

		expect_equal(meta@filters, filters)
	})

	it("create_analysis_meta defaults filters to empty list", {
		meta <- create_analysis_meta()

		expect_equal(meta@filters, list())
	})

	it("create_analysis_meta sets row_id correctly", {
		meta <- create_analysis_meta(row_id = "demographics_age_mean")

		expect_equal(meta@row_id, "demographics_age_mean")
	})

	it("create_analysis_meta defaults row_id to empty string", {
		meta <- create_analysis_meta()

		expect_equal(meta@row_id, "")
	})

	it("create_analysis_meta sets derivation correctly", {
		meta <- create_analysis_meta(derivation = "mean(AGE)")

		expect_equal(meta@derivation, "mean(AGE)")
	})

	it("create_analysis_meta defaults derivation to empty string", {
		meta <- create_analysis_meta()

		expect_equal(meta@derivation, "")
	})

	it("create_analysis_meta works with all parameters", {
		meta <- create_analysis_meta(
			source_vars = c("AVAL", "TRT01P"),
			filters = list(population = "FAS", safl = "Y"),
			row_id = "efficacy_response_rate",
			derivation = "RESP == 1"
		)

		expect_true(S7::S7_inherits(meta, AnalysisMeta))
		expect_equal(meta@source_vars, c("AVAL", "TRT01P"))
		expect_equal(meta@filters, list(population = "FAS", safl = "Y"))
		expect_equal(meta@row_id, "efficacy_response_rate")
		expect_equal(meta@derivation, "RESP == 1")
		expect_true(inherits(meta@timestamp, "POSIXct"))
		expect_true(nchar(meta@package_version) > 0)
		expect_true(nchar(meta@r_version) > 0)
	})

	# AnalysisMeta timestamp validator tests ----

	it("AnalysisMeta accepts NULL timestamp", {
		meta <- AnalysisMeta(
			source_vars = c("AGE"),
			timestamp = NULL
		)

		expect_true(S7::S7_inherits(meta, AnalysisMeta))
		expect_null(meta@timestamp)
	})

	it("AnalysisMeta accepts POSIXct timestamp", {
		ts <- Sys.time()
		meta <- AnalysisMeta(
			source_vars = c("AGE"),
			timestamp = ts
		)

		expect_true(S7::S7_inherits(meta, AnalysisMeta))
		expect_equal(meta@timestamp, ts)
	})

	it("AnalysisMeta rejects non-POSIXct timestamp", {
		expect_error(
			AnalysisMeta(source_vars = c("AGE"), timestamp = "2023-01-01"),
			"timestamp must be NULL or POSIXct"
		)

		expect_error(
			AnalysisMeta(source_vars = c("AGE"), timestamp = 20230101),
			"timestamp must be NULL or POSIXct"
		)

		expect_error(
			AnalysisMeta(source_vars = c("AGE"), timestamp = as.Date("2023-01-01")),
			"timestamp must be NULL or POSIXct"
		)
	})

	it("AnalysisMeta allows timestamp to be default (NULL)", {
		meta <- AnalysisMeta()

		expect_null(meta@timestamp)
	})

	# AnalysisMeta other property tests ----

	it("AnalysisMeta accepts character source_vars", {
		meta <- AnalysisMeta(source_vars = c("AGE", "SEX"))

		expect_equal(meta@source_vars, c("AGE", "SEX"))
	})

	it("AnalysisMeta defaults source_vars to empty character", {
		meta <- AnalysisMeta()

		expect_equal(meta@source_vars, character())
	})

	it("AnalysisMeta accepts list filters", {
		filters <- list(population = "ITT")
		meta <- AnalysisMeta(filters = filters)

		expect_equal(meta@filters, filters)
	})

	it("AnalysisMeta defaults filters to empty list", {
		meta <- AnalysisMeta()

		expect_equal(meta@filters, list())
	})

	it("AnalysisMeta accepts character row_id", {
		meta <- AnalysisMeta(row_id = "test_row")

		expect_equal(meta@row_id, "test_row")
	})

	it("AnalysisMeta defaults row_id to empty string", {
		meta <- AnalysisMeta()

		expect_equal(meta@row_id, "")
	})

	it("AnalysisMeta accepts character derivation", {
		meta <- AnalysisMeta(derivation = "mean(x)")

		expect_equal(meta@derivation, "mean(x)")
	})

	it("AnalysisMeta defaults derivation to empty string", {
		meta <- AnalysisMeta()

		expect_equal(meta@derivation, "")
	})

	it("AnalysisMeta accepts empty package_version", {
		meta <- AnalysisMeta(package_version = "")

		expect_equal(meta@package_version, "")
	})

	it("AnalysisMeta accepts empty r_version", {
		meta <- AnalysisMeta(r_version = "")

		expect_equal(meta@r_version, "")
	})
})
