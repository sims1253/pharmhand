# Tests for R/efficacy_cfb.R

describe("efficacy cfb", {
	it("create_cfb_summary_table returns ClinicalTable", {
		advs <- fixture_advs_cfb()

		tbl <- create_cfb_summary_table(advs, params = "SYSBP")

		expect_s7_class(tbl, ClinicalTable)
	})

	it("create_cfb_summary_table has correct type", {
		advs <- fixture_advs_cfb()

		tbl <- create_cfb_summary_table(advs, params = "SYSBP")

		expect_equal(tbl@type, "cfb")
	})

	it("create_cfb_summary_table has expected columns", {
		advs <- fixture_advs_cfb()

		tbl <- create_cfb_summary_table(advs, params = "SYSBP")

		expect_true(all(c("Parameter", "A n", "A Mean (SD)") %in% names(tbl@data)))
	})

	it("create_vs_by_visit_table returns ClinicalTable", {
		advs <- fixture_advs_vs_by_visit()

		tbl <- create_vs_by_visit_table(
			advs,
			visits = c("Baseline", "Week 2")
		)

		expect_s7_class(tbl, ClinicalTable)
	})

	it("create_vs_by_visit_table has correct type", {
		advs <- fixture_advs_vs_by_visit()

		tbl <- create_vs_by_visit_table(
			advs,
			visits = c("Baseline", "Week 2")
		)

		expect_equal(tbl@type, "vs_by_visit")
	})

	it("create_vs_by_visit_table has correct row count", {
		advs <- fixture_advs_vs_by_visit()

		tbl <- create_vs_by_visit_table(
			advs,
			visits = c("Baseline", "Week 2")
		)

		expect_equal(nrow(tbl@data), 2)
	})

	it("create_cfb_summary_table errors on NULL input", {
		expect_error(
			create_cfb_summary_table(NULL, params = "SYSBP"),
			"must be an ADaMData object or data.frame"
		)
	})

	it("create_cfb_summary_table errors when required columns missing", {
		advs_missing <- data.frame(
			USUBJID = c("01", "02"),
			TRT01P = c("A", "B"),
			PARAMCD = c("SYSBP", "SYSBP"),
			AVISIT = c("End of Treatment", "End of Treatment"),
			PARAM = c("SYSBP", "SYSBP")
		)
		expect_error(
			create_cfb_summary_table(advs_missing, params = "SYSBP"),
			"missing required columns"
		)
	})

	it("create_vs_by_visit_table errors on NULL input", {
		expect_error(
			create_vs_by_visit_table(NULL),
			"must be an ADaMData object or data.frame"
		)
	})

	it("create_vs_by_visit_table errors when required columns missing", {
		expect_error(
			create_vs_by_visit_table(data.frame()),
			"missing required columns"
		)
	})
})
