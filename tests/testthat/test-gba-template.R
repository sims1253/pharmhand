# Tests for to_gba_template

describe("gba-template", {
	it("to_gba_template formats ClinicalTable", {
		df <- data.frame(Statistic = "n", Value = 10)
		table <- ClinicalTable(data = df, flextable = NULL, type = "test")

		result <- to_gba_template(table, autofit = FALSE)

		expect_true(S7::S7_inherits(result, ClinicalTable))
		expect_true(inherits(result@flextable, "flextable"))
		expect_equal(attr(result@flextable, "pharmhand_theme"), "gba")
	})

	it("to_gba_template formats ClinicalReport tables", {
		df <- data.frame(Statistic = "n", Value = 10)
		table <- ClinicalTable(data = df, flextable = NULL, type = "test")

		section <- ReportSection(
			title = "Section",
			section_type = "test",
			content = list(table)
		)

		report <- ClinicalReport(
			study_id = "STUDY001",
			study_title = "Study Title",
			sections = list(section)
		)

		result <- to_gba_template(report, autofit = FALSE)

		inner_table <- result@sections[[1]]@content[[1]]
		expect_equal(attr(inner_table@flextable, "pharmhand_theme"), "gba")
	})

	it("to_gba_template handles list input", {
		df <- data.frame(Statistic = "n", Value = 10)
		table <- ClinicalTable(data = df, flextable = NULL, type = "test")

		result <- to_gba_template(list(table), autofit = FALSE)

		expect_equal(length(result), 1)
		expect_equal(attr(result[[1]]@flextable, "pharmhand_theme"), "gba")
	})

	it("to_gba_template validates input", {
		expect_error(
			to_gba_template("invalid"),
			"must be a ClinicalTable, ClinicalReport, or list"
		)
	})
})
