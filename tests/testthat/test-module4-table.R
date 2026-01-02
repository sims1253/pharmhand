# Tests for create_hta_module4_table

test_that("create_hta_module4_table creates empty template", {
	result <- create_hta_module4_table()

	expect_true(S7::S7_inherits(result, ClinicalTable))
	expect_equal(nrow(result@data), 0)
	expect_equal(
		names(result@data),
		c(
			"Endpoint",
			"Analysis Set",
			"Treatment",
			"Comparator",
			"Effect",
			"95% CI",
			"p-value"
		)
	)
})

test_that("create_hta_module4_table adds missing columns", {
	df <- data.frame(Endpoint = "OS", Effect = 0.8)
	result <- create_hta_module4_table(df, autofit = FALSE)

	expect_true(all(
		c("Treatment", "Comparator", "95% CI", "p-value") %in% names(result@data)
	))
	expect_equal(ncol(result@data), 7)
})

test_that("create_hta_module4_table drops extra columns by default", {
	df <- data.frame(
		Endpoint = "OS",
		Effect = 0.8,
		Extra = "drop"
	)

	expect_warning(
		result <- create_hta_module4_table(df, autofit = FALSE),
		"Dropping extra columns"
	)

	expect_false("Extra" %in% names(result@data))
})

test_that("create_hta_module4_table allows extra columns when requested", {
	df <- data.frame(
		Endpoint = "OS",
		Effect = 0.8,
		Extra = "keep"
	)

	result <- create_hta_module4_table(df, allow_extra = TRUE, autofit = FALSE)

	expect_true("Extra" %in% names(result@data))
	expect_equal(tail(names(result@data), 1), "Extra")
})

test_that("create_hta_module4_table applies G-BA theme marker", {
	result <- create_hta_module4_table(
		data.frame(Endpoint = "OS"),
		autofit = FALSE
	)

	expect_equal(attr(result@flextable, "pharmhand_theme"), "gba")
})

test_that("create_hta_module4_table validates empty columns vector", {
	expect_error(
		create_hta_module4_table(columns = character()),
		"must contain at least one column"
	)
})
