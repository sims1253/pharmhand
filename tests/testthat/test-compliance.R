# Tests for G-BA compliance checks

test_that("check_gba_compliance passes for valid data.frame", {
	df <- data.frame(Statistic = "n", Value = 10)
	res <- check_gba_compliance(df, strict = FALSE, require_theme = FALSE)

	expect_true(res$ok)
	expect_length(res$errors, 0)
})

test_that("check_gba_compliance flags duplicate column names", {
	df <- data.frame(a = 1, a = 2, check.names = FALSE)
	res <- check_gba_compliance(df, strict = FALSE, require_theme = FALSE)

	expect_false(res$ok)
	expect_true(any(grepl("duplicate column names", res$errors)))
})

test_that("check_gba_compliance flags empty column names", {
	df <- data.frame(x = 1, check.names = FALSE)
	names(df) <- ""
	res <- check_gba_compliance(df, strict = FALSE, require_theme = FALSE)

	expect_false(res$ok)
	expect_true(any(grepl("empty column names", res$errors)))
})

test_that("check_gba_compliance validates flextable theme marker", {
	df <- data.frame(Statistic = "n", Value = 10)
	ft <- flextable::flextable(df)

	res <- check_gba_compliance(ft, strict = FALSE, require_theme = TRUE)
	expect_false(res$ok)
	expect_true(any(grepl("missing G-BA theme marker", res$errors)))

	ft_gba <- theme_gba(ft, autofit = FALSE)
	res_gba <- check_gba_compliance(ft_gba, strict = FALSE, require_theme = TRUE)
	expect_true(res_gba$ok)
})

test_that("check_gba_compliance handles ClinicalTable", {
	df <- data.frame(Statistic = "n", Value = 10)
	ft <- theme_gba(flextable::flextable(df), autofit = FALSE)
	table <- ClinicalTable(data = df, flextable = ft, type = "test")

	res <- check_gba_compliance(table, strict = FALSE)
	expect_true(res$ok)
})

test_that("check_gba_compliance errors on invalid input in strict mode", {
	expect_error(
		check_gba_compliance("invalid", strict = TRUE),
		"must be a ClinicalTable, flextable, or data.frame"
	)
})

test_that("check_gba_compliance handles list input", {
	valid <- data.frame(Statistic = "n", Value = 10)
	invalid <- data.frame(a = 1, a = 2, check.names = FALSE)

	res <- check_gba_compliance(
		list(valid = valid, invalid = invalid),
		strict = FALSE,
		require_theme = FALSE
	)

	expect_false(res$ok)
	expect_true(any(grepl("duplicate column names", res$errors)))
})

test_that("check_gba_compliance enforces title when required", {
	df <- data.frame(Statistic = "n", Value = 10)
	ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

	res <- check_gba_compliance(
		ft,
		strict = FALSE,
		require_theme = TRUE,
		require_title = TRUE
	)
	expect_false(res$ok)
	expect_true(any(grepl("title", res$errors)))
})
