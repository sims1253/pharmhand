# Tests for format string grammar

test_that("format_spec creates valid FormatSpec object", {
	spec <- format_spec("xx.xx")

	expect_true(S7::S7_inherits(spec, FormatSpec))
	expect_equal(spec@pattern, "xx.xx")
	expect_equal(spec@null_format, "--")
	expect_equal(spec@neg_format, "sign")
})

test_that("format_spec validates pattern", {
	expect_error(format_spec("invalid!pattern"))
	expect_no_error(format_spec("a.a"))
	expect_no_error(format_spec("xx.xxx"))
	expect_no_error(format_spec("a.a+1"))
})

test_that("parse_format_pattern handles fixed width", {
	parsed <- parse_format_pattern("xx.xxx")

	expect_equal(parsed$int_width, 2)
	expect_equal(parsed$dec_width, 3)
	expect_false(parsed$auto_int)
	expect_false(parsed$auto_dec)
})

test_that("parse_format_pattern handles auto width", {
	parsed <- parse_format_pattern("a.a")

	expect_true(parsed$auto_int)
	expect_true(parsed$auto_dec)
})

test_that("parse_format_pattern handles auto with adjustment", {
	parsed <- parse_format_pattern("a.a+2")

	expect_true(parsed$auto_dec)
	expect_equal(parsed$dec_adjust, 2L)
})

test_that("apply_format formats fixed width correctly", {
	result <- apply_format("xx.x", 12.345, align = FALSE)
	expect_equal(result, "12.3")

	result <- apply_format("xx.xx", 1.5, align = FALSE)
	expect_equal(result, "1.50")
})

test_that("apply_format handles NA values", {
	spec <- format_spec("xx.x", null_format = "--")
	result <- apply_format(spec, NA_real_)
	expect_equal(result, "--")
})

test_that("apply_format handles negative numbers with sign", {
	spec <- format_spec("xx.x", neg_format = "sign")
	result <- apply_format(spec, -12.3, align = FALSE)
	expect_true(grepl("-", result, fixed = TRUE))
})

test_that("apply_format handles negative numbers with parens", {
	spec <- format_spec("xx.x", neg_format = "parens")
	result <- apply_format(spec, -12.3, align = FALSE)
	expect_true(grepl("(", result, fixed = TRUE))
	expect_true(grepl(")", result, fixed = TRUE))
})

test_that("apply_format handles integer-only format", {
	result <- apply_format("xx", 12.7, align = FALSE)
	expect_equal(result, "13")
})

test_that("apply_format right-aligns when requested", {
	result <- apply_format("xx.x", c(1.5, 123.4), align = TRUE)
	# Shorter value should have leading spaces
	expect_true(nchar(result[1]) <= nchar(result[2]))
})

test_that("CompositeFormat creates valid object", {
	fmt <- composite_format("{n} ({pct}%)", n = "a", pct = "xx.x")

	expect_true(S7::S7_inherits(fmt, CompositeFormat))
	expect_equal(fmt@template, "{n} ({pct}%)")
	expect_true("n" %in% names(fmt@specs))
	expect_true("pct" %in% names(fmt@specs))
})

test_that("apply_composite formats correctly", {
	fmt <- composite_format("{n} ({pct}%)", n = "a", pct = "xx.x")
	result <- apply_composite(fmt, n = 15, pct = 23.456)

	expect_equal(result, "15 (23.5%)")
})

test_that("fmt_n_pct preset works", {
	fmt <- fmt_n_pct()
	result <- apply_composite(fmt, n = 10, pct = 50.5)
	expect_equal(result, "10 (50.5%)")
})

test_that("fmt_mean_sd preset works", {
	fmt <- fmt_mean_sd()
	result <- apply_composite(fmt, mean = 25.3, sd = 4.56)
	expect_equal(result, "25.3 (4.56)")
})

test_that("fmt_median_range preset works", {
	fmt <- fmt_median_range()
	result <- apply_composite(fmt, median = 30.0, min = 18.0, max = 65.0)
	expect_equal(result, "30.0 (18.0, 65.0)")
})

test_that("fmt_ci preset works", {
	fmt <- fmt_ci()
	result <- apply_composite(fmt, est = 0.85, lower = 0.75, upper = 0.95)
	expect_equal(result, "0.85 (0.75, 0.95)")
})

test_that("fmt_pvalue formats correctly", {
	pval_fmt <- fmt_pvalue(threshold = 0.001)

	expect_equal(pval_fmt(0.05), "0.050")
	expect_equal(pval_fmt(0.0001), "<0.001")
	expect_equal(pval_fmt(NA), "--")
})

test_that("format_spec accepts custom null_format", {
	spec <- format_spec("xx.x", null_format = "N/A")
	result <- apply_format(spec, NA_real_)
	expect_equal(result, "N/A")
})

test_that("apply_format handles vector input", {
	values <- c(1.5, 2.5, NA, 4.5)
	result <- apply_format("xx.x", values)

	expect_length(result, 4)
	expect_equal(result[3], "--")
})
