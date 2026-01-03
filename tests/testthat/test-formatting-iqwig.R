# Tests for IQWiG-compliant formatting functions

test_that("format_pvalue formats correctly with default locale (en)", {
	# Basic formatting with 3 decimal places
	expect_equal(format_pvalue(0.0234), "0.023")
	expect_equal(format_pvalue(0.05), "0.050")
	expect_equal(format_pvalue(0.999), "0.999")
	expect_equal(format_pvalue(0.001), "0.001")

	# Below threshold
	expect_equal(format_pvalue(0.0001), "<0.001")
	expect_equal(format_pvalue(0.00005), "<0.001")
	expect_equal(format_pvalue(0.0000001), "<0.001")

	# Edge cases
	expect_equal(format_pvalue(0), "<0.001")
	expect_equal(format_pvalue(1), "1.000")

	# NA handling
	expect_equal(format_pvalue(NA), "NA")
})

test_that("format_pvalue respects German locale", {
	# German locale uses comma as decimal separator
	expect_equal(format_pvalue(0.0234, locale = "de"), "0,023")
	expect_equal(format_pvalue(0.05, locale = "de"), "0,050")
	expect_equal(format_pvalue(0.0001, locale = "de"), "<0,001")
})

test_that("format_pvalue handles custom digits and threshold", {
	# Custom digits
	expect_equal(format_pvalue(0.12345, digits = 4), "0.1235")
	expect_equal(format_pvalue(0.12345, digits = 2), "0.12")

	# Custom threshold
	expect_equal(format_pvalue(0.005, threshold = 0.01), "<0.010")
	expect_equal(format_pvalue(0.015, threshold = 0.01), "0.015")
})

test_that("format_pvalue is vectorized", {
	p_values <- c(0.05, 0.001, 0.0001, NA)
	expected <- c("0.050", "0.001", "<0.001", "NA")
	expect_equal(format_pvalue(p_values), expected)
})

test_that("format_ci formats correctly with semicolon separator", {
	# Basic CI formatting
	expect_equal(format_ci(0.85, 1.23), "[0.85; 1.23]")
	expect_equal(format_ci(0.5, 0.9), "[0.50; 0.90]")

	# Negative values
	expect_equal(format_ci(-0.5, 0.5), "[-0.50; 0.50]")

	# NA handling
	expect_equal(format_ci(NA, 1.0), "NA")
	expect_equal(format_ci(0.5, NA), "NA")
})

test_that("format_ci respects German locale", {
	expect_equal(format_ci(0.85, 1.23, locale = "de"), "[0,85; 1,23]")
})

test_that("format_ci handles custom digits and brackets", {
	# Custom digits
	expect_equal(format_ci(0.8567, 1.2345, digits = 3), "[0.857; 1.235]")

	# Custom brackets
	expect_equal(
		format_ci(0.85, 1.23, brackets = c("(", ")")),
		"(0.85; 1.23)"
	)
})

test_that("format_ci is vectorized", {
	lower <- c(0.5, 0.7, NA)
	upper <- c(0.9, 1.1, 1.0)
	expected <- c("[0.50; 0.90]", "[0.70; 1.10]", "NA")
	expect_equal(format_ci(lower, upper), expected)
})

test_that("format_ci validates input lengths", {
	expect_error(
		format_ci(c(0.5, 0.6), c(0.9)),
		"must have the same length"
	)
})

test_that("format_number handles locale correctly", {
	# English (default)
	expect_equal(format_number(1234.567, 2, "en"), "1234.57")

	# German
	expect_equal(format_number(1234.567, 2, "de"), "1234,57")

	# NA handling
	expect_equal(format_number(NA, 2, "en"), "NA")
})

test_that("format_percentage formats correctly", {
	# Basic percentage
	expect_equal(format_percentage(23.5), "23.5%")
	expect_equal(format_percentage(100), "100.0%")

	# Proportion conversion
	expect_equal(format_percentage(0.235, is_proportion = TRUE), "23.5%")

	# Without symbol
	expect_equal(format_percentage(23.5, symbol = FALSE), "23.5")

	# German locale
	expect_equal(format_percentage(23.5, locale = "de"), "23,5%")

	# Custom digits
	expect_equal(format_percentage(23.567, digits = 2), "23.57%")

	# NA handling
	expect_equal(format_percentage(NA), "NA")
})
