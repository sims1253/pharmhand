# Test Utility Functions
#'
#' Tests for utils.R functions including format_number, format_percentage,
#' format_ci, and format_pvalue.

test_that("format_number formats numeric values", {
	expect_equal(format_number(45.2345, digits = 2), "45.23")
	expect_equal(format_number(45.2345, digits = 1), "45.2")
	expect_equal(format_number(45.2345, digits = 0), "45")
})

test_that("format_number handles vector input", {
	result <- format_number(c(1.5, 2.345, 3.6789), digits = 1)
	expect_equal(result, c("1.5", "2.3", "3.7"))
})

test_that("format_percentage formats as percentage", {
	expect_equal(format_percentage(0.5, digits = 1), "50.0%")
	expect_equal(format_percentage(0.123, digits = 1), "12.3%")
	expect_equal(format_percentage(0.789, digits = 1), "78.9%")
})

test_that("format_percentage handles vector input", {
	result <- format_percentage(c(0.123, 0.456, 0.789), digits = 1)
	expect_equal(result, c("12.3%", "45.6%", "78.9%"))
})

test_that("format_ci formats confidence interval", {
	expect_equal(format_ci(1.23, 4.56, digits = 2), "1.23, 4.56")
	expect_equal(format_ci(1.23, 4.56, digits = 1), "1.2, 4.6")
})

test_that("format_ci with custom separator", {
	expect_equal(format_ci(1.23, 4.56, separator = " - "), "1.23 - 4.56")
})

test_that("format_ci handles vector input", {
	result <- format_ci(c(1.23, 5.67), c(4.56, 8.90), digits = 1)
	expect_equal(result, c("1.2, 4.6", "5.7, 8.9"))
})

test_that("format_pvalue formats p-values", {
	expect_equal(format_pvalue(0.0234, digits = 3), "0.023")
	expect_equal(format_pvalue(0.4567, digits = 3), "0.457")
})

test_that("format_pvalue formats small p-values", {
	expect_equal(format_pvalue(0.0005, digits = 3), "<0.001")
	expect_equal(format_pvalue(0.0001, digits = 3), "<0.001")
})

test_that("format_pvalue with custom threshold", {
	expect_equal(format_pvalue(0.0005, threshold = 0.01), "<0.01")
	expect_equal(format_pvalue(0.005, threshold = 0.01), "<0.01")
})

test_that("format_pvalue handles vector input", {
	result <- format_pvalue(c(0.0005, 0.0234, 0.4567), digits = 3)
	expect_equal(result, c("<0.001", "0.023", "0.457"))
})
