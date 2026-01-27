# Tests for format string grammar

describe("formatting", {
	it("format_spec creates valid FormatSpec object", {
		spec <- format_spec("xx.xx")

		expect_true(S7::S7_inherits(spec, FormatSpec))
		expect_equal(spec@pattern, "xx.xx")
		expect_equal(spec@null_format, "--")
		expect_equal(spec@neg_format, "sign")
	})

	it("format_spec validates pattern", {
		expect_error(format_spec("invalid!pattern"))
		expect_no_error(format_spec("a.a"))
		expect_no_error(format_spec("xx.xxx"))
		expect_no_error(format_spec("a.a+1"))
	})

	# Note: parse_format_pattern is an internal function (not exported)
	# We test it indirectly through format_spec and apply_format

	it("format_spec handles fixed width pattern correctly", {
		spec <- format_spec("xx.xxx")
		expect_true(S7::S7_inherits(spec, FormatSpec))
		expect_equal(spec@pattern, "xx.xxx")

		# Test that it works with apply_format
		result <- apply_format(spec, 12.3456, align = FALSE)
		expect_equal(result, "12.346")
	})

	it("format_spec handles auto width pattern correctly", {
		spec <- format_spec("a.a")
		expect_true(S7::S7_inherits(spec, FormatSpec))
		expect_equal(spec@pattern, "a.a")
	})

	it("format_spec handles auto with adjustment pattern correctly", {
		spec <- format_spec("a.a+2")
		expect_true(S7::S7_inherits(spec, FormatSpec))
		expect_equal(spec@pattern, "a.a+2")
	})

	it("apply_format formats fixed width correctly", {
		result <- apply_format("xx.x", 12.345, align = FALSE)
		expect_equal(result, "12.3")

		result <- apply_format("xx.xx", 1.5, align = FALSE)
		expect_equal(result, "1.50")
	})

	it("apply_format handles NA values", {
		spec <- format_spec("xx.x", null_format = "--")
		result <- apply_format(spec, NA_real_)
		expect_equal(result, "--")
	})

	it("apply_format handles negative numbers with sign", {
		spec <- format_spec("xx.x", neg_format = "sign")
		result <- apply_format(spec, -12.3, align = FALSE)
		expect_true(grepl("-", result, fixed = TRUE))
	})

	it("apply_format handles negative numbers with parens", {
		spec <- format_spec("xx.x", neg_format = "parens")
		result <- apply_format(spec, -12.3, align = FALSE)
		expect_true(grepl("(", result, fixed = TRUE))
		expect_true(grepl(")", result, fixed = TRUE))
	})

	it("apply_format handles integer-only format", {
		result <- apply_format("xx", 12.7, align = FALSE)
		expect_equal(result, "13")
	})

	it("apply_format right-aligns when requested", {
		result <- apply_format("xx.x", c(1.5, 12.4), align = TRUE)
		# Both values should have equal length when aligned (within spec width)
		expect_equal(nchar(result[1]), nchar(result[2]))
		# Shorter value should have leading spaces
		expect_true(grepl("^\\s", result[1]))
	})

	it("apply_format aligns correctly with parens negative format", {
		spec <- format_spec("xx.x", neg_format = "parens")
		result <- apply_format(spec, c(1.5, -12.3), align = TRUE)
		# Both values should have equal length when aligned
		# The parens format adds 1 extra character vs minus sign
		expect_equal(nchar(result[1]), nchar(result[2]))
		# Positive value should have leading space(s) to match negative with parens
		expect_true(grepl("^\\s", result[1]))
		# Negative value should have parens
		expect_true(grepl("^\\(", result[2]))
	})

	it("CompositeFormat creates valid object", {
		fmt <- composite_format("{n} ({pct}%)", n = "a", pct = "xx.x")

		expect_true(S7::S7_inherits(fmt, CompositeFormat))
		expect_equal(fmt@template, "{n} ({pct}%)")
		expect_true("n" %in% names(fmt@specs))
		expect_true("pct" %in% names(fmt@specs))
	})

	it("apply_composite formats correctly", {
		fmt <- composite_format("{n} ({pct}%)", n = "a", pct = "xx.x")
		result <- apply_composite(fmt, n = 15, pct = 23.456)

		expect_equal(result, "15 (23.5%)")
	})

	it("fmt_n_pct preset works", {
		fmt <- fmt_n_pct()
		result <- apply_composite(fmt, n = 10, pct = 50.5)
		expect_equal(result, "10 (50.5%)")
	})

	it("fmt_mean_sd preset works", {
		fmt <- fmt_mean_sd()
		result <- apply_composite(fmt, mean = 25.3, sd = 4.56)
		expect_equal(result, "25.3 (4.56)")
	})

	it("fmt_median_range preset works", {
		fmt <- fmt_median_range()
		result <- apply_composite(fmt, median = 30.0, min = 18.0, max = 65.0)
		expect_equal(result, "30.0 (18.0, 65.0)")
	})

	it("fmt_ci preset works", {
		fmt <- fmt_ci()
		result <- apply_composite(fmt, est = 0.85, lower = 0.75, upper = 0.95)
		expect_equal(result, "0.85 (0.75, 0.95)")
	})

	it("format_spec accepts custom null_format", {
		spec <- format_spec("xx.x", null_format = "N/A")
		result <- apply_format(spec, NA_real_)
		expect_equal(result, "N/A")
	})

	it("apply_format handles vector input", {
		values <- c(1.5, 2.5, NA, 4.5)
		result <- apply_format("xx.x", values)

		expect_length(result, 4)
		expect_equal(result[3], "--")
	})

	it("ClinicalPlot validates plot type", {
		expect_error(
			ClinicalPlot(plot = 1, data = NULL, type = "bad", title = "Bad"),
			"ggplot"
		)
	})

	it("format_content errors on unsupported formats", {
		tbl_data <- head(mtcars)
		ft <- flextable::flextable(tbl_data)
		tbl <- ClinicalTable(
			data = tbl_data,
			flextable = ft,
			type = "test",
			title = "Test"
		)

		expect_error(
			format_content(tbl, "csv"),
			"Unsupported format"
		)

		# Create a ClinicalPlot with a simple ggplot2 plot
		plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
			ggplot2::geom_point()
		cplot <- ClinicalPlot(
			plot = plot_obj,
			data = mtcars,
			type = "scatter",
			title = "Test"
		)

		expect_error(
			format_content(cplot, "jpg"),
			"Unsupported format"
		)
	})

	# Tests for round_half_up (indirect testing through format_number) ----
	# Note: round_half_up is an internal function, so we test it indirectly
	# through format_number which uses it internally

	it("format_number uses round_half_up for 0.5 values", {
		# Test that 0.5 values round up (not to even like default R rounding)
		# 0.5 should round to 1
		expect_equal(format_number(0.5, digits = 0), "1")
		# 1.5 should round to 2
		expect_equal(format_number(1.5, digits = 0), "2")
	})

	it("format_number uses round_half_up for -0.5 values", {
		# Test that -0.5 rounds to -1 (not to 0)
		expect_equal(format_number(-0.5, digits = 0), "-1")
	})

	it("format_number uses round_half_up with digits parameter", {
		expect_equal(format_number(0.55, digits = 1), "0.6")
		expect_equal(format_number(0.555, digits = 2), "0.56")
		expect_equal(format_number(1.25, digits = 1), "1.3")
	})

	it("format_number uses round_half_up for edge cases", {
		# Values below .5 should round down
		expect_equal(format_number(0.4, digits = 0), "0")
		expect_equal(format_number(0.49, digits = 0), "0")

		# Values at or above .5 should round up
		expect_equal(format_number(0.5, digits = 0), "1")
		expect_equal(format_number(0.51, digits = 0), "1")

		# Negative values
		# Note: sprintf formats -0.4 as "-0" due to R's sprintf behavior
		expect_equal(format_number(-0.4, digits = 0), "-0")
		expect_equal(format_number(-0.6, digits = 0), "-1")
	})

	it("format_number uses round_half_up with vectors", {
		result <- format_number(c(0.5, 1.5, 2.5), digits = 0)
		expect_equal(result, c("1", "2", "3"))
	})

	# Tests for format_number with trim=TRUE ----

	it("format_number with trim=TRUE removes trailing zeros", {
		expect_equal(format_number(1.500, digits = 2, trim = TRUE), "1.5")
		expect_equal(format_number(1.250, digits = 2, trim = TRUE), "1.25")
		expect_equal(format_number(1.000, digits = 2, trim = TRUE), "1")
	})

	it("format_number with trim=TRUE removes trailing decimal point", {
		expect_equal(format_number(5.000, digits = 2, trim = TRUE), "5")
		expect_equal(format_number(5.0, digits = 1, trim = TRUE), "5")
	})

	it("format_number with trim=TRUE keeps zeros when trim=FALSE", {
		expect_equal(format_number(1.500, digits = 2, trim = FALSE), "1.50")
		expect_equal(format_number(1.000, digits = 2, trim = FALSE), "1.00")
	})

	it("format_number with trim=TRUE works with vectors", {
		result <- format_number(c(1.500, 2.250, 3.000), digits = 2, trim = TRUE)
		expect_equal(result, c("1.5", "2.25", "3"))
	})

	it("format_number with trim=TRUE handles negative numbers", {
		expect_equal(format_number(-1.500, digits = 2, trim = TRUE), "-1.5")
		expect_equal(format_number(-1.000, digits = 2, trim = TRUE), "-1")
	})

	# Tests for FormatSpec validator errors ----

	it("format_spec rejects invalid neg_format values", {
		expect_error(
			format_spec("xx.x", neg_format = "invalid"),
			"neg_format must be 'sign', 'parens', or 'abs'"
		)

		expect_error(
			format_spec("xx.x", neg_format = "minus"),
			"neg_format must be 'sign', 'parens', or 'abs'"
		)

		expect_error(
			format_spec("xx.x", neg_format = ""),
			"neg_format must be 'sign', 'parens', or 'abs'"
		)
	})

	it("format_spec accepts valid neg_format values", {
		expect_no_error(format_spec("xx.x", neg_format = "sign"))
		expect_no_error(format_spec("xx.x", neg_format = "parens"))
		expect_no_error(format_spec("xx.x", neg_format = "abs"))
	})

	it("format_spec validates pattern characters", {
		# Invalid patterns
		expect_error(format_spec("invalid!pattern"), "must contain only")
		expect_error(format_spec("pattern$with$special"), "must contain only")
		expect_error(format_spec("pattern with spaces"), "must contain only")

		# Valid patterns
		expect_no_error(format_spec("xx.x"))
		expect_no_error(format_spec("a.a"))
		expect_no_error(format_spec("xx.xxx"))
		expect_no_error(format_spec("a.x+1"))
		expect_no_error(format_spec("123"))
	})
})
