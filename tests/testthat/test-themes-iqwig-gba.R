# Tests for IQWiG and G-BA theme functions

describe("themes-iqwig-gba", {
	it("theme_iqwig returns a flextable", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])
		result <- theme_iqwig(ft)

		expect_s3_class(result, "flextable")
	})

	it("theme_iqwig applies correct styling", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])
		result <- theme_iqwig(ft, autofit = FALSE)

		# Check it's a valid flextable
		expect_s3_class(result, "flextable")

		# The table should have content
		expect_equal(flextable::nrow_part(result, "body"), 5)
		expect_equal(flextable::ncol_keys(result), 4)
	})

	it("theme_iqwig validates input", {
		expect_error(
			theme_iqwig("not a flextable"),
			"must be a flextable"
		)
	})

	it("theme_iqwig respects autofit parameter", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])

		# Should not error with autofit = TRUE or FALSE
		expect_no_error(theme_iqwig(ft, autofit = TRUE))
		expect_no_error(theme_iqwig(ft, autofit = FALSE))
	})

	it("theme_iqwig accepts custom parameters", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])

		# Custom font
		expect_no_error(theme_iqwig(ft, font_name = "Times New Roman"))

		# Custom font size
		expect_no_error(theme_iqwig(ft, font_size = 11))

		# No bold header
		expect_no_error(theme_iqwig(ft, header_bold = FALSE))
	})

	it("theme_gba returns a flextable", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])
		result <- theme_gba(ft)

		expect_s3_class(result, "flextable")
	})

	it("theme_gba applies correct styling", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])
		result <- theme_gba(ft, autofit = FALSE)

		# Check it's a valid flextable
		expect_s3_class(result, "flextable")

		# The table should have content
		expect_equal(flextable::nrow_part(result, "body"), 5)
		expect_equal(flextable::ncol_keys(result), 4)
	})

	it("theme_gba validates input", {
		expect_error(
			theme_gba("not a flextable"),
			"must be a flextable"
		)
	})

	it("theme_gba respects autofit parameter", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])

		# Should not error with autofit = TRUE or FALSE
		expect_no_error(theme_gba(ft, autofit = TRUE))
		expect_no_error(theme_gba(ft, autofit = FALSE))
	})

	it("theme_gba accepts custom parameters", {
		ft <- flextable::flextable(mtcars[1:5, 1:4])

		# Custom header background
		expect_no_error(theme_gba(ft, header_bg = "#CCCCCC"))

		# Custom font size
		expect_no_error(theme_gba(ft, font_size = 12))

		# No bold header
		expect_no_error(theme_gba(ft, header_bold = FALSE))
	})

	it("themes handle data with NA values", {
		df <- data.frame(
			a = c(1, 2, NA),
			b = c("x", NA, "z"),
			stringsAsFactors = FALSE
		)
		ft <- flextable::flextable(df)

		# Both themes should handle NA values without error
		expect_no_error(theme_iqwig(ft))
		expect_no_error(theme_gba(ft))
	})

	it("themes handle empty data gracefully", {
		# Single row
		df <- data.frame(a = 1, b = "x")
		ft <- flextable::flextable(df)

		expect_no_error(theme_iqwig(ft))
		expect_no_error(theme_gba(ft))
	})
})
