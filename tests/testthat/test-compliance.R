# Tests for G-BA compliance checks

describe("compliance", {
	it("check_gba_compliance passes for valid data.frame", {
		df <- data.frame(Statistic = "n", Value = 10)
		res <- check_gba_compliance(df, strict = FALSE, require_theme = FALSE)

		expect_true(res$ok)
		expect_length(res$errors, 0)
	})

	it("check_gba_compliance flags duplicate column names", {
		df <- data.frame(a = 1, b = 2, check.names = FALSE)
		names(df) <- c("a", "a")
		res <- check_gba_compliance(df, strict = FALSE, require_theme = FALSE)

		expect_false(res$ok)
		expect_true(any(grepl("duplicate column names", res$errors, fixed = TRUE)))
	})

	it("check_gba_compliance flags empty column names", {
		df <- data.frame(x = 1, check.names = FALSE)
		names(df) <- ""
		res <- check_gba_compliance(df, strict = FALSE, require_theme = FALSE)

		expect_false(res$ok)
		expect_true(any(grepl("empty column names", res$errors, fixed = TRUE)))
	})

	it("check_gba_compliance validates flextable theme marker", {
		df <- data.frame(Statistic = "n", Value = 10)
		ft <- flextable::flextable(df)

		res <- check_gba_compliance(ft, strict = FALSE, require_theme = TRUE)
		expect_false(res$ok)
		expect_true(any(grepl(
			"missing G-BA theme marker",
			res$errors,
			fixed = TRUE
		)))

		ft_gba <- theme_gba(ft, autofit = FALSE)
		res_gba <- check_gba_compliance(
			ft_gba,
			strict = FALSE,
			require_theme = TRUE
		)
		expect_true(res_gba$ok)
	})

	it("check_gba_compliance handles ClinicalTable", {
		df <- data.frame(Statistic = "n", Value = 10)
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)
		table <- ClinicalTable(data = df, flextable = ft, type = "test")

		res <- check_gba_compliance(table, strict = FALSE)
		expect_true(res$ok)
	})

	it("check_gba_compliance errors on invalid input in strict mode", {
		expect_error(
			check_gba_compliance("invalid", strict = TRUE),
			"must be a ClinicalTable, flextable, or data.frame"
		)
	})

	it("check_gba_compliance handles list input", {
		valid <- data.frame(Statistic = "n", Value = 10)
		invalid <- data.frame(a = 1, b = 2, check.names = FALSE)
		names(invalid) <- c("a", "a")

		res <- check_gba_compliance(
			list(valid = valid, invalid = invalid),
			strict = FALSE,
			require_theme = FALSE
		)

		expect_false(res$ok)
		expect_true(any(grepl("duplicate column names", res$errors, fixed = TRUE)))
	})

	it("check_gba_compliance enforces title when required", {
		df <- data.frame(Statistic = "n", Value = 10)
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = TRUE,
			require_title = TRUE
		)
		expect_false(res$ok)
		expect_true(any(grepl("title", res$errors, fixed = TRUE)))
	})

	# Tests for flextable header/body requirements ----

	it("check_gba_compliance flags missing header row in flextable", {
		# Create a flextable and remove its header
		df <- data.frame(Statistic = "n", Value = 10)
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

		# Remove header part
		ft <- flextable::delete_part(ft, part = "header")

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = FALSE,
			require_header = TRUE
		)
		expect_false(res$ok)
		expect_true(any(grepl("no header row", res$errors, fixed = TRUE)))
	})

	it("check_gba_compliance passes when header present", {
		df <- data.frame(Statistic = "n", Value = 10)
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = FALSE,
			require_header = TRUE
		)
		expect_true(res$ok)
	})

	it("check_gba_compliance flags missing body row in flextable", {
		# Create a flextable with empty data
		df <- data.frame(Statistic = character(), Value = numeric())
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = FALSE,
			require_body = TRUE
		)
		expect_false(res$ok)
		expect_true(any(grepl("no body rows", res$errors, fixed = TRUE)))
	})

	it("check_gba_compliance passes when body present", {
		df <- data.frame(Statistic = "n", Value = 10)
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = FALSE,
			require_body = TRUE
		)
		expect_true(res$ok)
	})

	# Tests for empty data handling ----

	it("check_gba_compliance flags data frame with no columns", {
		df <- data.frame()

		res <- check_gba_compliance(
			df,
			strict = FALSE,
			require_theme = FALSE
		)
		expect_false(res$ok)
		expect_true(any(grepl("has no columns", res$errors, fixed = TRUE)))
	})

	it("check_gba_compliance passes data frame with columns but no rows", {
		df <- data.frame(Statistic = character(), Value = numeric())

		res <- check_gba_compliance(
			df,
			strict = FALSE,
			require_theme = FALSE
		)
		expect_true(res$ok)
	})

	it("check_gba_compliance handles flextable with empty dataset", {
		# Create flextable from empty data frame
		df <- data.frame(Statistic = character(), Value = numeric())
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = FALSE,
			require_body = FALSE
		)
		# Should be OK since we're not requiring body rows
		expect_true(res$ok)
	})

	# Tests for require_header and require_body flexibility ----

	it("check_gba_compliance can skip header requirement", {
		# Create flextable without header
		df <- data.frame(Statistic = "n", Value = 10)
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)
		ft <- flextable::delete_part(ft, part = "header")

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = FALSE,
			require_header = FALSE
		)
		expect_true(res$ok)
	})

	it("check_gba_compliance can skip body requirement", {
		# Create flextable without body
		df <- data.frame(Statistic = character(), Value = numeric())
		ft <- theme_gba(flextable::flextable(df), autofit = FALSE)

		res <- check_gba_compliance(
			ft,
			strict = FALSE,
			require_theme = FALSE,
			require_body = FALSE
		)
		expect_true(res$ok)
	})
})
