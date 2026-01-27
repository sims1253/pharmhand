#'
#' Tests for internal validation helpers: ph_abort, ph_warn, ph_inform,
#' assert_data_frame, assert_numeric_scalar, assert_character_scalar,
#' assert_in_range, assert_positive, and assert_column_exists.

ph_abort <- getFromNamespace("ph_abort", "pharmhand")
ph_warn <- getFromNamespace("ph_warn", "pharmhand")
ph_inform <- getFromNamespace("ph_inform", "pharmhand")
assert_data_frame <- getFromNamespace("assert_data_frame", "pharmhand")
assert_numeric_scalar <- getFromNamespace("assert_numeric_scalar", "pharmhand")
assert_integer_scalar <- getFromNamespace("assert_integer_scalar", "pharmhand")
assert_positive_integer <- getFromNamespace(
	"assert_positive_integer",
	"pharmhand"
)
assert_non_negative_integer <- getFromNamespace(
	"assert_non_negative_integer",
	"pharmhand"
)
assert_character_scalar <- getFromNamespace(
	"assert_character_scalar",
	"pharmhand"
)
assert_in_range <- getFromNamespace("assert_in_range", "pharmhand")
assert_all_positive <- getFromNamespace("assert_all_positive", "pharmhand")
assert_positive <- getFromNamespace("assert_positive", "pharmhand")
assert_non_negative <- getFromNamespace("assert_non_negative", "pharmhand")
assert_column_exists <- getFromNamespace("assert_column_exists", "pharmhand")

describe("assertions", {
	it("ph_abort strips call and reports message", {
		err <- tryCatch(
			ph_abort("Test error message"),
			error = function(e) e
		)

		expect_s3_class(err, "error")
		expect_null(conditionCall(err))
		expect_match(conditionMessage(err), "Test error message")
	})

	it("ph_warn strips call and reports message", {
		warn <- NULL

		withCallingHandlers(
			ph_warn("Test warning message"),
			warning = function(w) {
				warn <<- w
				invokeRestart("muffleWarning")
			}
		)

		expect_s3_class(warn, "warning")
		expect_null(conditionCall(warn))
		expect_match(conditionMessage(warn), "Test warning message")
	})

	it("ph_inform strips call and reports message", {
		msg <- NULL

		withCallingHandlers(
			ph_inform("Test info message"),
			message = function(m) {
				msg <<- m
				invokeRestart("muffleMessage")
			}
		)

		expect_s3_class(msg, "message")
		expect_match(conditionMessage(msg), "Test info message")
	})

	it("assert_data_frame validates data frames", {
		df <- data.frame(x = 1:3)
		expect_identical(assert_data_frame(df), df)

		x <- 1
		expect_error(
			assert_data_frame(x),
			"'x' must be a data frame"
		)
	})

	it("assert_numeric_scalar validates numeric scalars", {
		x <- 1.5
		expect_identical(assert_numeric_scalar(x), x)

		x <- c(1, 2)
		expect_error(
			assert_numeric_scalar(x),
			"'x' must be a single numeric value"
		)

		x <- "text"
		expect_error(
			assert_numeric_scalar(x),
			"'x' must be a single numeric value"
		)
	})

	it("assert_character_scalar validates non-empty strings", {
		x <- "a"
		expect_identical(assert_character_scalar(x), x)

		x <- ""
		expect_error(
			assert_character_scalar(x),
			"'x' must be a non-empty character string"
		)

		x <- NA_character_
		expect_error(
			assert_character_scalar(x),
			"'x' must be a non-empty character string"
		)

		x <- c("a", "b")
		expect_error(
			assert_character_scalar(x),
			"'x' must be a non-empty character string"
		)

		x <- 10
		expect_error(
			assert_character_scalar(x),
			"'x' must be a non-empty character string"
		)
	})

	it("assert_in_range uses exclusive bounds", {
		x <- 2
		expect_identical(assert_in_range(x, lower = 1, upper = 3), x)

		x <- 1
		expect_error(
			assert_in_range(x, lower = 1, upper = 3),
			"'x' must be greater than 1 and less than 3"
		)

		x <- 3
		expect_error(
			assert_in_range(x, lower = 1, upper = 3),
			"'x' must be greater than 1 and less than 3"
		)
	})

	it("assert_positive validates positive numeric scalars", {
		x <- 1
		expect_identical(assert_positive(x), x)

		x <- 0
		expect_error(
			assert_positive(x),
			"'x' must be a single positive number"
		)

		x <- -1
		expect_error(
			assert_positive(x),
			"'x' must be a single positive number"
		)

		x <- c(1, 2)
		expect_error(
			assert_positive(x),
			"'x' must be a single positive number"
		)

		x <- "text"
		expect_error(
			assert_positive(x),
			"'x' must be a single positive number"
		)
	})

	it("assert_column_exists validates column names", {
		df <- data.frame(a = 1, b = 2)
		expect_identical(assert_column_exists(df, "a"), TRUE)

		expect_error(
			assert_column_exists(df, "c"),
			"Column 'c' not found in 'data'"
		)

		expect_error(
			assert_column_exists(df, "c", data_arg = "df"),
			"Column 'c' not found in 'df'"
		)
	})

	it("assert_numeric_scalar rejects NA", {
		x <- NA_real_
		expect_error(
			assert_numeric_scalar(x),
			"'x' must be a single numeric value"
		)
	})

	it("assert_integer_scalar rejects NA", {
		x <- NA_integer_
		expect_error(
			assert_integer_scalar(x),
			"'x' must be a single integer value"
		)
	})

	it("assert_positive_integer rejects NA", {
		x <- NA_real_
		expect_error(
			assert_positive_integer(x),
			"'x' must be a single numeric value"
		)
	})

	it("assert_non_negative_integer rejects NA", {
		x <- NA_real_
		expect_error(
			assert_non_negative_integer(x),
			"'x' must be a single numeric value"
		)
	})

	it("assert_positive rejects NA", {
		x <- NA_real_
		expect_error(
			assert_positive(x),
			"'x' must be a single positive number"
		)
	})

	it("assert_non_negative rejects NA", {
		x <- NA_real_
		expect_error(
			assert_non_negative(x),
			"'x' must be a single non-negative number"
		)
	})

	it("assert_in_range rejects NA", {
		x <- NA_real_
		expect_error(
			assert_in_range(x, lower = 0, upper = 10),
			"'x' must not contain NA values"
		)
	})

	it("assert_all_positive rejects NA", {
		x <- c(1, 2, NA)
		expect_error(
			assert_all_positive(x),
			"'x' must not contain NA values"
		)
	})
})
