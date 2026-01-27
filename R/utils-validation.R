# =============================================================================
# Validation and Assertion Helpers
# =============================================================================
# Internal validation helpers for the pharmhand package.
# These functions provide consistent, robust validation across all functions.

# =============================================================================
# Core Messaging Functions
# =============================================================================

#' Abort with Error
#'
#' Internal wrapper for rlang::abort() that provides consistent error formatting.
#'
#' @param ... Arguments passed to rlang::abort()
#'
#' @keywords internal
ph_abort <- function(...) {
	rlang::abort(paste0(...), call = NULL)
}

#' Issue Warning
#'
#' Internal wrapper for warning() that provides consistent warning formatting.
#'
#' @param ... Arguments passed to warning()
#'
#' @keywords internal
ph_warn <- function(...) {
	args <- list(...)
	args$call. <- FALSE
	do.call(warning, args)
}

#' Inform User
#'
#' Internal wrapper for message() that provides consistent message formatting.
#'
#' @param ... Arguments passed to message()
#'
#' @keywords internal
ph_inform <- function(...) {
	args <- list(...)
	do.call(message, args)
}

# =============================================================================
# Scalar Assertions
# =============================================================================

#' Assert Numeric Scalar
#'
#' Validates that x is a single numeric value (length 1).
#' NA values are rejected.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_numeric_scalar <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a single numeric value", arg))
	}
	if (is.na(x)) {
		ph_abort(sprintf("'%s' must be a single numeric value", arg))
	}
	invisible(x)
}

#' Assert Character Scalar
#'
#' Validates that x is a single character string.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#' @param allow_empty Logical. If TRUE, empty strings are allowed.
#'   Default: FALSE
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_character_scalar <- function(
	x,
	arg = deparse(substitute(x)),
	allow_empty = FALSE
) {
	if (!is.character(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a non-empty character string", arg))
	}
	if (is.na(x) || (!allow_empty && nchar(x) == 0)) {
		ph_abort(sprintf("'%s' must be a non-empty character string", arg))
	}
	invisible(x)
}

#' Assert Logical Scalar
#'
#' Validates that x is a single logical value.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_logical_scalar <- function(x, arg = deparse(substitute(x))) {
	if (!is.logical(x) || length(x) != 1 || is.na(x)) {
		ph_abort(sprintf("'%s' must be a single logical value (TRUE/FALSE)", arg))
	}
	invisible(x)
}

#' Assert Integer Scalar
#'
#' Validates that x is a single integer value.
#' NA values are rejected.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_integer_scalar <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a single integer value", arg))
	}
	if (is.na(x)) {
		ph_abort(sprintf("'%s' must be a single integer value", arg))
	}
	if (x != as.integer(x)) {
		ph_abort(sprintf("'%s' must be a single integer value", arg))
	}
	invisible(x)
}

#' Assert Positive Integer Scalar
#'
#' Validates that x is a single positive integer value (>= 1).
#' NA values are rejected.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_positive_integer <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a single numeric value", arg))
	}
	if (is.na(x)) {
		ph_abort(sprintf("'%s' must be a single numeric value", arg))
	}
	if (x < 1 || x != as.integer(x)) {
		ph_abort(sprintf("'%s' must be a positive integer", arg))
	}
	invisible(x)
}

#' Assert Non-Negative Integer Scalar
#'
#' Validates that x is a single non-negative integer value (>= 0).
#' NA values are rejected.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_non_negative_integer <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a single numeric value", arg))
	}
	if (is.na(x)) {
		ph_abort(sprintf("'%s' must be a single numeric value", arg))
	}
	if (x < 0 || x != as.integer(x)) {
		ph_abort(sprintf("'%s' must be a non-negative integer", arg))
	}
	invisible(x)
}

# =============================================================================
# Vector Assertions
# =============================================================================

#' Assert Numeric Vector
#'
#' Validates that x is a numeric vector.
#'
#' @param x Value to check
#' @param len Optional integer specifying required length. If NULL, any length
#'   is allowed
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_numeric_vector <- function(x, len = NULL, arg = deparse(substitute(x))) {
	if (!is.numeric(x)) {
		ph_abort(sprintf("'%s' must be numeric", arg))
	}
	if (!is.null(len) && length(x) != len) {
		ph_abort(sprintf("'%s' must have length %d, got %d", arg, len, length(x)))
	}
	invisible(x)
}

#' Assert Character Vector
#'
#' Validates that x is a character vector.
#'
#' @param x Value to check
#' @param len Optional integer specifying required length. If NULL, any length
#'   is allowed
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_character_vector <- function(
	x,
	len = NULL,
	arg = deparse(substitute(x))
) {
	if (!is.character(x)) {
		ph_abort(sprintf("'%s' must be a character vector", arg))
	}
	if (!is.null(len) && length(x) != len) {
		ph_abort(sprintf("'%s' must have length %d, got %d", arg, len, length(x)))
	}
	invisible(x)
}

# =============================================================================
# Data Frame Assertions
# =============================================================================

#' Assert Data Frame
#'
#' Validates that x is a data frame and optionally checks for required columns.
#'
#' @param x Value to check
#' @param required_cols Optional character vector of required column names
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_data_frame <- function(
	x,
	required_cols = NULL,
	arg = deparse(substitute(x))
) {
	if (!is.data.frame(x)) {
		ph_abort(sprintf("'%s' must be a data frame", arg))
	}
	if (!is.null(required_cols)) {
		missing_cols <- setdiff(required_cols, names(x))
		if (length(missing_cols) > 0) {
			ph_abort(sprintf(
				"'%s' is missing required columns: %s",
				arg,
				paste(missing_cols, collapse = ", ")
			))
		}
	}
	invisible(x)
}

# =============================================================================
# Numeric Assertions
# =============================================================================

#' Assert In Range
#'
#' Validates that x is within the specified range (exclusive bounds).
#' NA values are rejected.
#'
#' @param x Value to check
#' @param lower Lower bound (exclusive)
#' @param upper Upper bound (exclusive)
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_in_range <- function(
	x,
	lower = -Inf,
	upper = Inf,
	arg = deparse(substitute(x))
) {
	if (anyNA(x)) {
		ph_abort(sprintf("'%s' must not contain NA values", arg))
	}
	if (x <= lower || x >= upper) {
		ph_abort(sprintf(
			"'%s' must be greater than %s and less than %s",
			arg,
			lower,
			upper
		))
	}
	invisible(x)
}

#' Assert All Positive
#'
#' Validates that all elements in x are positive (> 0).
#' NA values are rejected.
#'
#' @param x Numeric vector to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_all_positive <- function(x, arg = deparse(substitute(x))) {
	if (anyNA(x)) {
		ph_abort(sprintf("'%s' must not contain NA values", arg))
	}
	if (!is.numeric(x)) {
		ph_abort(sprintf("'%s' must be numeric", arg))
	}
	if (any(x <= 0)) {
		ph_abort(sprintf("'%s' must contain only positive values", arg))
	}
	invisible(x)
}

#' Assert No NA
#'
#' Validates that x does not contain any NA values.
#'
#' @param x Vector to check (any atomic type)
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_no_na <- function(x, arg = deparse(substitute(x))) {
	if (anyNA(x)) {
		ph_abort(sprintf("'%s' must not contain NA values", arg))
	}
	invisible(x)
}

# =============================================================================
# Structural Assertions
# =============================================================================

#' Assert Lengths Match
#'
#' Validates that all provided vectors have the same length.
#'
#' @param ... Vectors to check (at least two required)
#'
#' @return Invisibly returns TRUE if validation passes
#'
#' @keywords internal
assert_lengths_match <- function(...) {
	vectors <- list(...)
	if (length(vectors) < 2) {
		ph_abort("assert_lengths_match() requires at least two vectors")
	}

	lengths <- lengths(vectors)
	if (length(unique(lengths)) > 1) {
		ph_abort(sprintf(
			"All vectors must have the same length, got: %s",
			paste(paste0("length=", lengths), collapse = ", ")
		))
	}

	invisible(TRUE)
}

#' Assert Positive
#'
#' Validates that x is a single positive number (> 0).
#' NA values are rejected.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @keywords internal
assert_positive <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a single positive number", arg))
	}
	if (is.na(x)) {
		ph_abort(sprintf("'%s' must be a single positive number", arg))
	}
	if (x <= 0) {
		ph_abort(sprintf("'%s' must be a single positive number", arg))
	}
	invisible(x)
}

#' Assert Column Exists
#'
#' Validates that a specified column exists in a data frame.
#'
#' @param data Data frame to check
#' @param col Character string name of the column to check
#' @param data_arg Character string name of the data argument (for error
#'   messages)
#'
#' @return Invisibly returns TRUE if validation passes
#'
#' @keywords internal
assert_column_exists <- function(data, col, data_arg = "data") {
	if (!col %in% names(data)) {
		ph_abort(sprintf("Column '%s' not found in '%s'", col, data_arg))
	}
	invisible(TRUE)
}

# =============================================================================
# Exported Helper for Non-Negative Numbers
# =============================================================================

#' Assert Non-Negative
#'
#' Validates that x is a single non-negative number (>= 0).
#' NA values are rejected.
#'
#' @param x Value to check
#' @param arg Character string describing the argument (for error messages)
#'
#' @return Invisibly returns x if validation passes
#'
#' @export
assert_non_negative <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a single non-negative number", arg))
	}
	if (is.na(x)) {
		ph_abort(sprintf("'%s' must be a single non-negative number", arg))
	}
	if (x < 0) {
		ph_abort(sprintf("'%s' must be a single non-negative number", arg))
	}
	invisible(x)
}
