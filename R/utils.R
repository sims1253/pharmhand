# =============================================================================
# Internal validation helpers (replacing cli dependency)
# =============================================================================

#' @keywords internal
ph_abort <- function(...) {
	args <- list(...)
	args$call. <- FALSE
	do.call(stop, args)
}

#' @keywords internal
ph_warn <- function(...) {
	args <- list(...)
	args$call. <- FALSE
	do.call(warning, args)
}

#' @keywords internal
ph_inform <- function(...) {
	args <- list(...)
	do.call(message, args)
}

#' @keywords internal
assert_data_frame <- function(x, arg = deparse(substitute(x))) {
	if (!is.data.frame(x)) {
		ph_abort(sprintf("'%s' must be a data frame", arg))
	}
	invisible(x)
}

#' @keywords internal
assert_numeric_scalar <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1) {
		ph_abort(sprintf("'%s' must be a single numeric value", arg))
	}
	invisible(x)
}

#' @keywords internal
assert_character_scalar <- function(x, arg = deparse(substitute(x))) {
	if (!is.character(x) || length(x) != 1 || is.na(x) || nchar(x) == 0) {
		ph_abort(sprintf("'%s' must be a non-empty character string", arg))
	}
	invisible(x)
}

#' @keywords internal
assert_in_range <- function(x, lower, upper, arg = deparse(substitute(x))) {
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

#' @keywords internal
assert_positive <- function(x, arg = deparse(substitute(x))) {
	if (!is.numeric(x) || length(x) != 1 || x <= 0) {
		ph_abort(sprintf("'%s' must be a single positive number", arg))
	}
	invisible(x)
}

#' @keywords internal
assert_column_exists <- function(data, col, data_arg = "data") {
	if (!col %in% names(data)) {
		ph_abort(sprintf("Column '%s' not found in '%s'", col, data_arg))
	}
	invisible(TRUE)
}
