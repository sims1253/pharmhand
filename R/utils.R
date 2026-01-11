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

#' Get NA String
#'
#' Returns the string to display for NA values.
#'
#' @return Character string for NA display
#' @keywords internal
get_na_string <- function() {
	getOption("pharmhand.na_string", "--")
}

# =============================================================================
# Package Defaults System
# =============================================================================

#' Package-wide default values
#'
#' Central registry of default parameter values used across the package.
#' These can be overridden using options("pharmhand.<name>", value).
#'
#' @keywords internal
.PH_DEFAULTS <- list(
	trt_var = "TRT01P",
	subject_var = "USUBJID",
	autofit = TRUE,
	conf_level = 0.95,
	ci_level = 0.95,
	na_string = "--",
	population = "FAS",
	n_top = 15,
	threshold = 0.1,
	digits = 2,
	locale = "en"
)

#' Get Package Default Value
#'
#' Retrieves a default parameter value, checking options first,
#' then falling back to built-in defaults.
#'
#' @param name Character string name of the parameter
#' @param default Optional override default if both option and built-in are NULL
#'
#' @return The default value for the parameter
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get default treatment variable
#' ph_default("trt_var") # "TRT01P"
#'
#' # Override via option
#' options(pharmhand.trt_var = "ARM")
#' ph_default("trt_var") # "ARM"
#' }
ph_default <- function(name, default = NULL) {
	# Check option first (allows user override)
	opt_value <- getOption(paste0("pharmhand.", name))
	if (!is.null(opt_value)) {
		return(opt_value)
	}

	# Fall back to built-in default
	builtin <- .PH_DEFAULTS[[name]]
	if (!is.null(builtin)) {
		return(builtin)
	}

	# Use provided default or NULL
	default
}
