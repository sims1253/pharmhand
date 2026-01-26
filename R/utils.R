# =============================================================================
# Package Defaults System
# =============================================================================

#' Null Coalescing Operator
#'
#' Return first value if not NULL, otherwise return second value.
#' This is a common pattern in functional programming for providing defaults.
#'
#' @param x First value to check
#' @param y Second value to return if x is NULL
#'
#' @return x if not NULL, otherwise y
#'
#' @name grapes-or-or-grapes
#' @aliases %||%
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' NULL %||% "default"  # "default"
#' "value" %||% "default"  # "value"
#' }
`%||%` <- function(x, y) {
	if (is.null(x)) y else x
}

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
	assert_character_scalar(name, "name")

	# Fall back to built-in default
	builtin <- .PH_DEFAULTS[[name]]

	# Check option first (allows user override), but validate type/shape.
	opt_value <- getOption(paste0("pharmhand.", name))
	if (!is.null(opt_value)) {
		expected <- if (!is.null(builtin)) builtin else default

		is_scalar_atomic <- function(x) {
			is.atomic(x) && length(x) == 1
		}

		ok <- TRUE
		if (!is.null(expected)) {
			if (is.logical(expected) && is_scalar_atomic(expected)) {
				ok <- is.logical(opt_value) &&
					is_scalar_atomic(opt_value) &&
					!is.na(opt_value)
			} else if (is.numeric(expected) && is_scalar_atomic(expected)) {
				ok <- is.numeric(opt_value) &&
					is_scalar_atomic(opt_value) &&
					!is.na(opt_value)
			} else if (is.character(expected) && is_scalar_atomic(expected)) {
				ok <- is.character(opt_value) &&
					is_scalar_atomic(opt_value) &&
					!is.na(opt_value)
			} else {
				ok <- inherits(opt_value, class(expected))
			}
		}

		if (isTRUE(ok)) {
			return(opt_value)
		}

		ph_warn(sprintf(
			"Invalid option 'pharmhand.%s' (expected %s, got %s); using default",
			name,
			if (is.null(expected)) {
				"<unknown>"
			} else {
				paste(class(expected), collapse = "/")
			},
			paste(class(opt_value), collapse = "/")
		))
	}

	if (!is.null(builtin)) {
		return(builtin)
	}

	default
}

# =============================================================================
# NA Display Helpers
# =============================================================================

#' Get NA String
#'
#' Returns the string to display for NA values.
#'
#' @return Character string for NA display
#' @keywords internal
get_na_string <- function() {
	ph_default("na_string", "--")
}

# =============================================================================
# Data Frame Utilities
# =============================================================================

#' Get Optional Column Value
#'
#' Safely extract a column value, returning a default if not present or NA.
#'
#' @param row A data frame row or named list
#' @param col_name Character. Column name to extract.
#' @param default Default value if column missing or NA. Default: ""
#'
#' @return The column value or default
#' @keywords internal
.get_optional_col <- function(row, col_name, default = "") {
	if (col_name %in% names(row) && !is.na(row[[col_name]])) {
		return(as.character(row[[col_name]]))
	}
	return(default)
}
