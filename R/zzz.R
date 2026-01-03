#' S7 Method Registration and Package Initialization
#'
#' This file handles the registration of S7 methods when the package is loaded.
#' S7 requires explicit method registration in `.onLoad()` for proper dispatch.
#'
#' @name S7_registration
NULL

# Silence R CMD check warnings for global variables
utils::globalVariables(c(
	".data",
	"where",
	"level",
	"PT",
	"SOC",
	"variable",
	"value",
	"N_tot",
	"pct",
	"AEBODSYS",
	"AEDECOD",
	"USUBJID",
	"TRT01P",
	"n",
	"mean",
	"sd",
	"median",
	"min",
	"max"
))

#' @importFrom stats sd median quantile setNames as.formula na.omit
#' @importFrom rlang %||% .data
#' @importFrom admiraldev assert_data_frame assert_character_scalar
#'   assert_character_vector assert_numeric_vector
NULL
#' @title Package initialization hook
#' @description Called when the package is loaded. Registers S7 methods and
#' sets default options.
#' @param libname Directory containing the package
#' @param pkgname Name of the package
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
	# Register all S7 methods
	# This is critical for proper generic dispatch
	S7::methods_register()

	# Set default options for pharmhand (only if not already set by user)
	set_option_if_null <- function(name, value) {
		if (is.null(getOption(name))) {
			do.call(options, stats::setNames(list(value), name))
		}
	}

	# Performance settings
	set_option_if_null("pharmhand.docx_batch_size", 50)
	set_option_if_null("pharmhand.parallel_threshold", 5)
	set_option_if_null("pharmhand.workers", NULL)

	# Cache settings
	set_option_if_null("pharmhand.cache_enabled", TRUE)
	set_option_if_null("pharmhand.cache_max_size_mb", 500)

	# Formatting settings
	set_option_if_null("pharmhand.na_string", "--")

	# Image settings
	set_option_if_null("pharmhand.default_plot_dpi", 300)
	set_option_if_null("pharmhand.default_plot_width", 6)
	set_option_if_null("pharmhand.default_plot_height", 4)
}
