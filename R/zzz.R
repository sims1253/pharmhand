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
#' @importFrom tidyselect where any_of all_of
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

  # Set default options for FunctionReport
  options(
    # Performance settings
    FunctionReport.docx_batch_size = 50,
    FunctionReport.parallel_threshold = 5,
    FunctionReport.workers = NULL,

    # Cache settings
    FunctionReport.cache_enabled = TRUE,
    FunctionReport.cache_max_size_mb = 500,

    # Image settings
    FunctionReport.default_plot_dpi = 300,
    FunctionReport.default_plot_width = 6,
    FunctionReport.default_plot_height = 4
  )
}
