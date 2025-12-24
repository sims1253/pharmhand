#' S7 Configuration Classes for Clinical Reports
#'
#' S7 classes for managing configuration of clinical study reports.
#' Supports loading from YAML files and runtime overrides.
#'
#' @name config_classes
NULL

#' Abstract base class for all configuration objects
#'
#' @keywords internal
Configuration <- S7::new_class(
  "Configuration",
  package = "FunctionReport",
  properties = list(
    source = S7::new_property(S7::class_character, default = "yaml"),
    priority = S7::new_property(S7::class_integer, default = 0L)
  ),
  abstract = TRUE
)

#' SubgroupConfig Class
#'
#' Configuration for analysis subgroups (e.g., age groups, sex, race).
#'
#' @param variable Character string specifying the variable name in the data
#' @param labels Named list mapping variable values to display labels
#' @param order Character vector specifying the display order
#' @param filter_values Optional character vector of values to include
#' @param source Character string for configuration source
#' @param priority Integer priority (higher overrides lower)
#'
#' @return A SubgroupConfig object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- SubgroupConfig(
#'   variable = "AGEGR1",
#'   labels = list(Y_LT65 = "< 65", Y_GE65 = ">= 65")
#' )
#' }
SubgroupConfig <- S7::new_class(
  "SubgroupConfig",
  package = "FunctionReport",
  parent = Configuration,
  properties = list(
    variable = S7::new_property(S7::class_character),
    labels = S7::new_property(S7::class_list, default = list()),
    order = S7::new_property(S7::class_any),
    filter_values = S7::new_property(S7::class_any)
  )
)

#' PopulationConfig Class
#'
#' Configuration for analysis populations (e.g., SAF, FAS, PPS).
#'
#' @param variable Character string specifying the variable name in the data
#' @param label Character string for population display label
#' @param description Character string describing the population
#' @param flag_value Character string value indicating membership (default "Y")
#' @param source Character string for configuration source
#' @param priority Integer priority
#'
#' @return A PopulationConfig object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- PopulationConfig(
#'   variable = "SAFFL",
#'   label = "Safety Analysis Set",
#'   description = "Subjects who received at least one dose"
#' )
#' }
PopulationConfig <- S7::new_class(
  "PopulationConfig",
  package = "FunctionReport",
  parent = Configuration,
  properties = list(
    variable = S7::new_property(S7::class_character),
    label = S7::new_property(S7::class_character),
    description = S7::new_property(S7::class_any),
    flag_value = S7::new_property(S7::class_character, default = "Y")
  )
)

#' SOCConfig Class
#'
#' Configuration for System Organ Class (SOC) handling in adverse events.
#'
#' @param variable Character string for SOC variable name (e.g., "AEBODSYS")
#' @param include_all Logical, include all SOCs found in data
#' @param custom_order Character vector for custom SOC ordering
#' @param sort_by Character string for sorting method ("frequency" or "alphabetical")
#' @param min_subjects Numeric minimum subjects required to include SOC
#' @param top_n Numeric maximum number of SOCs (NULL = no limit)
#' @param source Character string for configuration source
#' @param priority Integer priority
#'
#' @return An SOCConfig object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- SOCConfig(
#'   variable = "AEBODSYS",
#'   sort_by = "frequency"
#' )
#' }
SOCConfig <- S7::new_class(
  "SOCConfig",
  package = "FunctionReport",
  parent = Configuration,
  properties = list(
    variable = S7::new_property(S7::class_character),
    include_all = S7::new_property(S7::class_logical, default = TRUE),
    custom_order = S7::new_property(S7::class_any),
    sort_by = S7::new_property(S7::class_character, default = "frequency"),
    min_subjects = S7::new_property(S7::class_numeric, default = 1),
    top_n = S7::new_property(S7::class_any)
  )
)

#' PTConfig Class
#'
#' Configuration for Preferred Term (PT) handling in adverse events.
#'
#' @param variable Character string for PT variable name (e.g., "AEDECOD")
#' @param include_all Logical, include all PTs found in data
#' @param sort_by Character string for sorting method ("frequency" or "alphabetical")
#' @param min_subjects Numeric minimum subjects required to include PT
#' @param top_n_per_soc Numeric maximum PTs per SOC (NULL = no limit)
#' @param show_pt_codes Logical, show PT codes alongside names
#' @param source Character string for configuration source
#' @param priority Integer priority
#'
#' @return A PTConfig object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- PTConfig(
#'   variable = "AEDECOD",
#'   sort_by = "frequency"
#' )
#' }
PTConfig <- S7::new_class(
  "PTConfig",
  package = "FunctionReport",
  parent = Configuration,
  properties = list(
    variable = S7::new_property(S7::class_character),
    include_all = S7::new_property(S7::class_logical, default = TRUE),
    sort_by = S7::new_property(S7::class_character, default = "frequency"),
    min_subjects = S7::new_property(S7::class_numeric, default = 1),
    top_n_per_soc = S7::new_property(S7::class_any),
    show_pt_codes = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' ConfigurationRegistry Class
#'
#' Central registry for all configuration objects. Manages subgroups,
#' populations, SOC/PT settings, report templates, and performance settings.
#'
#' @param subgroups Named list of SubgroupConfig objects
#' @param populations Named list of PopulationConfig objects
#' @param soc_config An SOCConfig object
#' @param pt_config A PTConfig object
#' @param report_types List of report type configurations
#' @param performance List of performance settings
#' @param plots List of plot styling settings
#' @param tables List of table styling settings
#' @param validation List of validation settings
#'
#' @return A ConfigurationRegistry object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' # Override a subgroup at runtime
#' registry <- define_subgroup_config(registry, "custom", "VAR1")
#' }
ConfigurationRegistry <- S7::new_class(
  "ConfigurationRegistry",
  package = "FunctionReport",
  properties = list(
    subgroups = S7::new_property(S7::class_list, default = list()),
    populations = S7::new_property(S7::class_list, default = list()),
    soc_config = S7::new_property(S7::class_any),
    pt_config = S7::new_property(S7::class_any),
    report_types = S7::new_property(S7::class_list, default = list()),
    performance = S7::new_property(S7::class_list, default = list()),
    plots = S7::new_property(S7::class_list, default = list()),
    tables = S7::new_property(S7::class_list, default = list()),
    validation = S7::new_property(S7::class_list, default = list())
  )
)
