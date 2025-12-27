#' Configuration API for Clinical Reports
#'
#' Functions for loading, managing, and modifying report configuration.
#' Supports loading from YAML files and runtime overrides.
#'
#' @name config_api
NULL

#' Load configuration from YAML file
#'
#' Loads configuration from a YAML file and creates a ConfigurationRegistry.
#' If no path is provided, loads the default configuration from the package.
#'
#' @param path Character string path to YAML configuration file.
#'   If NULL, loads the default package configuration.
#'
#' @return A ConfigurationRegistry object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load default configuration
#' registry <- load_config()
#'
#' # Load custom configuration
#' registry <- load_config("path/to/config.yaml")
#' }
load_config <- function(path = NULL) {
	if (is.null(path)) {
		path <- system.file(
			"config",
			"config_defaults.yaml",
			package = "pharmhand"
		)
	}

	if (!file.exists(path)) {
		cli::cli_abort("Configuration file not found: {path}")
	}

	config <- yaml::read_yaml(path)
	parse_config_registry(config)
}

#' Parse configuration from YAML list
#'
#' Internal function to parse YAML configuration list into
#' ConfigurationRegistry object.
#'
#' @param config List from yaml::read_yaml()
#'
#' @return A ConfigurationRegistry object
#'
#' @keywords internal
parse_config_registry <- function(config) {
	# Parse subgroups
	subgroups <- list()
	if (!is.null(config$subgroups)) {
		for (name in names(config$subgroups)) {
			sg_config <- config$subgroups[[name]]
			subgroups[[name]] <- SubgroupConfig(
				variable = sg_config$variable,
				labels = sg_config$labels %||% list(),
				order = sg_config$order,
				filter_values = sg_config$filter_values,
				source = "yaml",
				priority = 0
			)
		}
	}

	# Parse populations
	populations <- list()
	if (!is.null(config$populations)) {
		for (name in names(config$populations)) {
			pop_config <- config$populations[[name]]
			populations[[name]] <- PopulationConfig(
				variable = pop_config$variable,
				label = pop_config$label,
				description = pop_config$description,
				flag_value = pop_config$flag_value %||% "Y",
				source = "yaml",
				priority = 0
			)
		}
	}

	# Parse SOC config
	soc_config <- NULL
	if (!is.null(config$soc_config)) {
		soc_config <- SOCConfig(
			variable = config$soc_config$variable,
			include_all = config$soc_config$include_all %||% TRUE,
			custom_order = config$soc_config$custom_order,
			sort_by = config$soc_config$sort_by %||% "frequency",
			min_subjects = config$soc_config$min_subjects %||% 1,
			top_n = config$soc_config$top_n,
			source = "yaml",
			priority = 0
		)
	}

	# Parse PT config
	pt_config <- NULL
	if (!is.null(config$pt_config)) {
		pt_config <- PTConfig(
			variable = config$pt_config$variable,
			include_all = config$pt_config$include_all %||% TRUE,
			sort_by = config$pt_config$sort_by %||% "frequency",
			min_subjects = config$pt_config$min_subjects %||% 1,
			top_n_per_soc = config$pt_config$top_n_per_soc,
			show_pt_codes = config$pt_config$show_pt_codes %||% FALSE,
			source = "yaml",
			priority = 0
		)
	}

	# Create registry
	ConfigurationRegistry(
		subgroups = subgroups,
		populations = populations,
		soc_config = soc_config,
		pt_config = pt_config,
		report_types = config$report_types %||% list(),
		performance = config$performance %||% list(),
		plots = config$plots %||% list(),
		tables = config$tables %||% list(),
		validation = config$validation %||% list()
	)
}

#' Define or override a subgroup configuration
#'
#' Adds a new subgroup configuration or overrides an existing one.
#' Runtime overrides have higher priority than YAML configurations.
#'
#' @param registry A ConfigurationRegistry object
#' @param name Character string name for the subgroup
#' @param variable Character string variable name in the data
#' @param labels Named list mapping values to display labels
#' @param order Character vector for display order
#' @param filter_values Optional character vector of values to include
#' @param priority Integer priority (default 100, higher than YAML)
#'
#' @return The modified ConfigurationRegistry object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' registry <- define_subgroup_config(
#'   registry,
#'   name = "age_custom",
#'   variable = "AGEGR1",
#'   labels = list(YOUNG = "< 75", OLD = ">= 75")
#' )
#' }
define_subgroup_config <- function(
	registry,
	name,
	variable,
	labels = NULL,
	order = NULL,
	filter_values = NULL,
	priority = 100
) {
	checkmate::assert_class(registry, "ConfigurationRegistry")
	checkmate::assert_string(name)
	checkmate::assert_string(variable)

	new_subgroup <- SubgroupConfig(
		variable = variable,
		labels = labels %||% list(),
		order = order,
		filter_values = filter_values,
		source = "r_api",
		priority = priority
	)

	registry@subgroups[[name]] <- new_subgroup
	registry
}

#' Define or override a population configuration
#'
#' @param registry A ConfigurationRegistry object
#' @param name Character string name for the population
#' @param variable Character string variable name in the data
#' @param label Character string display label
#' @param description Character string description
#' @param flag_value Character string indicating membership
#' @param priority Integer priority
#'
#' @return The modified ConfigurationRegistry object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' registry <- define_population_config(
#'   registry,
#'   name = "CUSTOM_POP",
#'   variable = "POPFL",
#'   label = "Custom Population"
#' )
#' }
define_population_config <- function(
	registry,
	name,
	variable,
	label = NULL,
	description = NULL,
	flag_value = "Y",
	priority = 100
) {
	checkmate::assert_class(registry, "ConfigurationRegistry")
	checkmate::assert_string(name)
	checkmate::assert_string(variable)

	new_population <- PopulationConfig(
		variable = variable,
		label = label %||% name,
		description = description,
		flag_value = flag_value,
		source = "r_api",
		priority = priority
	)

	registry@populations[[name]] <- new_population
	registry
}

#' Get subgroup configuration
#'
#' Retrieves a subgroup configuration by name.
#'
#' @param registry A ConfigurationRegistry object
#' @param name Character string name of the subgroup
#'
#' @return A SubgroupConfig object or NULL if not found
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' config <- get_subgroup_config(registry, "age_groups")
#' }
get_subgroup_config <- function(registry, name) {
	checkmate::assert_class(registry, "ConfigurationRegistry")
	checkmate::assert_string(name)

	registry@subgroups[[name]]
}

#' Get population configuration
#'
#' Retrieves a population configuration by name.
#'
#' @param registry A ConfigurationRegistry object
#' @param name Character string name of the population
#'
#' @return A PopulationConfig object or NULL if not found
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' config <- get_population_config(registry, "SAF")
#' }
get_population_config <- function(registry, name) {
	checkmate::assert_class(registry, "ConfigurationRegistry")
	checkmate::assert_string(name)

	registry@populations[[name]]
}

#' List available subgroups
#'
#' Returns names of all configured subgroups.
#'
#' @param registry A ConfigurationRegistry object
#'
#' @return Character vector of subgroup names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' list_subgroups(registry)
#' }
list_subgroups <- function(registry) {
	checkmate::assert_class(registry, "ConfigurationRegistry")
	names(registry@subgroups)
}

#' List available populations
#'
#' Returns names of all configured populations.
#'
#' @param registry A ConfigurationRegistry object
#'
#' @return Character vector of population names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' list_populations(registry)
#' }
list_populations <- function(registry) {
	checkmate::assert_class(registry, "ConfigurationRegistry")
	names(registry@populations)
}

#' Update SOC configuration
#'
#' Updates the SOC configuration with new settings.
#'
#' @param registry A ConfigurationRegistry object
#' @param variable Character string for SOC variable name
#' @param sort_by Character string for sorting method
#' @param min_subjects Numeric minimum subjects
#' @param top_n Numeric maximum number of SOCs
#'
#' @return The modified ConfigurationRegistry object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' registry <- update_soc_config(
#'   registry,
#'   sort_by = "alphabetical",
#'   top_n = 10
#' )
#' }
update_soc_config <- function(
	registry,
	variable = NULL,
	sort_by = NULL,
	min_subjects = NULL,
	top_n = NULL
) {
	checkmate::assert_class(registry, "ConfigurationRegistry")

	current <- registry@soc_config
	if (is.null(current)) {
		current <- SOCConfig(variable = "AEBODSYS")
	}

	if (!is.null(variable)) {
		current@variable <- variable
	}
	if (!is.null(sort_by)) {
		current@sort_by <- sort_by
	}
	if (!is.null(min_subjects)) {
		current@min_subjects <- min_subjects
	}
	if (!is.null(top_n)) {
		current@top_n <- top_n
	}
	current@source <- "r_api"
	current@priority <- 100

	registry@soc_config <- current
	registry
}

#' Update PT configuration
#'
#' Updates the PT configuration with new settings.
#'
#' @param registry A ConfigurationRegistry object
#' @param variable Character string for PT variable name
#' @param sort_by Character string for sorting method
#' @param min_subjects Numeric minimum subjects
#' @param top_n_per_soc Numeric maximum PTs per SOC
#'
#' @return The modified ConfigurationRegistry object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' registry <- update_pt_config(
#'   registry,
#'   sort_by = "frequency"
#' )
#' }
update_pt_config <- function(
	registry,
	variable = NULL,
	sort_by = NULL,
	min_subjects = NULL,
	top_n_per_soc = NULL
) {
	checkmate::assert_class(registry, "ConfigurationRegistry")

	current <- registry@pt_config
	if (is.null(current)) {
		current <- PTConfig(variable = "AEDECOD")
	}

	if (!is.null(variable)) {
		current@variable <- variable
	}
	if (!is.null(sort_by)) {
		current@sort_by <- sort_by
	}
	if (!is.null(min_subjects)) {
		current@min_subjects <- min_subjects
	}
	if (!is.null(top_n_per_soc)) {
		current@top_n_per_soc <- top_n_per_soc
	}
	current@source <- "r_api"
	current@priority <- 100

	registry@pt_config <- current
	registry
}

#' Get performance setting
#'
#' Retrieves a performance setting value.
#'
#' @param registry A ConfigurationRegistry object
#' @param name Character string name of the setting
#' @param default Default value if setting not found
#'
#' @return The setting value
#'
#' @export
#'
#' @examples
#' \dontrun{
#' registry <- load_config()
#' batch_size <- get_performance_setting(registry, "docx.batch_size", 50)
#' }
get_performance_setting <- function(registry, name, default = NULL) {
	checkmate::assert_class(registry, "ConfigurationRegistry")

	# Navigate nested structure (e.g., "docx.batch_size")
	parts <- strsplit(name, "\\.")[[1]]
	value <- registry@performance

	for (part in parts) {
		if (is.list(value) && part %in% names(value)) {
			value <- value[[part]]
		} else {
			return(default)
		}
	}

	value
}
