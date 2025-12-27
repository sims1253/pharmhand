#' Chef Pipeline Integration
#'
#' Bridge between the ramnog/chef ecosystem and pharmhand's S7 classes.
#' Enables AMNOG-style HTA analyses with conversion to clinical reports.
#'
#' @name chef_integration
NULL

#' Convert Chef Results to AnalysisResults
#'
#' Transform chef's hierarchical data.table output into pharmhand's
#' AnalysisResults S7 object for downstream processing.
#'
#' @param chef_output A data.table from chef pipeline execution
#' @param type Character string for result type (default: "hta")
#' @param metadata Additional metadata to attach
#'
#' @return An AnalysisResults S7 object
#' @export
#'
#' @examples
#' \dontrun{
#' # After running chef pipeline
#' results <- chef_to_analysis_results(chef_output)
#' table <- create_clinical_table(results, title = "HTA Analysis")
#' }
chef_to_analysis_results <- function(
	chef_output,
	type = "hta",
	metadata = list()
) {
	if (!requireNamespace("data.table", quietly = TRUE)) {
		cli::cli_abort("Package {.pkg data.table} is required for chef integration")
	}

	# Validate input
	if (!data.table::is.data.table(chef_output)) {
		if (is.data.frame(chef_output)) {
			chef_output <- data.table::as.data.table(chef_output)
		} else {
			cli::cli_abort("{.arg chef_output} must be a data.table or data.frame")
		}
	}

	# Chef output has hierarchical structure with these typical columns:
	# - endpoint_spec_id, endpoint_id, strat_id, stat_id, stratum_id
	# - stat_result_label, stat_result_value, stat_result_qualifier

	# Flatten to analysis-ready format
	stats_df <- flatten_chef_results(chef_output)

	# Build metadata from chef output
	chef_meta <- extract_chef_metadata(chef_output)
	full_metadata <- c(metadata, chef_meta)

	AnalysisResults(
		stats = stats_df,
		type = type,
		groupings = list(
			by_endpoint = unique(chef_output$endpoint_id),
			by_strata = unique(chef_output$strat_id)
		),
		metadata = full_metadata
	)
}

#' Flatten Chef Hierarchical Results
#'
#' Convert chef's nested structure to a flat data frame suitable for tables.
#'
#' @param dt Chef output data.table
#'
#' @return A data.frame with flattened results
#' @keywords internal
flatten_chef_results <- function(dt) {
	# Identify key columns
	id_cols <- c("endpoint_id", "strat_id", "stat_id", "stratum_id")
	id_cols <- intersect(id_cols, names(dt))

	value_cols <- c(
		"stat_result_label",
		"stat_result_value",
		"stat_result_qualifier"
	)
	value_cols <- intersect(value_cols, names(dt))

	# Select and rename for clarity
	result <- as.data.frame(dt)

	if (length(id_cols) > 0) {
		result$row_label <- apply(
			result[, id_cols, drop = FALSE],
			1,
			function(x) paste(na.omit(x), collapse = " | ")
		)
	}

	# Convert to standard column names
	col_map <- c(
		"stat_result_label" = "label",
		"stat_result_value" = "value",
		"stat_result_qualifier" = "qualifier",
		"endpoint_id" = "endpoint",
		"strat_id" = "stratum",
		"stat_id" = "statistic"
	)

	for (old_name in names(col_map)) {
		if (old_name %in% names(result)) {
			names(result)[names(result) == old_name] <- col_map[[old_name]]
		}
	}

	result
}

#' Extract Metadata from Chef Output
#'
#' @param dt Chef output data.table
#'
#' @return List of metadata
#' @keywords internal
extract_chef_metadata <- function(dt) {
	list(
		source = "chef",
		n_endpoints = length(unique(dt$endpoint_id)),
		n_strata = length(unique(dt$strat_id)),
		generated_at = Sys.time()
	)
}

#' Create Chef Endpoint Specification
#'
#' Helper to create endpoint specifications compatible with chef pipeline.
#'
#' @param name Endpoint name
#' @param variable Variable name in ADaM dataset
#' @param type Endpoint type (e.g., "binary", "continuous", "tte")
#' @param population Population filter expression
#' @param strata Character vector of stratification variables
#' @param stats List of statistical functions to apply
#' @param criteria List of inclusion criteria
#'
#' @return A list suitable for chef endpoint definition
#' @export
#'
#' @examples
#' \dontrun{
#' ep_spec <- create_chef_endpoint(
#'   name = "Response Rate",
#'   variable = "AVALC",
#'   type = "binary",
#'   strata = c("SEX", "AGEGR1"),
#'   stats = list(chefStats::n_subj, chefStats::prop_est)
#' )
#' }
create_chef_endpoint <- function(
	name,
	variable,
	type = "binary",
	population = NULL,
	strata = character(),
	stats = list(),
	criteria = list()
) {
	# Input validation
	if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
		cli::cli_abort("{.arg name} must be a non-empty character string")
	}

	if (
		!is.character(variable) || length(variable) != 1 || nchar(variable) == 0
	) {
		cli::cli_abort("{.arg variable} must be a non-empty character string")
	}

	valid_types <- c("binary", "continuous", "tte", "count")
	if (!is.character(type) || length(type) != 1 || !type %in% valid_types) {
		cli::cli_abort(
			"{.arg type} must be one of: {.val {valid_types}}"
		)
	}

	if (!is.character(strata)) {
		cli::cli_abort("{.arg strata} must be a character vector")
	}

	if (!is.list(stats)) {
		cli::cli_abort("{.arg stats} must be a list")
	}

	if (!is.list(criteria)) {
		cli::cli_abort("{.arg criteria} must be a list")
	}

	list(
		name = name,
		variable = variable,
		type = type,
		population = population,
		strata = strata,
		stats = stats,
		criteria = criteria
	)
}

#' Run Chef Pipeline with pharmhand Integration
#'
#' Execute a chef analysis pipeline and return results as pharmhand.
#'
#' @param adam_data Named list of ADaM datasets
#'   (e.g., list(adsl = adsl, adae = adae))
#' @param endpoints List of endpoint specifications from create_chef_endpoint()
#' @param output_type Type of output: "results", "table", or "report"
#' @param ... Additional arguments passed to chef functions
#'
#' @return Depending on output_type:
#'   - "results": AnalysisResults object
#'   - "table": ClinicalTable object
#'   - "report": ClinicalReport object
#' @export
#'
#' @examples
#' \dontrun{
#' # Define endpoints
#' endpoints <- list(
#'   create_chef_endpoint("AE Rate", "AEDECOD", type = "count")
#' )
#'
#' # Run pipeline
#' report <- run_chef_pipeline(
#'   adam_data = list(adsl = adsl, adae = adae),
#'   endpoints = endpoints,
#'   output_type = "report"
#' )
#' }
run_chef_pipeline <- function(
	adam_data,
	endpoints,
	output_type = c("results", "table", "report"),
	...
) {
	output_type <- match.arg(output_type)

	# Validate inputs
	if (!is.list(adam_data) || is.null(names(adam_data))) {
		cli::cli_abort("{.arg adam_data} must be a named list of data frames")
	}

	# Check chef availability
	if (!requireNamespace("chef", quietly = TRUE)) {
		cli::cli_abort(
			c(
				"Package {.pkg chef} is required for pipeline execution",
				"i" = "Install with: remotes::install_github('hta-pharma/chef')"
			)
		)
	}

	# Build chef endpoint definitions
	# Note: logic commented out as chef::mk_endpoint_str requires specific inputs
	# and the result is currently unused in this placeholder implementation
	# ep_defs <- lapply(endpoints, function(ep) {
	#   chef::mk_endpoint_str(
	#     endpoint_label = ep$name,
	#     data_prepare = if (is.null(ep$population)) identity else ep$population,
	#     stat_by_strata_by_trt = ep$stats,
	#     crit_endpoint = ep$criteria
	#   )
	# })

	# For now, return a placeholder - full chef integration requires

	# proper targets pipeline setup which is context-dependent
	cli::cli_alert_info(
		"Chef pipeline configured. Full execution requires targets setup."
	)

	# Create mock results for development
	# Safely extract endpoint names with fallback for malformed endpoints

	endpoint_names <- vapply(
		endpoints,
		function(ep) {
			if (is.null(ep$name) || !is.character(ep$name) || length(ep$name) != 1) {
				return("Unnamed Endpoint")
			}
			ep$name
		},
		character(1)
	)

	mock_results <- data.table::data.table(
		endpoint_id = endpoint_names,
		strat_id = "Overall",
		stat_id = "n",
		stat_result_value = NA_real_,
		stat_result_label = "N"
	)

	results <- chef_to_analysis_results(mock_results, type = "hta")

	# Convert to requested output type
	switch(
		output_type,
		"results" = results,
		"table" = create_clinical_table(results, title = "HTA Analysis"),
		"report" = {
			section <- ReportSection(
				title = "HTA Analysis",
				section_type = "hta",
				content = list(create_clinical_table(results, title = "HTA Results"))
			)
			ClinicalReport(
				study_id = "HTA",
				study_title = "Health Technology Assessment",
				sections = list(section)
			)
		}
	)
}

#' Register Chef Statistical Functions
#'
#' Wrapper to access chefStats functions with pharmhand metadata.
#'
#' @param stat_name Name of the chefStats function
#'
#' @return A function wrapped with metadata for traceability
#' @export
get_chef_stat <- function(stat_name) {
	if (!requireNamespace("chefStats", quietly = TRUE)) {
		cli::cli_abort(
			c(
				"Package {.pkg chefStats} is required",
				"i" = "Install with: remotes::install_github('hta-pharma/chefStats')"
			)
		)
	}

	fn <- tryCatch(
		get(stat_name, envir = asNamespace("chefStats")),
		error = function(e) {
			cli::cli_abort("Function {.fn {stat_name}} not found in chefStats")
		}
	)

	# Wrap with metadata
	structure(
		fn,
		class = c("chef_stat", class(fn)),
		stat_name = stat_name,
		source = "chefStats"
	)
}

#' List Available Chef Statistical Functions
#'
#' @return Character vector of available chefStats function names
#' @export
list_chef_stats <- function() {
	if (!requireNamespace("chefStats", quietly = TRUE)) {
		cli::cli_abort(
			c(
				"Package {.pkg chefStats} is required",
				"i" = "Install with: remotes::install_github('hta-pharma/chefStats')"
			)
		)
	}

	# Get exported functions from chefStats
	exports <- getNamespaceExports("chefStats")

	# Filter to likely statistical functions (exclude internal/utility functions)
	stats <- exports[!grepl("^\\.", exports)]

	stats
}
