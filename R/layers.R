#' Layer System
#'
#' Layer-based approach to clinical table generation.
#'
#' @name layers
NULL

#' AnalysisLayer Base Class
#'
#' Base class for analysis layers.
#'
#' @keywords internal
#' @noRd
AnalysisLayer <- S7::new_class(
	"AnalysisLayer",
	package = "pharmhand",
	abstract = TRUE,
	properties = list(
		target_var = S7::new_property(
			S7::class_character,
			validator = function(value) {
				if (length(value) < 1) "@target_var must have at least one variable"
			}
		),
		by_vars = S7::new_property(S7::class_character, default = character()),
		where = S7::new_property(S7::class_any, default = NULL),
		format_spec = S7::new_property(S7::class_any, default = NULL),
		label = S7::new_property(S7::class_any, default = NULL),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

#' Count Layer Class
#'
#' Layer for categorical variable summaries including counts and percentages.
#' Supports distinct subject counts and nested hierarchies.
#'
#' @export
#'
#' @param target_var Character vector of variable names to summarize
#' @param by_vars Character vector of grouping variables
#' @param where Filter expression (quosure or NULL)
#' @param format_spec Format specification for output
#' @param distinct_by Variable for distinct counting (default: USUBJID)
#' @param include_pct Logical, include percentages
#' @param denoms_by Variables to calculate denominators by
#' @param label Optional label for the layer
#' @param metadata Additional metadata
#'
#' @return A CountLayer object
#'
#' @examples
#'
#' layer <- CountLayer(
#'   target_var = "SEX",
#'   by_vars = "TRT01P",
#'   include_pct = TRUE
#' )
#'
CountLayer <- S7::new_class(
	"CountLayer",
	package = "pharmhand",
	parent = AnalysisLayer,
	properties = list(
		distinct_by = S7::new_property(S7::class_character, default = "USUBJID"),
		include_pct = S7::new_property(S7::class_logical, default = TRUE),
		denoms_by = S7::new_property(S7::class_character, default = character())
	)
)

#' Descriptive Layer Class
#'
#' Layer for continuous variable summaries including mean, SD, median,
#' min, max, and other descriptive statistics.
#'
#' @export
#'
#' @param target_var Character vector of variable names to summarize
#' @param by_vars Character vector of grouping variables
#' @param where Filter expression (quosure or NULL)
#' @param format_spec Format specification for output
#' @param stats Character vector of statistics to compute
#' @param precision Integer or "auto" for decimal precision
#' @param label Optional label for the layer
#' @param metadata Additional metadata
#'
#' @return A DescriptiveLayer object
#'
#' @examples
#' \dontrun{
#' layer <- DescriptiveLayer(
#'   target_var = "AGE",
#'   by_vars = "TRT01P",
#'   stats = c("n", "mean", "sd", "median", "min", "max")
#' )
#' }
DescriptiveLayer <- S7::new_class(
	"DescriptiveLayer",
	package = "pharmhand",
	parent = AnalysisLayer,
	properties = list(
		stats = S7::new_property(
			S7::class_character,
			default = c("n", "mean", "sd", "median", "min", "max")
		),
		precision = S7::new_property(S7::class_any, default = "auto")
	)
)

#' Shift Layer Class
#'
#' Layer for shift table analysis showing transitions from baseline
#' to post-baseline states (e.g., lab grade shifts).
#'
#' @export
#'
#' @param target_var Character vector of exactly 2 variable names:
#'   c(baseline_var, post_var). First element is the baseline variable,
#'   second is the post-baseline variable.
#' @param by_vars Character vector of grouping variables
#' @param where Filter expression (quosure or NULL)
#' @param format_spec Format specification for output
#' @param distinct_by Variable for distinct counting (default: USUBJID)
#' @param include_n Logical, include subject counts
#' @param include_pct Logical, include percentages
#' @param label Optional label for the layer
#' @param metadata Additional metadata
#'
#' @return A ShiftLayer object
#'
#' @examples
#' \dontrun{
#' layer <- ShiftLayer(
#'   target_var = c("BTOXGR", "ATOXGR"),
#'   by_vars = "TRT01P"
#' )
#' }
ShiftLayer <- S7::new_class(
	"ShiftLayer",
	package = "pharmhand",
	parent = AnalysisLayer,
	properties = list(
		distinct_by = S7::new_property(S7::class_character, default = "USUBJID"),
		include_n = S7::new_property(S7::class_logical, default = TRUE),
		include_pct = S7::new_property(S7::class_logical, default = TRUE)
	),
	validator = function(self) {
		if (length(self@target_var) != 2) {
			"@target_var must have exactly 2 variables: c(baseline_var, post_var)"
		}
	}
)

#' Layered Table Class
#'
#' Container for multiple analysis layers that compose into a single table.
#' Manages the stacking and rendering of layers.
#'
#' @export
#'
#' @usage
#' LayeredTable(
#'   data = data.frame(),
#'   trt_var = "TRT01P",
#'   pop_filter = NULL,
#'   layers = list(),
#'   title = NULL,
#'   metadata = list(),
#'   big_n = NULL
#' )
#'
#' @param data Source data frame
#' @param trt_var Treatment variable name
#' @param pop_filter Population filter expression
#' @param layers List of AnalysisLayer objects
#' @param title Table title
#' @param metadata Additional metadata
#' @param big_n Pre-computed treatment group counts (optional)
#'
#' @return A LayeredTable object
#'
#' @examples
#' \dontrun{
#' tbl <- LayeredTable(
#'   data = adsl,
#'   trt_var = "TRT01P",
#'   layers = list(
#'     CountLayer(target_var = "SEX"),
#'     DescriptiveLayer(target_var = "AGE")
#'   ),
#'   title = "Demographics"
#' )
#' }
LayeredTable <- S7::new_class(
	"LayeredTable",
	package = "pharmhand",
	properties = list(
		data = S7::new_property(S7::class_data.frame),
		trt_var = S7::new_property(S7::class_character, default = "TRT01P"),
		pop_filter = S7::new_property(S7::class_any, default = NULL),
		layers = S7::new_property(
			S7::class_list,
			default = list(),
			validator = function(value) {
				if (length(value) > 0) {
					all_layers <- vapply(
						value,
						S7::S7_inherits,
						logical(1),
						AnalysisLayer
					)
					if (!all(all_layers)) {
						return("All elements in @layers must be AnalysisLayer objects")
					}
				}
				NULL
			}
		),
		title = S7::new_property(S7::class_any, default = NULL),
		metadata = S7::new_property(S7::class_list, default = list()),
		# Computed
		n_layers = S7::new_property(
			class = S7::class_integer,
			getter = function(self) length(self@layers)
		),
		big_n = S7::new_property(S7::class_any, default = NULL)
	)
)

#' Add a layer to a LayeredTable
#'
#' @param table A LayeredTable object
#' @param layer An AnalysisLayer object
#'
#' @return Updated LayeredTable
#' @export
add_layer <- function(table, layer) {
	if (!S7::S7_inherits(table, LayeredTable)) {
		stop("'table' must be a LayeredTable object", call. = FALSE)
	}
	if (!S7::S7_inherits(layer, AnalysisLayer)) {
		stop("'layer' must be an AnalysisLayer object", call. = FALSE)
	}
	table@layers <- c(table@layers, list(layer))
	table
}

#' Build a LayeredTable
#'
#' Execute all layers and combine results into a single data frame.
#'
#' @param table A LayeredTable object
#' @param ... Additional arguments
#'
#' @return A data frame with combined layer results
#' @export
build_table <- function(table, ...) {
	if (!S7::S7_inherits(table, LayeredTable)) {
		stop("'table' must be a LayeredTable object", call. = FALSE)
	}

	data <- table@data
	trt_var <- table@trt_var

	# Apply population filter if specified
	if (!is.null(table@pop_filter)) {
		data <- dplyr::filter(data, !!table@pop_filter)
	}

	# Pre-calculate denominators (Big N) if not already present
	big_n <- table@big_n
	if (is.null(big_n)) {
		if (!"USUBJID" %in% names(data)) {
			stop(
				"Column 'USUBJID' not found in data. Required for subject counting.",
				call. = FALSE
			)
		}
		big_n <- data |>
			dplyr::group_by(!!rlang::sym(trt_var)) |>
			dplyr::summarise(
				N_tot = dplyr::n_distinct(.data$USUBJID),
				.groups = "drop"
			)
	}

	# Process each layer
	results <- lapply(table@layers, function(layer) {
		build_layer(layer, data, trt_var, big_n = big_n)
	})

	# Stack results
	dplyr::bind_rows(results)
}

#' Build a single layer
#'
#' @param layer An AnalysisLayer object
#' @param ... Additional arguments passed to layer-specific methods
#'   (typically includes `data`, `trt_var`, and `big_n`)
#'
#' @return A data frame with layer results
#' @export
build_layer <- S7::new_generic("build_layer", "layer")

#' @export
S7::method(build_layer, CountLayer) <- function(
	layer,
	data,
	trt_var,
	big_n = NULL
) {
	build_count_layer(layer, data, trt_var, big_n = big_n)
}

#' @export
S7::method(build_layer, DescriptiveLayer) <- function(
	layer,
	data,
	trt_var,
	big_n = NULL
) {
	build_descriptive_layer(layer, data, trt_var)
}

#' @export
S7::method(build_layer, ShiftLayer) <- function(
	layer,
	data,
	trt_var,
	big_n = NULL
) {
	build_shift_layer(layer, data, trt_var)
}

#' @keywords internal
#' @noRd
build_count_layer <- function(layer, data, trt_var, big_n = NULL) {
	target <- layer@target_var[1]
	by_vars <- c(trt_var, layer@by_vars)
	distinct_by <- layer@distinct_by

	# Apply filter if specified
	if (!is.null(layer@where)) {
		data <- dplyr::filter(data, !!layer@where)
	}

	# Calculate counts
	counts <- data |>
		dplyr::group_by(dplyr::across(dplyr::all_of(c(by_vars, target)))) |>
		dplyr::summarise(
			n = dplyr::n_distinct(.data[[distinct_by]]),
			.groups = "drop"
		)

	# Calculate denominators and percentages if requested
	if (layer@include_pct) {
		denom_vars <- if (length(layer@denoms_by) > 0) layer@denoms_by else trt_var

		# Use pre-calculated big_n if it matches trt_var and no other denoms_by
		if (
			!is.null(big_n) &&
				identical(denom_vars, trt_var) &&
				trt_var %in% names(big_n)
		) {
			denoms <- big_n
		} else {
			denoms <- data |>
				dplyr::group_by(dplyr::across(dplyr::all_of(denom_vars))) |>
				dplyr::summarise(
					N_tot = dplyr::n_distinct(.data[[distinct_by]]),
					.groups = "drop"
				)
		}

		counts <- counts |>
			dplyr::left_join(denoms, by = denom_vars) |>
			dplyr::mutate(pct = round(.data$n / .data$N_tot * 100, 1))
	}

	counts$variable <- target
	counts$layer_type <- "count"
	counts
}

#' @keywords internal
#' @noRd
build_descriptive_layer <- function(layer, data, trt_var) {
	target <- layer@target_var[1]
	by_vars <- c(trt_var, layer@by_vars)
	stats <- layer@stats

	# Apply filter if specified
	if (!is.null(layer@where)) {
		data <- dplyr::filter(data, !!layer@where)
	}

	# Only compute requested statistics for efficiency
	stat_exprs <- list(
		n = rlang::expr(dplyr::n()),
		mean = rlang::expr(mean(.data[[!!target]], na.rm = TRUE)),
		sd = rlang::expr(stats::sd(.data[[!!target]], na.rm = TRUE)),
		median = rlang::expr(stats::median(.data[[!!target]], na.rm = TRUE)),
		min = rlang::expr(min(.data[[!!target]], na.rm = TRUE)),
		max = rlang::expr(max(.data[[!!target]], na.rm = TRUE)),
		q1 = rlang::expr(stats::quantile(.data[[!!target]], 0.25, na.rm = TRUE)),
		q3 = rlang::expr(stats::quantile(.data[[!!target]], 0.75, na.rm = TRUE))
	)

	# Build only the requested statistics
	stat_calls <- stat_exprs[intersect(stats, names(stat_exprs))]

	result <- data |>
		dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) |>
		dplyr::summarise(!!!stat_calls, .groups = "drop")

	result$variable <- target
	result$layer_type <- "descriptive"
	result
}

#' @keywords internal
#' @noRd
build_shift_layer <- function(layer, data, trt_var) {
	by_vars <- c(trt_var, layer@by_vars)
	baseline <- layer@target_var[1]
	post <- layer@target_var[2]
	distinct_by <- layer@distinct_by

	# Apply filter if specified
	if (!is.null(layer@where)) {
		data <- dplyr::filter(data, !!layer@where)
	}

	# Calculate shift counts (distinct subjects, not rows)
	result <- data |>
		dplyr::filter(!is.na(.data[[baseline]]) & !is.na(.data[[post]])) |>
		dplyr::group_by(
			dplyr::across(dplyr::all_of(by_vars)),
			baseline = .data[[baseline]],
			post = .data[[post]]
		) |>
		dplyr::summarise(
			n = dplyr::n_distinct(.data[[distinct_by]]),
			.groups = "drop"
		)

	if (layer@include_pct) {
		totals <- result |>
			dplyr::group_by(dplyr::across(dplyr::all_of(c(by_vars, "baseline")))) |>
			dplyr::summarise(N = sum(.data$n), .groups = "drop")

		result <- result |>
			dplyr::left_join(totals, by = c(by_vars, "baseline")) |>
			dplyr::mutate(pct = round(.data$n / .data$N * 100, 1))
	}

	result$variable <- paste(baseline, "->", post)
	result$layer_type <- "shift"
	result
}
