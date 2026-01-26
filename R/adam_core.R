#' ADaM Analysis Core
#'
#' Vectorized functions for clinical data analysis.
#'
#' @name adam_core
NULL

# Helper Functions for Data Access ----

#' Get Treatment Group Counts
#'
#' Extract or compute treatment group counts from ADaMData or data frame.
#'
#' @param data ADaMData object or data frame
#' @param trt_var Treatment variable name (used only for data frames,
#'   ignored for ADaMData which uses its own trt_var)
#' @param population Population to filter by (used only for data frames,
#'   filters by `{population}FL == "Y"`)
#' @param subject_var Subject ID variable (used only for data frames)
#'
#' @return Data frame with treatment variable and N column
#' @export
#'
#' @examples
#' \dontrun{
#' # From ADaMData - uses stored trt_var and population
#' adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
#' trt_n <- get_trt_n(adam)
#'
#' # From data frame - must specify parameters
#' trt_n <- get_trt_n(adsl, trt_var = "TRT01P", population = "SAF")
#' }
get_trt_n <- function(
	data,
	trt_var = "TRT01P",
	population = NULL,
	subject_var = "USUBJID"
) {
	# If ADaMData, use computed property
	if (S7::S7_inherits(data, ADaMData)) {
		# Warn if user passed arguments that are ignored for ADaMData
		# Check if trt_var was explicitly passed (not default)
		calling_env <- parent.frame()
		trt_var_explicit <- !missing(trt_var)
		population_explicit <- !missing(population)
		subject_var_explicit <- !missing(subject_var)

		if (trt_var_explicit) {
			ph_warn(
				"'trt_var' argument is ignored for ADaMData objects. ",
				"Using stored 'trt_var' property."
			)
		}
		if (population_explicit) {
			ph_warn(
				"'population' argument is ignored for ADaMData objects. ",
				"Using stored 'population' property."
			)
		}
		if (subject_var_explicit) {
			ph_warn(
				"'subject_var' argument is ignored for ADaMData objects. ",
				"Using stored 'subject_var' property."
			)
		}

		return(data@trt_n)
	}

	# For data frames, compute directly
	df <- data

	# Apply population filter if specified
	if (!is.null(population) && population != "ALL") {
		pop_fl <- paste0(population, "FL")
		if (pop_fl %in% names(df)) {
			df <- df[df[[pop_fl]] == "Y", , drop = FALSE]
		}
	}

	if (nrow(df) == 0) {
		return(data.frame())
	}

	df |>
		dplyr::summarise(
			N = dplyr::n_distinct(.data[[subject_var]]),
			.by = dplyr::all_of(trt_var)
		)
}

#' Get Filtered Data
#'
#' Extract filtered data from ADaMData or apply population filter to a
#' data frame. For ADaMData objects, returns the `filtered_data` property.
#' For data frames, applies the specified population filter.
#'
#' @param data ADaMData object or data frame
#' @param population Population to filter by (used only for data frames,
#'   filters by `{population}FL == "Y"`)
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' \dontrun{
#' # From ADaMData - uses stored population
#' adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
#' df <- get_filtered_data(adam)
#'
#' # From data frame - must specify population
#' df <- get_filtered_data(adsl, population = "SAF")
#' }
get_filtered_data <- function(data, population = NULL) {
	# If ADaMData, use computed property
	if (S7::S7_inherits(data, ADaMData)) {
		return(data@filtered_data)
	}

	# For data frames, apply filter if specified
	df <- data

	if (!is.null(population) && population != "ALL") {
		pop_fl <- paste0(population, "FL")
		if (pop_fl %in% names(df)) {
			df <- df[df[[pop_fl]] == "Y", , drop = FALSE]
		}
	}

	df
}

#' Get Treatment Variable Name
#'
#' Extract treatment variable name from ADaMData or return default.
#'
#' @param data ADaMData object or data frame
#' @param default Default treatment variable name for data frames
#'
#' @return Character string with treatment variable name
#' @keywords internal
get_trt_var <- function(data, default = "TRT01P") {
	if (S7::S7_inherits(data, ADaMData)) {
		return(data@trt_var)
	}
	default
}

#' Get Subject Variable Name
#'
#' Extract subject ID variable name from ADaMData or return default.
#'
#' @param data ADaMData object or data frame
#' @param default Default subject variable name for data frames
#'
#' @return Character string with subject variable name
#' @keywords internal
get_subject_var <- function(data, default = "USUBJID") {
	if (S7::S7_inherits(data, ADaMData)) {
		return(data@subject_var)
	}
	default
}

#' Get Total Subject Count
#'
#' Extract total subject count from ADaMData or calculate from data frame.
#' For ADaMData, uses the computed `subject_n` property which respects
#' population filters.
#'
#' @param data ADaMData object or data frame
#' @param population Population filter for data frames (optional)
#' @param subject_var Subject variable name for data frames (optional)
#'
#' @return Integer count of distinct subjects
#' @export
#'
#' @examples
#' \dontrun{
#' # From ADaMData - uses computed property
#' adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
#' n_subj <- get_subject_n(adam)
#'
#' # From data frame
#' n_subj <- get_subject_n(adsl, population = "FAS")
#' }
get_subject_n <- function(data, population = NULL, subject_var = "USUBJID") {
	# If ADaMData, use computed property
	if (S7::S7_inherits(data, ADaMData)) {
		return(data@subject_n)
	}

	# For data frames, compute directly
	df <- data

	# Apply population filter if specified
	if (!is.null(population) && population != "ALL") {
		pop_fl <- paste0(population, "FL")
		if (pop_fl %in% names(df)) {
			df <- df[df[[pop_fl]] == "Y", , drop = FALSE]
		}
	}

	if (nrow(df) == 0) {
		return(0L)
	}

	dplyr::n_distinct(df[[subject_var]])
}

#' Get Summary Label
#'
#' Get a summary label for ADaMData or AnalysisResults.
#' For ADaMData, returns "Population (N=X)".
#' For AnalysisResults, returns "type (n=Y)" or "type (empty)".
#'
#' @param object ADaMData or AnalysisResults object
#'
#' @return Character string summary label
#' @export
#'
#' @examples
#' \dontrun{
#' adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
#' get_summary_label(adam)  # "FAS (N=150)"
#'
#' results <- calculate_baseline(adam, vars = c("AGE"))
#' get_summary_label(results)  # "baseline (n=2)"
#' }
get_summary_label <- function(object) {
	if (S7::S7_inherits(object, ADaMData)) {
		return(object@summary_label)
	}

	if (S7::S7_inherits(object, AnalysisResults)) {
		return(object@summary_label)
	}

	# Fallback for other objects
	stop("'object' must be an ADaMData or AnalysisResults object")
}

# Analysis Functions ----

#' Analyze ADaMData
#'
#' S7 method for analyzing ADaMData objects.
#'
#' @param x An ADaMData object
#' @param ... Additional arguments
#'
#' @return An AnalysisResults object
#' @export
#' @name analyze_ADaMData
analyze_ADaMData <- S7::method(analyze, ADaMData) <- function(x, ...) {
	# Use computed properties for filtered data and treatment counts
	df <- x@filtered_data

	# Check if population filter was applied successfully
	if (x@population != "ALL") {
		pop_fl <- paste0(x@population, "FL")
		if (!pop_fl %in% names(x@data)) {
			ph_warn(
				paste0(
					"Population flag '",
					pop_fl,
					"' not found in data. No filtering applied."
				)
			)
		}
	}

	# Use computed property for treatment counts
	stats <- x@trt_n

	AnalysisResults(stats = stats, type = "baseline", metadata = x@metadata)
}

#' Calculate Baseline Characteristics
#'
#' @param data ADaMData object
#' @param vars Character vector of variables to analyze
#'
#' @return AnalysisResults object
#' @export
calculate_baseline <- function(data, vars) {
	# Use computed property for filtered data (respects population filter)
	df <- data@filtered_data
	trt_var <- data@trt_var

	# Separate numeric and categorical analysis for robustness
	num_vars <- df |>
		dplyr::select(dplyr::all_of(vars)) |>
		dplyr::select(dplyr::where(is.numeric)) |>
		names()
	cat_vars <- setdiff(vars, num_vars)

	res_num <- NULL
	if (length(num_vars) > 0) {
		res_num <- df |>
			dplyr::select(!!dplyr::sym(trt_var), dplyr::all_of(num_vars)) |>
			tidyr::pivot_longer(
				cols = dplyr::all_of(num_vars),
				names_to = "variable",
				values_to = "value"
			) |>
			dplyr::summarise(
				n = sum(!is.na(.data$value)),
				mean = mean(.data$value, na.rm = TRUE),
				sd = sd(.data$value, na.rm = TRUE),
				median = median(.data$value, na.rm = TRUE),
				min = min(.data$value, na.rm = TRUE),
				max = max(.data$value, na.rm = TRUE),
				.by = c("variable", !!dplyr::sym(trt_var))
			)
	}

	res_cat <- NULL
	if (length(cat_vars) > 0) {
		res_cat <- df |>
			dplyr::select(!!dplyr::sym(trt_var), dplyr::all_of(cat_vars)) |>
			tidyr::pivot_longer(
				cols = dplyr::all_of(cat_vars),
				names_to = "variable",
				values_to = "value"
			) |>
			dplyr::group_by(
				.data$variable,
				!!dplyr::sym(trt_var),
				.data$value
			) |>
			dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
			dplyr::mutate(pct = (.data$n / sum(.data$n)) * 100) |>
			dplyr::ungroup() |>
			dplyr::mutate(
				label = paste0(
					.data$value,
					" (n=",
					.data$n,
					", ",
					round(.data$pct, 1),
					"%)"
				)
			) |>
			dplyr::select(
				"variable",
				!!dplyr::sym(trt_var),
				"n",
				"label"
			)
	}

	# Ensure at least one result exists
	final_stats <- res_num %||% res_cat
	if (is.null(final_stats)) {
		ph_warn("No valid variables found for baseline analysis")
		final_stats <- data.frame()
	}

	# Return combined results (preferring numeric summary for the main stats slot)
	AnalysisResults(
		stats = final_stats,
		type = "baseline",
		metadata = list(categorical = res_cat)
	)
}

#' Analyze Adverse Events by SOC and PT
#'
#' @param data ADAE dataset (or ADaMData containing it)
#' @param soc_var Character string for SOC variable. Default "AEBODSYS"
#' @param pt_var Character string for PT variable. Default "AEDECOD"
#'
#' @return AnalysisResults object
#' @export
analyze_soc_pt <- function(data, soc_var = "AEBODSYS", pt_var = "AEDECOD") {
	# Use helper functions for consistent variable access
	trt_var <- get_trt_var(data)
	sub_var <- get_subject_var(data)

	# Use filtered_data for ADaMData (respects population filter)
	df <- if (S7::S7_inherits(data, ADaMData)) data@filtered_data else data

	# Total subjects per treatment arm for percentages
	# For ADaMData, get_trt_n already computes this efficiently
	if (S7::S7_inherits(data, ADaMData)) {
		big_n <- data@trt_n
		names(big_n)[names(big_n) == "N"] <- "N_tot"
	} else {
		big_n <- df |>
			dplyr::summarise(
				N_tot = dplyr::n_distinct(!!dplyr::sym(sub_var)),
				.by = !!dplyr::sym(trt_var)
			)
	}

	# SOC Level Stats
	soc_stats <- df |>
		dplyr::summarise(
			n = dplyr::n_distinct(!!dplyr::sym(sub_var)),
			.by = c(!!dplyr::sym(soc_var), !!dplyr::sym(trt_var))
		) |>
		dplyr::left_join(big_n, by = trt_var) |>
		dplyr::mutate(
			pct = (n / N_tot) * 100,
			level = "SOC",
			label = !!dplyr::sym(soc_var)
		)

	# PT Level Stats
	pt_stats <- df |>
		dplyr::summarise(
			n = dplyr::n_distinct(!!dplyr::sym(sub_var)),
			.by = c(
				!!dplyr::sym(soc_var),
				!!dplyr::sym(pt_var),
				!!dplyr::sym(trt_var)
			)
		) |>
		dplyr::left_join(big_n, by = trt_var) |>
		dplyr::mutate(
			pct = (n / N_tot) * 100,
			level = "PT",
			label = !!dplyr::sym(pt_var)
		)

	combined <- dplyr::bind_rows(soc_stats, pt_stats) |>
		dplyr::arrange(!!dplyr::sym(soc_var), .data$level == "PT", .data$label)

	AnalysisResults(stats = combined, type = "safety_ae")
}

#' Apply Subgroup Analysis
#'
#' @param data ADaMData object or data frame
#' @param subgroup_var Character string specifying the subgroup variable
#' @param analysis_fn Function to perform analysis
#' @param ... Additional arguments
#'
#' @return A list of AnalysisResults per subgroup
#' @export
apply_subgroups <- function(data, subgroup_var, analysis_fn, ...) {
	is_adam <- S7::S7_inherits(data, ADaMData)

	# Use filtered_data for ADaMData to respect population filter
	df <- if (is_adam) data@filtered_data else data

	subgroups <- unique(df[[subgroup_var]])
	subgroups <- subgroups[!is.na(subgroups)]

	lapply(subgroups, function(sg) {
		sg_data <- df[df[[subgroup_var]] == sg, ]
		if (is_adam) {
			# Create a new ADaMData object to avoid mutating the original
			# Use helper functions for consistency
			data_sg <- ADaMData(
				data = sg_data,
				domain = data@domain,
				population = data@population,
				subject_var = get_subject_var(data),
				trt_var = get_trt_var(data),
				metadata = data@metadata
			)
			res <- analysis_fn(data_sg, ...)
		} else {
			res <- analysis_fn(sg_data, ...)
		}
		res@metadata$subgroup <- sg
		res
	})
}
