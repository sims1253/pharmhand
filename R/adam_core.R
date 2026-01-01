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
		dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
		dplyr::summarise(
			N = dplyr::n_distinct(.data[[subject_var]]),
			.groups = "drop"
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
	# Apply population filter using admiral utilities if applicable
	df <- x@data

	# Ensure population filter is valid
	if (x@population != "ALL") {
		pop_fl <- paste0(x@population, "FL")
		if (pop_fl %in% names(df)) {
			# Use standard filter for population flags
			df <- df |> dplyr::filter(!!dplyr::sym(pop_fl) == "Y")
		} else {
			ph_warn(
				paste0(
					"Population flag '",
					pop_fl,
					"' not found in data. No filtering applied."
				)
			)
		}
	}

	# Basic summary stats (Baseline)
	stats <- df |>
		dplyr::group_by(!!dplyr::sym(x@trt_var)) |>
		dplyr::summarise(
			N = dplyr::n_distinct(!!dplyr::sym(x@subject_var)),
			.groups = "drop"
		)

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
	df <- data@data
	trt_var <- data@trt_var

	# Apply population filter
	if (
		data@population != "ALL" && paste0(data@population, "FL") %in% names(df)
	) {
		df <- df |>
			dplyr::filter(!!dplyr::sym(paste0(data@population, "FL")) == "Y")
	}

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
			dplyr::group_by(.data$variable, !!dplyr::sym(trt_var)) |>
			dplyr::summarise(
				n = sum(!is.na(.data$value)),
				mean = mean(.data$value, na.rm = TRUE),
				sd = sd(.data$value, na.rm = TRUE),
				median = median(.data$value, na.rm = TRUE),
				min = min(.data$value, na.rm = TRUE),
				max = max(.data$value, na.rm = TRUE),
				.groups = "drop"
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
	is_adam <- S7::S7_inherits(data, ADaMData)

	if (is_adam) {
		df <- data@data
		trt_var <- data@trt_var
		sub_var <- data@subject_var
	} else {
		df <- data
		trt_var <- "TRT01P"
		sub_var <- "USUBJID"
	}

	# Total subjects per treatment arm for percentages
	big_n <- df |>
		dplyr::group_by(!!dplyr::sym(trt_var)) |>
		dplyr::summarise(
			N_tot = dplyr::n_distinct(!!dplyr::sym(sub_var)),
			.groups = "drop"
		)

	# SOC Level Stats
	soc_stats <- df |>
		dplyr::group_by(!!dplyr::sym(soc_var), !!dplyr::sym(trt_var)) |>
		dplyr::summarise(
			n = dplyr::n_distinct(!!dplyr::sym(sub_var)),
			.groups = "drop"
		) |>
		dplyr::left_join(big_n, by = trt_var) |>
		dplyr::mutate(
			pct = (n / N_tot) * 100,
			level = "SOC",
			label = !!dplyr::sym(soc_var)
		)

	# PT Level Stats
	pt_stats <- df |>
		dplyr::group_by(
			!!dplyr::sym(soc_var),
			!!dplyr::sym(pt_var),
			!!dplyr::sym(trt_var)
		) |>
		dplyr::summarise(
			n = dplyr::n_distinct(!!dplyr::sym(sub_var)),
			.groups = "drop"
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
	df <- if (is_adam) data@data else data

	subgroups <- unique(df[[subgroup_var]])
	subgroups <- subgroups[!is.na(subgroups)]

	lapply(subgroups, function(sg) {
		sg_data <- df[df[[subgroup_var]] == sg, ]
		if (is_adam) {
			# Create a new ADaMData object to avoid mutating the original
			data_sg <- ADaMData(
				data = sg_data,
				domain = data@domain,
				population = data@population,
				subject_var = data@subject_var,
				trt_var = data@trt_var,
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
