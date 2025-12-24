#' High-Performance ADaM Analysis Core
#'
#' This module provides vectorized functions for clinical data analysis
#' using the dplyr and tidyr ecosystems. It replaces legacy row-wise loops
#' with efficient group-by operations.
#'
#' @name adam_core
NULL

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
      # admiral::assert_filter_cond is great for validating condition strings,
      # but here we use standard filter for population flags.
      df <- df |> dplyr::filter(!!dplyr::sym(pop_fl) == "Y")
    } else {
      cli::cli_warn(
        "Population flag {pop_fl} not found in data. No filtering applied."
      )
    }
  }

  # Example of using admiral for data derivation if needed
  # df <- df |> admiral::derive_vars_dtm(...)

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
    dplyr::select(where(is.numeric)) |>
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

  # Return combined results (preferring numeric summary for the main stats slot)
  AnalysisResults(
    stats = res_num %||% res_cat,
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
  # Robust check for S7 ADaMData
  is_adam <- inherits(data, "FunctionReport::ADaMData") ||
    inherits(data, "ADaMData")

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
  is_adam <- inherits(data, "FunctionReport::ADaMData") ||
    inherits(data, "ADaMData")
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
