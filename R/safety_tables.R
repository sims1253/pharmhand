#' Safety Analysis Tables
#'
#' Reusable functions for creating standard safety analysis tables
#' including AE overviews, SOC/PT summaries, severity, relationship,
#' SAEs, and deaths.
#'
#' @name safety_tables
#' @keywords internal
NULL

#' Create AE Overview Table
#'
#' Generates a standard adverse event overview table including TEAEs,
#' related AEs, SAEs, and discontinuations.
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts data frame (must contain TRT01A and N columns)
#' @param title Table title (default: "Overview of Adverse Events")
#' @param trt_var Treatment variable name (default: "TRT01A")
#' @param subjid_var Subject ID variable name (default: "USUBJID")
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#'
#' @return A ClinicalTable object
#' @export
create_ae_overview_table <- function(
  adae,
  trt_n,
  title = "Overview of Adverse Events",
  trt_var = "TRT01A",
  subjid_var = "USUBJID",
  autofit = TRUE
) {
  if (!is.data.frame(adae)) {
    cli::cli_abort("{.arg adae} must be a data frame")
  }
  if (!is.data.frame(trt_n)) {
    cli::cli_abort("{.arg trt_n} must be a data frame")
  }
  if (!all(c(trt_var, "N") %in% names(trt_n))) {
    cli::cli_abort("{.arg trt_n} must contain columns {trt_var} and 'N'")
  }

  # Helper for summarizing category
  summarize_category <- function(data, category_label) {
    if (nrow(data) == 0) {
      return(NULL)
    }

    data |>
      dplyr::group_by(!!rlang::sym(trt_var)) |>
      dplyr::summarise(
        n = dplyr::n_distinct(!!rlang::sym(subjid_var)),
        .groups = "drop"
      ) |>
      dplyr::left_join(trt_n, by = trt_var) |>
      dplyr::mutate(
        pct = round(.data$n / .data$N * 100, 1),
        Category = category_label
      )
  }

  categories <- list()

  # 1. Any TEAE
  if ("TRTEMFL" %in% names(adae)) {
    teae <- adae |> dplyr::filter(.data$TRTEMFL == "Y")
    categories[[1]] <- summarize_category(
      teae,
      "Subjects with at least one TEAE"
    )
  }

  # 2. Related TEAEs
  if ("TRTEMFL" %in% names(adae) && "AEREL" %in% names(adae)) {
    rel_teae <- adae |>
      dplyr::filter(
        .data$TRTEMFL == "Y",
        .data$AEREL %in% c("PROBABLE", "POSSIBLE", "RELATED")
      )
    categories[[2]] <- summarize_category(
      rel_teae,
      "Subjects with at least one related TEAE"
    )
  }

  # 3. SAEs
  if ("TRTEMFL" %in% names(adae) && "AESER" %in% names(adae)) {
    sae <- adae |>
      dplyr::filter(.data$TRTEMFL == "Y", .data$AESER == "Y")
    categories[[3]] <- summarize_category(
      sae,
      "Subjects with at least one SAE"
    )
  }

  # 4. Leading to Discontinuation
  if ("TRTEMFL" %in% names(adae) && "AEACN" %in% names(adae)) {
    disc <- adae |>
      dplyr::filter(.data$TRTEMFL == "Y", .data$AEACN == "DRUG WITHDRAWN")
    categories[[4]] <- summarize_category(
      disc,
      "Subjects with AE leading to discontinuation"
    )
  }

  # 5. Deaths (based on AEOUT)
  if ("TRTEMFL" %in% names(adae) && "AEOUT" %in% names(adae)) {
    fatal <- adae |>
      dplyr::filter(.data$TRTEMFL == "Y", .data$AEOUT == "FATAL")
    categories[[5]] <- summarize_category(fatal, "Deaths")
  }

  # Combine results
  overview_combined <- dplyr::bind_rows(categories)

  if (nrow(overview_combined) == 0) {
    cli::cli_warn("No adverse event data found matching criteria")
    return(NULL)
  }

  # Format for display: n (pct%)
  overview_formatted <- overview_combined |>
    dplyr::mutate(
      value = sprintf("%d (%.1f%%)", n, pct)
    ) |>
    dplyr::select("Category", !!rlang::sym(trt_var), "value") |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(trt_var),
      values_from = "value",
      values_fill = list(value = "0 (0.0%)")
    )

  # Create flextable
  ft <- create_hta_table(
    overview_formatted,
    title = title,
    col_widths = c(Category = 2.5),
    autofit = autofit
  )

  ClinicalTable(
    data = overview_formatted,
    flextable = ft,
    type = "ae_overview",
    title = title
  )
}

#' Create AE by System Organ Class Table
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_ae_soc_table <- function(
  adae,
  trt_n,
  title = "Adverse Events by System Organ Class",
  trt_var = "TRT01A",
  autofit = TRUE
) {
  soc_summary <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y") |>
    dplyr::group_by(!!rlang::sym(trt_var), .data$AEBODSYS) |>
    dplyr::summarise(
      n_subj = dplyr::n_distinct(.data$USUBJID),
      n_events = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(trt_n, by = trt_var) |>
    dplyr::mutate(
      pct = round(.data$n_subj / .data$N * 100, 1),
      display = paste0(.data$n_subj, " (", .data$pct, "%)")
    ) |>
    dplyr::select("AEBODSYS", !!rlang::sym(trt_var), "display") |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(trt_var),
      values_from = "display",
      values_fill = "0 (0.0%)"
    ) |>
    dplyr::rename(`System Organ Class` = "AEBODSYS") |>
    dplyr::arrange(.data$`System Organ Class`)

  ft <- create_hta_table(
    soc_summary,
    title = title,
    footnotes = c(
      "Safety Population",
      "n (%) = Number (percentage) of subjects with at least one event"
    ),
    autofit = autofit
  )

  ClinicalTable(
    data = soc_summary,
    flextable = ft,
    type = "ae_soc",
    title = title
  )
}

#' Create AE PT Table for a specific SOC
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param soc SOC value to filter by
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_ae_pt_table_for_soc <- function(
  adae,
  trt_n,
  soc,
  trt_var = "TRT01A",
  autofit = TRUE
) {
  pt_summary <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y", .data$AEBODSYS == soc) |>
    dplyr::group_by(!!rlang::sym(trt_var), .data$AEDECOD) |>
    dplyr::summarise(
      n_subj = dplyr::n_distinct(.data$USUBJID),
      .groups = "drop"
    ) |>
    dplyr::left_join(trt_n, by = trt_var) |>
    dplyr::mutate(
      pct = round(.data$n_subj / .data$N * 100, 1),
      display = paste0(.data$n_subj, " (", .data$pct, "%)")
    ) |>
    dplyr::select("AEDECOD", !!rlang::sym(trt_var), "display") |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(trt_var),
      values_from = "display",
      values_fill = "0 (0.0%)"
    ) |>
    dplyr::rename(`Preferred Term` = "AEDECOD") |>
    dplyr::arrange(.data$`Preferred Term`)

  ft <- create_hta_table(
    pt_summary,
    title = soc,
    footnotes = c(
      "Safety Population",
      "n (%) = Number (percentage) of subjects with at least one event"
    ),
    autofit = autofit
  )

  ClinicalTable(
    data = pt_summary,
    flextable = ft,
    type = "ae_pt_soc",
    title = soc
  )
}

#' Create AE PT Tables for all SOCs
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return List of ClinicalTable objects
#' @export
create_ae_pt_tables_by_soc <- function(
  adae,
  trt_n,
  trt_var = "TRT01A",
  autofit = TRUE
) {
  socs <- sort(unique(adae$AEBODSYS[adae$TRTEMFL == "Y"]))

  lapply(socs, function(soc) {
    create_ae_pt_table_for_soc(
      adae = adae,
      trt_n = trt_n,
      soc = soc,
      trt_var = trt_var,
      autofit = autofit
    )
  })
}

#' Calculate AE TTE Data for a specific SOC
#'
#' @param adsl ADSL data frame
#' @param adae ADAE data frame
#' @param soc SOC value to filter by
#' @param trt_var Treatment variable name
#' @return Data frame formatted for KM plotting
#' @export
calculate_ae_tte_data <- function(
  adsl,
  adae,
  soc,
  trt_var = "TRT01A"
) {
  # Define event of interest: Time to first event in target SOC
  first_event <- adae |>
    dplyr::filter(.data$AEBODSYS == soc, .data$TRTEMFL == "Y") |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::arrange(.data$ASTDY) |>
    dplyr::slice(1) |>
    dplyr::select("USUBJID", "ASTDY") |>
    dplyr::mutate(event = 1)

  # Merge with ADSL
  tte_data <- adsl |>
    dplyr::filter(.data$SAFFL == "Y") |>
    dplyr::left_join(first_event, by = "USUBJID")

  # Derive time and event
  # If TRTDURD is not available, derive it from TRTEDT and TRTSDT
  if (!"TRTDURD" %in% names(tte_data)) {
    if ("TRTEDT" %in% names(tte_data) && "TRTSDT" %in% names(tte_data)) {
      tte_data$TRTDURD <- as.numeric(tte_data$TRTEDT - tte_data$TRTSDT) +
        1
    } else {
      cli::cli_abort(
        c(
          "Cannot calculate treatment duration for time-to-event analysis",
          "x" = "TRTDURD, TRTEDT, and TRTSDT are all missing from the data"
        )
      )
    }
  }

  tte_data <- tte_data |>
    dplyr::mutate(
      event = ifelse(!is.na(.data$event), 1, 0),
      time = ifelse(.data$event == 1, .data$ASTDY, .data$TRTDURD),
      time = ifelse(is.na(.data$time), .data$TRTDURD, .data$time)
    ) |>
    dplyr::filter(!is.na(.data$time), !is.na(!!rlang::sym(trt_var)))

  return(tte_data)
}

#' Create AE KM Plot for a specific SOC
#'
#' @param adsl ADSL data frame
#' @param adae ADAE data frame
#' @param soc SOC value to filter by
#' @param trt_var Treatment variable name
#' @return ClinicalPlot object
#' @export
create_ae_km_plot_for_soc <- function(
  adsl,
  adae,
  soc,
  trt_var = "TRT01A"
) {
  tte_data <- calculate_ae_tte_data(adsl, adae, soc, trt_var)

  if (nrow(tte_data) == 0) {
    return(NULL)
  }

  create_km_plot(
    data = tte_data,
    time_var = "time",
    event_var = "event",
    trt_var = trt_var,
    title = paste("Time to First", soc),
    xlab = "Days",
    ylab = "Probability of No Event",
    risk_table = TRUE
  )
}

#' Create Most Common AE Table
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param title Table title
#' @param n_top Number of top PTs to show
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_common_ae_table <- function(
  adae,
  trt_n,
  title = "Most Common Adverse Events",
  n_top = 15,
  trt_var = "TRT01A",
  autofit = TRUE
) {
  top_pts <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y") |>
    dplyr::group_by(.data$AEDECOD) |>
    dplyr::summarise(
      n = dplyr::n_distinct(.data$USUBJID),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::slice_head(n = n_top) |>
    dplyr::pull(.data$AEDECOD)

  common_summary <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y", .data$AEDECOD %in% top_pts) |>
    dplyr::group_by(!!rlang::sym(trt_var), .data$AEBODSYS, .data$AEDECOD) |>
    dplyr::summarise(
      n_subj = dplyr::n_distinct(.data$USUBJID),
      .groups = "drop"
    ) |>
    dplyr::left_join(trt_n, by = trt_var) |>
    dplyr::mutate(
      pct = round(.data$n_subj / .data$N * 100, 1),
      display = paste0(.data$n_subj, " (", .data$pct, "%)")
    ) |>
    dplyr::select(
      "AEBODSYS",
      "AEDECOD",
      !!rlang::sym(trt_var),
      "display"
    ) |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(trt_var),
      values_from = "display",
      values_fill = "0 (0.0%)"
    ) |>
    dplyr::rename(
      `System Organ Class` = "AEBODSYS",
      `Preferred Term` = "AEDECOD"
    ) |>
    dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)

  ft <- create_hta_table(
    common_summary,
    title = title,
    footnotes = c(
      "Safety Population",
      sprintf(
        "Showing top %d most frequently reported Preferred Terms",
        n_top
      ),
      "n (%) = Number (percentage) of subjects with event"
    ),
    autofit = autofit
  )

  ClinicalTable(
    data = common_summary,
    flextable = ft,
    type = "ae_common",
    title = title
  )
}

#' Create AE by Maximum Severity Table
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_ae_severity_table <- function(
  adae,
  trt_n,
  title = "Subjects by Maximum Adverse Event Severity",
  trt_var = "TRT01A",
  autofit = TRUE
) {
  severity_summary <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y") |>
    dplyr::group_by(!!rlang::sym(trt_var), .data$USUBJID) |>
    dplyr::summarise(
      max_sev = max(.data$AESEV, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(!!rlang::sym(trt_var), .data$max_sev) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::left_join(trt_n, by = trt_var) |>
    dplyr::mutate(
      pct = round(.data$n / .data$N * 100, 1),
      display = paste0(.data$n, " (", .data$pct, "%)")
    ) |>
    dplyr::select("max_sev", !!rlang::sym(trt_var), "display") |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(trt_var),
      values_from = "display",
      values_fill = "0 (0.0%)"
    ) |>
    dplyr::rename(`Maximum Severity` = "max_sev")

  ft <- create_hta_table(
    severity_summary,
    title = title,
    footnotes = c(
      "Safety Population",
      "Maximum severity across all TEAEs per subject",
      "n (%) = Number (percentage) of subjects"
    ),
    autofit = autofit
  )

  ClinicalTable(
    data = severity_summary,
    flextable = ft,
    type = "ae_severity",
    title = title
  )
}

#' Create AE by Relationship Table
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_ae_relationship_table <- function(
  adae,
  trt_n,
  title = "Adverse Events by Relationship to Study Drug",
  trt_var = "TRT01A",
  autofit = TRUE
) {
  rel_summary <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y") |>
    dplyr::group_by(!!rlang::sym(trt_var), .data$AEREL) |>
    dplyr::summarise(
      n_subj = dplyr::n_distinct(.data$USUBJID),
      .groups = "drop"
    ) |>
    dplyr::left_join(trt_n, by = trt_var) |>
    dplyr::mutate(
      pct = round(.data$n_subj / .data$N * 100, 1),
      display = paste0(.data$n_subj, " (", .data$pct, "%)")
    ) |>
    dplyr::select("AEREL", !!rlang::sym(trt_var), "display") |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(trt_var),
      values_from = "display",
      values_fill = "0 (0.0%)"
    ) |>
    dplyr::rename(`Relationship to Study Drug` = "AEREL")

  ft <- create_hta_table(
    rel_summary,
    title = title,
    footnotes = c(
      "Safety Population",
      "Subjects counted once per relationship category",
      "n (%) = Number (percentage) of subjects"
    ),
    autofit = autofit
  )

  ClinicalTable(
    data = rel_summary,
    flextable = ft,
    type = "ae_relationship",
    title = title
  )
}

#' Create SAE Table
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_sae_table <- function(
  adae,
  trt_n,
  title = "Serious Adverse Events",
  trt_var = "TRT01A",
  autofit = TRUE
) {
  sae <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y", .data$AESER == "Y")

  if (nrow(sae) == 0) {
    summary_df <- data.frame(
      Message = "No serious adverse events reported during the study"
    )
  } else {
    summary_df <- sae |>
      dplyr::group_by(
        !!rlang::sym(trt_var),
        .data$AEBODSYS,
        .data$AEDECOD
      ) |>
      dplyr::summarise(
        n_subj = dplyr::n_distinct(.data$USUBJID),
        .groups = "drop"
      ) |>
      dplyr::left_join(trt_n, by = trt_var) |>
      dplyr::mutate(
        pct = round(.data$n_subj / .data$N * 100, 1),
        display = paste0(.data$n_subj, " (", .data$pct, "%)")
      ) |>
      dplyr::select(
        "AEBODSYS",
        "AEDECOD",
        !!rlang::sym(trt_var),
        "display"
      ) |>
      tidyr::pivot_wider(
        names_from = !!rlang::sym(trt_var),
        values_from = "display",
        values_fill = "0 (0.0%)"
      ) |>
      dplyr::rename(
        `System Organ Class` = "AEBODSYS",
        `Preferred Term` = "AEDECOD"
      ) |>
      dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)
  }

  ft <- create_hta_table(
    summary_df,
    title = title,
    footnotes = c(
      "Safety Population",
      "SAE = Serious Adverse Event (AESER = 'Y')",
      "n (%) = Number (percentage) of subjects"
    )
  )

  ClinicalTable(
    data = summary_df,
    flextable = ft,
    type = "sae",
    title = title
  )
}

#' Create AE Leading to Discontinuation Table
#'
#' @param adae ADAE data frame
#' @param trt_n Treatment group counts
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_ae_discontinuation_table <- function(
  adae,
  trt_n,
  title = "Adverse Events Leading to Study Drug Discontinuation",
  trt_var = "TRT01A",
  autofit = TRUE
) {
  disc <- adae |>
    dplyr::filter(.data$TRTEMFL == "Y", .data$AEACN == "DRUG WITHDRAWN")

  if (nrow(disc) == 0) {
    summary_df <- data.frame(
      Message = "No adverse events leading to study drug discontinuation"
    )
  } else {
    summary_df <- disc |>
      dplyr::group_by(
        !!rlang::sym(trt_var),
        .data$AEBODSYS,
        .data$AEDECOD
      ) |>
      dplyr::summarise(
        n_subj = dplyr::n_distinct(.data$USUBJID),
        .groups = "drop"
      ) |>
      dplyr::left_join(trt_n, by = trt_var) |>
      dplyr::mutate(
        pct = round(.data$n_subj / .data$N * 100, 1),
        display = paste0(.data$n_subj, " (", .data$pct, "%)")
      ) |>
      dplyr::select(
        "AEBODSYS",
        "AEDECOD",
        !!rlang::sym(trt_var),
        "display"
      ) |>
      tidyr::pivot_wider(
        names_from = !!rlang::sym(trt_var),
        values_from = "display",
        values_fill = "0 (0.0%)"
      ) |>
      dplyr::rename(
        `System Organ Class` = "AEBODSYS",
        `Preferred Term` = "AEDECOD"
      ) |>
      dplyr::arrange(.data$`System Organ Class`, .data$`Preferred Term`)
  }

  ft <- create_hta_table(
    summary_df,
    title = title,
    footnotes = c(
      "Safety Population",
      "AEACN = 'DRUG WITHDRAWN'",
      "n (%) = Number (percentage) of subjects"
    ),
    autofit = autofit
  )

  ClinicalTable(
    data = summary_df,
    flextable = ft,
    type = "ae_discontinuation",
    title = title
  )
}

#' Create Deaths Summary Table
#'
#' @param adsl ADSL data frame
#' @param title Table title
#' @param trt_var Treatment variable name
#' @param autofit Logical, whether to autofit column widths (default: TRUE)
#' @return ClinicalTable object
#' @export
create_deaths_table <- function(
  adsl,
  title = "Deaths Summary",
  trt_var = "TRT01A",
  autofit = TRUE
) {
  death_summary <- adsl |>
    dplyr::filter(.data$SAFFL == "Y") |>
    dplyr::group_by(!!rlang::sym(trt_var)) |>
    dplyr::summarise(
      N = dplyr::n(),
      n_deaths = sum(.data$DTHFL == "Y", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct = round(.data$n_deaths / .data$N * 100, 1),
      `Deaths n (%)` = paste0(.data$n_deaths, " (", .data$pct, "%)"),
      N = as.character(.data$N)
    ) |>
    dplyr::select(!!rlang::sym(trt_var), "N", "Deaths n (%)")

  death_wide <- death_summary |>
    tidyr::pivot_longer(
      cols = c("N", "Deaths n (%)"),
      names_to = "Statistic",
      values_to = "Value"
    ) |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(trt_var),
      values_from = "Value"
    ) |>
    dplyr::mutate(
      Statistic = dplyr::case_when(
        .data$Statistic == "N" ~ "Safety Population (N)",
        TRUE ~ .data$Statistic
      )
    )

  ft <- create_hta_table(
    death_wide,
    title = title,
    footnotes = c(
      "Safety Population",
      "DTHFL = 'Y'",
      "n (%) = Number (percentage) of subjects"
    ),
    autofit = autofit
  )

  ClinicalTable(
    data = death_wide,
    flextable = ft,
    type = "deaths",
    title = title
  )
}
