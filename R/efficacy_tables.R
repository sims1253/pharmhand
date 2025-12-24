#' Efficacy Analysis Tables
#'
#' Reusable functions for creating standard efficacy analysis tables
#' including endpoints, change from baseline, lab summaries, and shifts.
#'
#' @name efficacy_tables
#' @keywords internal
NULL

#' Create Primary Endpoint Summary Table
#'
#' @param advs ADVS data frame
#' @param trt_n Treatment group counts
#' @param paramcd Parameter code to analyze (default: "SYSBP")
#' @param visit Visit to analyze (default: "End of Treatment")
#' @param title Table title
#' @return ClinicalTable object
#' @export
create_primary_endpoint_table <- function(
    advs,
    trt_n,
    paramcd = "SYSBP",
    visit = "End of Treatment",
    title = "Primary Endpoint Summary",
    autofit = TRUE
) {
    # Input validation
    if (!is.data.frame(advs)) {
        cli::cli_abort("{.arg advs} must be a data frame")
    }
    if (!is.data.frame(trt_n)) {
        cli::cli_abort("{.arg trt_n} must be a data frame")
    }

    # Filter and summarize
    primary_data <- advs |>
        dplyr::filter(
            .data$PARAMCD == paramcd,
            .data$AVISIT == visit
        ) |>
        dplyr::group_by(.data$TRT01P) |>
        dplyr::summarise(
            n = dplyr::n(),
            Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
            SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
            Median = round(median(.data$AVAL, na.rm = TRUE), 1),
            Min = round(min(.data$AVAL, na.rm = TRUE), 1),
            Max = round(max(.data$AVAL, na.rm = TRUE), 1),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            `Mean (SD)` = paste0(.data$Mean, " (", .data$SD, ")"),
            `Min, Max` = paste0(.data$Min, ", ", .data$Max),
            n = as.character(.data$n),
            Median = as.character(.data$Median)
        ) |>
        dplyr::select("TRT01P", "n", "Mean (SD)", "Median", "Min, Max")

    # Transpose
    primary_wide <- primary_data |>
        tidyr::pivot_longer(
            cols = c("n", "Mean (SD)", "Median", "Min, Max"),
            names_to = "Statistic"
        ) |>
        tidyr::pivot_wider(
            names_from = "TRT01P",
            values_from = "value"
        )

    # Styling
    primary_ft <- create_hta_table(
        primary_wide,
        title = title,
        footnotes = c(
            "Safety Population",
            paste(paramcd, "=", paramcd, "(analyzed at", visit, ")")
        ),
        autofit = autofit
    )

    ClinicalTable(
        data = primary_wide,
        flextable = primary_ft,
        type = "primary_endpoint",
        title = title,
        metadata = list(param = paramcd, visit = visit)
    )
}

#' Create Change from Baseline Summary Table
#'
#' @param advs ADVS data frame
#' @param trt_n Treatment group counts
#' @param params Vector of parameter codes to include
#' @param visit Visit to analyze
#' @param title Table title
#' @return ClinicalTable object
#' @export
create_cfb_summary_table <- function(
    advs,
    trt_n,
    params = c("SYSBP", "DIABP", "PULSE"),
    visit = "End of Treatment",
    title = "Change from Baseline Summary",
    autofit = TRUE
) {
    cfb_data <- advs |>
        dplyr::filter(
            .data$PARAMCD %in% params,
            !is.na(.data$CHG),
            .data$AVISIT == visit
        ) |>
        dplyr::group_by(.data$TRT01P, .data$PARAM) |>
        dplyr::summarise(
            n = dplyr::n(),
            Mean_CFB = round(mean(.data$CHG, na.rm = TRUE), 2),
            SD_CFB = round(sd(.data$AVAL, na.rm = TRUE), 2),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            display = paste0(.data$Mean_CFB, " (", .data$SD_CFB, ")"),
            n = as.character(.data$n)
        ) |>
        dplyr::select("PARAM", "TRT01P", "n", "display")

    cfb_wide <- cfb_data |>
        tidyr::pivot_wider(
            names_from = "TRT01P",
            values_from = c("n", "display"),
            names_glue = "{TRT01P}_{.value}"
        )

    # Rename columns for display
    names(cfb_wide) <- gsub("_display$", " Mean (SD)", names(cfb_wide))
    names(cfb_wide) <- gsub("_n$", " n", names(cfb_wide))
    names(cfb_wide) <- gsub("^PARAM$", "Parameter", names(cfb_wide))

    cfb_ft <- create_hta_table(
        cfb_wide,
        title = title,
        footnotes = c(
            "Safety Population",
            "Mean (SD) presented for change from baseline"
        ),
        autofit = autofit
    )

    ClinicalTable(
        data = cfb_wide,
        flextable = cfb_ft,
        type = "cfb",
        title = title
    )
}

#' Create Vital Signs by Visit Table
#'
#' @param advs ADVS data frame
#' @param trt_n Treatment group counts
#' @param paramcd Parameter code to analyze
#' @param visits Vector of visits to include
#' @param title Table title
#' @return ClinicalTable object
#' @export
create_vs_by_visit_table <- function(
    advs,
    trt_n,
    paramcd = "SYSBP",
    visits = c("Baseline", "Week 2", "Week 4", "Week 8", "End of Treatment"),
    title = "Vital Signs by Visit",
    autofit = TRUE
) {
    vs_by_visit <- advs |>
        dplyr::filter(
            .data$PARAMCD == paramcd,
            .data$AVISIT %in% visits
        ) |>
        dplyr::group_by(.data$TRT01P, .data$AVISIT) |>
        dplyr::summarise(
            n = dplyr::n(),
            Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
            SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            display = paste0(.data$n, " / ", .data$Mean, " (", .data$SD, ")")
        ) |>
        dplyr::select("AVISIT", "TRT01P", "display") |>
        tidyr::pivot_wider(
            names_from = "TRT01P",
            values_from = "display",
            values_fill = "--"
        ) |>
        dplyr::rename(Visit = "AVISIT")

    # Order visits
    vs_by_visit <- vs_by_visit |>
        dplyr::mutate(
            Visit = factor(.data$Visit, levels = visits)
        ) |>
        dplyr::arrange(.data$Visit) |>
        dplyr::mutate(Visit = as.character(.data$Visit))

    vs_ft <- create_hta_table(
        vs_by_visit,
        title = title,
        footnotes = c(
            "Safety Population",
            "Format: n / Mean (SD)"
        ),
        autofit = autofit
    )

    ClinicalTable(
        data = vs_by_visit,
        flextable = vs_ft,
        type = "vs_by_visit",
        title = title
    )
}

#' Create Laboratory Summary Table
#'
#' @param adlb ADLB data frame
#' @param trt_n Treatment group counts
#' @param params Vector of parameter codes to analyze
#' @param visit Visit to analyze
#' @param title Table title
#' @return ClinicalTable object
#' @export
create_lab_summary_table <- function(
    adlb,
    trt_n,
    params = c("HGB", "WBC", "PLAT", "ALT", "AST", "BILI", "CREAT"),
    visit = "Week 24",
    title = "Laboratory Parameters Summary",
    autofit = TRUE
) {
    lab_data <- adlb |>
        dplyr::filter(
            .data$PARAMCD %in% params,
            .data$AVISIT == visit
        ) |>
        dplyr::group_by(.data$TRT01P, .data$PARAM) |>
        dplyr::summarise(
            n = dplyr::n(),
            Mean = round(mean(.data$AVAL, na.rm = TRUE), 2),
            SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            display = paste0(.data$Mean, " (", .data$SD, ")"),
            n = as.character(.data$n)
        ) |>
        dplyr::select("PARAM", "TRT01P", "n", "display") |>
        tidyr::pivot_wider(
            names_from = "TRT01P",
            values_from = c("n", "display"),
            names_glue = "{TRT01P}_{.value}"
        )

    names(lab_data) <- gsub("_display$", " Mean (SD)", names(lab_data))
    names(lab_data) <- gsub("_n$", " n", names(lab_data))
    names(lab_data) <- gsub("^PARAM$", "Parameter", names(lab_data))

    lab_ft <- create_hta_table(
        lab_data,
        title = title,
        footnotes = c("Safety Population", "Mean (SD) presented"),
        autofit = autofit
    )

    ClinicalTable(
        data = lab_data,
        flextable = lab_ft,
        type = "lab_summary",
        title = title
    )
}

#' Create Laboratory Shift Table
#'
#' @param adlb ADLB data frame
#' @param trt_n Treatment group counts
#' @param paramcd Parameter code to analyze
#' @param visit Visit to analyze
#' @param title Table title
#' @return ClinicalTable object
#' @export
create_lab_shift_table <- function(
    adlb,
    trt_n,
    paramcd = "ALT",
    visit = "Week 24",
    title = "Laboratory Shift Table",
    autofit = TRUE
) {
    shift_data <- adlb |>
        dplyr::filter(
            .data$PARAMCD == paramcd,
            !is.na(.data$BNRIND),
            !is.na(.data$ANRIND),
            .data$AVISIT == visit
        ) |>
        dplyr::group_by(.data$TRT01P, .data$BNRIND, .data$ANRIND) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        tidyr::pivot_wider(
            names_from = "ANRIND",
            values_from = "n",
            values_fill = 0
        ) |>
        dplyr::rename(`Baseline Status` = "BNRIND")

    shift_wide <- shift_data |>
        dplyr::mutate(Treatment = .data$TRT01P) |>
        dplyr::select(
            "Treatment",
            "Baseline Status",
            dplyr::everything(),
            -"TRT01P"
        )

    shift_ft <- create_hta_table(
        shift_wide,
        title = title,
        footnotes = c(
            "Safety Population",
            "Shift from baseline normal range indicator"
        ),
        autofit = autofit
    )

    ClinicalTable(
        data = shift_wide,
        flextable = shift_ft,
        type = "lab_shift",
        title = title
    )
}

#' Create Subgroup Analysis Table
#'
#' @param adsl ADSL data frame
#' @param advs ADVS data frame
#' @param paramcd Parameter code to analyze
#' @param visit Visit to analyze
#' @param subgroups List of subgroup variables (e.g. list(AGEGR1="Age Group"))
#' @param title Table title
#' @return ClinicalTable object
#' @export
create_subgroup_analysis_table <- function(
    adsl,
    advs,
    paramcd = "SYSBP",
    visit = "End of Treatment",
    subgroups = list(AGEGR1 = "Age Group", SEX = "Sex"),
    title = "Subgroup Analysis",
    autofit = TRUE
) {
    subgroup_data_raw <- advs |>
        dplyr::filter(
            .data$PARAMCD == paramcd,
            .data$AVISIT == visit
        )

    results_list <- list()

    # Iterate through subgroups
    for (var_name in names(subgroups)) {
        label <- subgroups[[var_name]]

        # Determine variable name (handle character vector or named list)
        # Using .data with string variable for group_by requires distinct handling or ensure it exists
        # To be safe, we check if variable is in advs (usually subgroup vars are in ADSL, so merging might be needed)
        # Assuming ADVS contains these or we need to merge.
        # The original code just used advs which comes from pharmaverseadam::advs which has standard vars.

        res <- subgroup_data_raw |>
            dplyr::group_by(.data$TRT01P, .data[[var_name]]) |>
            dplyr::summarise(
                n = dplyr::n(),
                Mean = round(mean(.data$AVAL, na.rm = TRUE), 1),
                SD = round(sd(.data$AVAL, na.rm = TRUE), 2),
                .groups = "drop"
            ) |>
            dplyr::mutate(
                Subgroup = label,
                Category = .data[[var_name]],
                display = paste0(
                    .data$n,
                    " / ",
                    .data$Mean,
                    " (",
                    .data$SD,
                    ")"
                )
            )
        results_list[[var_name]] <- res
    }

    all_subgroups <- dplyr::bind_rows(results_list) |>
        dplyr::select("Subgroup", "Category", "TRT01P", "display") |>
        tidyr::pivot_wider(
            names_from = "TRT01P",
            values_from = "display",
            values_fill = "--"
        )

    subgroup_ft <- create_hta_table(
        all_subgroups,
        title = title,
        footnotes = c(
            "Safety Population",
            "Format: n / Mean (SD)"
        ),
        autofit = autofit
    )

    ClinicalTable(
        data = all_subgroups,
        flextable = subgroup_ft,
        type = "subgroup",
        title = title
    )
}
