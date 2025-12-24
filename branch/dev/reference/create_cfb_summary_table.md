# Create Change from Baseline Summary Table

Generates a summary table showing change from baseline statistics for
vital signs or other continuous parameters. The table displays mean
change, standard deviation, and other descriptive statistics by
treatment group for a specified analysis visit.

## Usage

``` r
create_cfb_summary_table(
  advs,
  trt_n,
  params = c("SYSBP", "DIABP", "PULSE"),
  visit = "End of Treatment",
  title = "Change from Baseline Summary",
  autofit = TRUE
)
```

## Arguments

- advs:

  An ADaM ADVS (Analysis Data Vital Signs) data frame. Required columns
  include: USUBJID, PARAMCD (parameter code), PARAM (parameter name),
  AVISIT (analysis visit), CHG (change from baseline), and the treatment
  variable (typically TRT01P).

- trt_n:

  A data frame or named vector containing treatment group counts. If a
  data frame, should contain treatment variable column and N column.
  Used for displaying "N=X" in column headers.

- params:

  Character vector of PARAMCD values identifying which vital sign
  parameters to include in the table (e.g., c("SYSBP", "DIABP",
  "PULSE")). Must match values in the PARAMCD column of advs.

- visit:

  Character string specifying the analysis visit to summarize. Must
  match a value in the AVISIT column of advs (e.g., "End of Treatment",
  "Week 12").

- title:

  Character string for the table title.

- autofit:

  Logical, whether to automatically adjust column widths to fit content.
  Default is TRUE.

## Value

A ClinicalTable S7 object containing the formatted change-from-baseline
summary statistics table. The object includes the underlying data frame,
a formatted flextable for rendering, and metadata about the analysis.
