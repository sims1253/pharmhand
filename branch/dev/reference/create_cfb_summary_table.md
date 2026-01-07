# Create Change from Baseline Summary Table

Generates a summary table showing change from baseline statistics for
vital signs or other continuous parameters. The table displays mean
change, standard deviation, and other descriptive statistics by
treatment group for an analysis visit.

## Usage

``` r
create_cfb_summary_table(
  advs,
  params = c("SYSBP", "DIABP", "PULSE"),
  visit = "End of Treatment",
  trt_var = "TRT01P",
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

- params:

  Character vector of PARAMCD values identifying which vital sign
  parameters to include in the table (e.g., c("SYSBP", "DIABP",
  "PULSE")). Must match values in the PARAMCD column of advs.

- visit:

  Character string specifying the analysis visit to summarize. Must
  match a value in the AVISIT column of advs (e.g., "End of Treatment",
  "Week 12").

- trt_var:

  Treatment variable name (default: "TRT01P")

- title:

  Character string for the table title.

- autofit:

  Logical, whether to automatically adjust column widths to fit content.
  Default is TRUE.

## Value

A ClinicalTable S7 object with the formatted change-from-baseline
summary statistics table. The object includes the underlying data frame,
a formatted flextable for rendering, and metadata about the analysis.

## Examples

``` r
# Create change from baseline summary
advs <- data.frame(
  USUBJID = c("01", "02", "03", "04"),
  TRT01P = c("Placebo", "Placebo", "Active", "Active"),
  PARAMCD = rep("SYSBP", 4),
  PARAM = rep("Systolic Blood Pressure", 4),
  AVISIT = rep("End of Treatment", 4),
  CHG = c(-2.5, -3.1, -8.2, -7.5)
)
table <- create_cfb_summary_table(advs, params = "SYSBP")
table@type
#> [1] "cfb"
```
