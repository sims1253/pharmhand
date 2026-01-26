# Create Change from Baseline Summary Table

Generates a summary table showing change from baseline statistics for
vital signs or other continuous parameters. The table displays mean
change, standard deviation, and other descriptive statistics by
treatment group for an analysis visit.

## Usage

``` r
create_cfb_summary_table(
  data,
  params = c("SYSBP", "DIABP", "PULSE"),
  visit = "End of Treatment",
  trt_var = "TRT01P",
  title = "Change from Baseline Summary",
  footnotes = character(),
  autofit = TRUE,
  ...
)
```

## Arguments

- data:

  An ADaMData object (with domain "ADVS") or an ADVS data frame.
  Required columns include: PARAMCD (parameter code), PARAM (parameter
  name), AVISIT (analysis visit), CHG (change from baseline), and the
  treatment variable (default: "TRT01P" for data frames, or @trt_var for
  ADaMData).

- params:

  Character vector of PARAMCD values identifying which vital sign
  parameters to include in the table (e.g., c("SYSBP", "DIABP",
  "PULSE")). Must match values in the PARAMCD column of data.

- visit:

  Character string specifying the analysis visit to summarize. Must
  match a value in the AVISIT column of data (e.g., "End of Treatment",
  "Week 12").

- trt_var:

  Treatment variable name (default: "TRT01P"). Ignored for ADaMData
  objects which use their own trt_var property.

- title:

  Character string for the table title.

- footnotes:

  Character vector of footnotes to append to the table.

- autofit:

  Logical, whether to automatically adjust column widths to fit content.
  Default is TRUE.

- ...:

  Additional arguments passed to
  [`create_clinical_table`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)

## Value

A ClinicalTable S7 object with the formatted change-from-baseline
summary statistics table. The object includes the underlying data frame,
a formatted flextable for rendering, and metadata about the analysis.

## Examples

``` r
# Create change from baseline summary
advs <- data.frame(
  USUBJID = rep(c("01", "02", "03", "04"), each = 1),
  TRT01P = rep(c("Placebo", "Placebo", "Active", "Active"), each = 1),
  PARAMCD = rep("SYSBP", 4),
  PARAM = rep("Systolic Blood Pressure", 4),
  AVISIT = rep("End of Treatment", 4),
  CHG = c(-2.5, -3.1, -8.2, -7.5)
)
table <- create_cfb_summary_table(advs, params = "SYSBP")
#> Automatically wrapping data.frame in ADaMData object
table@type
#> [1] "cfb"
```
