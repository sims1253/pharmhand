# Create Vital Signs by Visit Table

Generates a table displaying vital signs measurements across multiple
visits, showing sample size, mean, and standard deviation by treatment
group.

## Usage

``` r
create_vs_by_visit_table(
  data,
  paramcd = "SYSBP",
  visits = c("Baseline", "Week 2", "Week 4", "Week 8", "End of Treatment"),
  trt_var = "TRT01P",
  title = "Vital Signs by Visit",
  footnotes = character(),
  autofit = TRUE,
  ...
)
```

## Arguments

- data:

  An ADaMData object (with domain "ADVS") or an ADVS data frame.
  Required columns include: PARAMCD, AVISIT, AVAL, and the treatment
  variable.

- paramcd:

  Parameter code to analyze (e.g., "SYSBP", "DIABP").

- visits:

  Vector of visits to include in the table (e.g., c("Baseline", "Week
  2", "Week 4", "Week 8", "End of Treatment")).

- trt_var:

  Treatment variable name (default: "TRT01P"). Ignored for ADaMData
  objects which use their own trt_var property.

- title:

  Table title.

- footnotes:

  Character vector of footnotes to append to the table.

- autofit:

  Logical, whether to autofit column widths (default: TRUE).

- ...:

  Additional arguments passed to
  [`create_clinical_table`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)

## Value

A ClinicalTable S7 object with the formatted vital signs by visit table.

## Examples

``` r
# Create vital signs by visit table
advs <- data.frame(
  USUBJID = rep(c("01", "02", "03", "04"), each = 2),
  TRT01P = rep(c("Placebo", "Placebo", "Active", "Active"), each = 2),
  PARAMCD = rep("SYSBP", 8),
  AVISIT = c("Baseline", "Week 2", "Baseline", "Week 2",
             "Baseline", "Week 2", "Baseline", "Week 2"),
  AVAL = c(120, 118, 125, 122, 118, 112, 122, 115)
)
table <- create_vs_by_visit_table(advs, paramcd = "SYSBP")
#> Automatically wrapping data.frame in ADaMData object
table@type
#> [1] "vs_by_visit"
```
