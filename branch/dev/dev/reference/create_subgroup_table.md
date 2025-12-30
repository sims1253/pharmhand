# Create Subgroup Analysis Table

Generates a tabular summary of subgroup analyses for HTA dossiers and
Word documents. Provides the same information as
[`create_forest_plot()`](https://sims1253.github.io/pharmhand/dev/reference/create_forest_plot.md)
in table format showing treatment effects (HR or OR) by subgroup with
confidence intervals and interaction p-values.

## Usage

``` r
create_subgroup_table(
  data,
  subgroups,
  endpoint_type = c("tte", "binary"),
  time_var = "AVAL",
  event_var = "CNSR",
  response_var = "AVALC",
  response_values = c("CR", "PR"),
  trt_var = "TRT01P",
  ref_group = NULL,
  conf_level = 0.95,
  show_interaction = TRUE,
  title = "Subgroup Analysis",
  autofit = TRUE
)
```

## Arguments

- data:

  ADaMData object or data frame

- subgroups:

  Named list mapping variable names to display labels, e.g.,
  `list(AGEGR1 = "Age Group", SEX = "Sex", RACE = "Race")`

- endpoint_type:

  "tte" for time-to-event (HR) or "binary" for binary outcomes (OR)

- time_var:

  Time variable for TTE endpoints (default: "AVAL")

- event_var:

  Event variable for TTE endpoints. If "CNSR", will be inverted
  automatically. Default: "CNSR"

- response_var:

  Response variable for binary endpoints (default: "AVALC")

- response_values:

  Values indicating response for binary endpoints (default: c("CR",
  "PR"))

- trt_var:

  Treatment variable name (default: "TRT01P")

- ref_group:

  Reference treatment group. If NULL, uses first level.

- conf_level:

  Confidence level (default: 0.95)

- show_interaction:

  Logical, calculate and show interaction p-values (default: TRUE)

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
# TTE subgroup table
sg_table <- create_subgroup_table(
  data = adtte,
  subgroups = list(
    AGEGR1 = "Age Group",
    SEX = "Sex"
  ),
  endpoint_type = "tte",
  title = "Subgroup Analysis - Overall Survival"
)

# Binary endpoint subgroup table
sg_table <- create_subgroup_table(
  data = adrs,
  subgroups = list(SEX = "Sex"),
  endpoint_type = "binary",
  response_values = c("CR", "PR"),
  title = "Response Rate by Subgroup"
)
} # }
```
