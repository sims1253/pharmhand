# Create Subgroup Forest Plot

Generates a forest plot showing treatment effects (hazard ratio or odds
ratio) across pre-specified subgroups with optional interaction
p-values.

## Usage

``` r
create_forest_plot(
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
  null_line = 1,
  title = "Subgroup Analysis",
  xlab = NULL,
  log_scale = TRUE,
  colors = NULL,
  base_size = 11
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

- null_line:

  Reference line value (default: 1 for HR/OR)

- title:

  Plot title

- xlab:

  X-axis label. If NULL, auto-generated based on endpoint_type.

- log_scale:

  Logical, use log scale for x-axis (default: TRUE)

- colors:

  Optional named vector of colors for estimate types

- base_size:

  Base font size for plot text elements (default: 11)

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
# TTE subgroup analysis
forest <- create_forest_plot(
  data = adtte,
  subgroups = list(
    AGEGR1 = "Age Group",
    SEX = "Sex",
    RACE = "Race"
  ),
  endpoint_type = "tte",
  title = "Subgroup Analysis - Overall Survival"
)

# Binary subgroup analysis
forest <- create_forest_plot(
  data = adrs,
  subgroups = list(SEX = "Sex", AGEGR1 = "Age"),
  endpoint_type = "binary",
  response_values = c("CR", "PR"),
  title = "Response Rate by Subgroup"
)
} # }
```
