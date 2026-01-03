# Create Time-to-Event Summary Table

Generates a standard TTE summary table with median survival, confidence
intervals, hazard ratios, and optional landmark estimates for efficacy
endpoints like OS, PFS, EFS.

## Usage

``` r
create_tte_summary_table(
  data,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01P",
  ref_group = NULL,
  conf_level = 0.95,
  check_ph = TRUE,
  landmarks = NULL,
  time_unit = "months",
  title = "Time-to-Event Summary",
  autofit = TRUE
)
```

## Arguments

- data:

  ADaMData object or data frame containing TTE data

- time_var:

  Time variable name (default: "AVAL")

- event_var:

  Event indicator variable. If "CNSR" (ADaM censoring flag), it will be
  automatically inverted (0=event becomes 1=event). Otherwise expects
  1=event, 0=censor. Default: "CNSR"

- trt_var:

  Treatment variable name (default: "TRT01P")

- ref_group:

  Reference group for HR calculation. If NULL, uses first level of
  treatment variable.

- conf_level:

  Confidence level for intervals (default: 0.95)

- check_ph:

  Logical. Whether to test proportional hazards assumption and warn on
  violations (default: TRUE)

- landmarks:

  Numeric vector of timepoints for landmark survival estimates (e.g.,
  c(12, 24) for 12 and 24 month rates). NULL for none.

- time_unit:

  Character string for time unit display (default: "months")

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable object with TTE summary statistics

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
tte_table <- create_tte_summary_table(
  data = adtte,
  time_var = "AVAL",
  event_var = "CNSR",
  title = "Overall Survival Summary"
)

# With landmark estimates
tte_table <- create_tte_summary_table(
  data = adtte,
  landmarks = c(12, 24),
  time_unit = "months",
  title = "Progression-Free Survival"
)
} # }
```
