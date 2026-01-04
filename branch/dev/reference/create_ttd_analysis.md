# Create Time-to-Deterioration Analysis

Performs time-to-deterioration (TTD) analysis for PRO endpoints.
Deterioration is defined as a decrease from baseline exceeding the MCID.

## Usage

``` r
create_ttd_analysis(
  data,
  subject_var = "USUBJID",
  trt_var = "TRT01P",
  param_var = "PARAMCD",
  paramcd = NULL,
  value_var = "AVAL",
  base_var = "BASE",
  chg_var = "CHG",
  time_var = "ADY",
  visit_var = "AVISITN",
  threshold,
  direction = c("decrease", "increase"),
  definition = c("first", "confirmed"),
  confirmation_visits = 1,
  censor_at = NULL
)
```

## Arguments

- data:

  Data frame with longitudinal PRO data

- subject_var:

  Character. Subject ID variable. Default: "USUBJID"

- trt_var:

  Character. Treatment variable. Default: "TRT01P"

- param_var:

  Character. Parameter variable. Default: "PARAMCD"

- paramcd:

  Character. Parameter code to analyze. Default: NULL (uses first)

- value_var:

  Character. Value variable. Default: "AVAL"

- base_var:

  Character. Baseline value variable. Default: "BASE"

- chg_var:

  Character. Change from baseline variable. Default: "CHG"

- time_var:

  Character. Time/day variable. Default: "ADY"

- visit_var:

  Character. Visit number variable. Default: "AVISITN"

- threshold:

  Numeric. MCID threshold for deterioration (positive = improvement)

- direction:

  Character. "decrease" if lower is worse, "increase" if higher is worse

- definition:

  Character. "first" or "confirmed" deterioration. Default: "first"

- confirmation_visits:

  Integer. Visits to confirm (for definition="confirmed")

- censor_at:

  Numeric. Time to censor if no event. Default: max time in data

## Value

A list with components:

- ttd_data:

  Prepared time-to-deterioration data frame

- km_fit:

  Kaplan-Meier fit object from survfit

- summary_table:

  Summary statistics by treatment group

- threshold:

  MCID threshold used

- direction:

  Direction of deterioration

- definition:

  Deterioration definition used

- n_subjects:

  Total number of subjects

- n_events:

  Total number of deterioration events
