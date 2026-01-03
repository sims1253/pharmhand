# Test Proportional Hazards Assumption

Tests the proportional hazards assumption for Cox regression models
using Schoenfeld residuals (cox.zph test).

## Usage

``` r
test_ph_assumption(
  data,
  time_var = NULL,
  event_var = NULL,
  trt_var = NULL,
  covariates = character(),
  alpha = 0.05,
  plot = FALSE
)
```

## Arguments

- data:

  Data frame with time-to-event data, or a coxph model object

- time_var:

  Character. Time variable (required if data is a data frame)

- event_var:

  Character. Event variable (required if data is a data frame)

- trt_var:

  Character. Treatment variable (required if data is a data frame)

- covariates:

  Character vector. Additional covariates to include

- alpha:

  Numeric. Significance level for flagging violations (default: 0.05)

- plot:

  Logical. Whether to create diagnostic plot (default: FALSE)

## Value

A list with:

- results: Data frame with variable, rho, chisq, p-value, violation flag

- global_test: Global test result (p-value)

- violation: Logical, TRUE if any p \< alpha

- model: The fitted coxph model

- zph: The cox.zph result object

- plot: ClinicalPlot if plot=TRUE

## References

Grambsch, P. M. and Therneau, T. M. (1994). Proportional hazards tests
and diagnostics based on weighted residuals. Biometrika, 81, 515-26.

IQWiG Methods v8.0, Section 10.3.12, p. 235-237.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- test_ph_assumption(
  data = adtte,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01P",
  plot = TRUE
)
} # }
```
