# Calculate Interaction P-value

Calculates the p-value for treatment-by-subgroup interaction using
likelihood ratio test. Used internally by forest plot functions.

## Usage

``` r
calculate_interaction_pvalue(
  df,
  subgroup_var,
  endpoint_type,
  time_var,
  event_var_use,
  trt_var
)
```

## Arguments

- df:

  Data frame containing analysis data with treatment, subgroup, and
  endpoint variables.

- subgroup_var:

  Character. Name of the subgroup variable column.

- endpoint_type:

  Character. Either "tte" for time-to-event (uses Cox regression) or
  "binary" for binary endpoints (uses logistic regression).

- time_var:

  Character. Name of time variable column (only used when endpoint_type
  = "tte").

- event_var_use:

  Character. Name of event indicator column (only used when
  endpoint_type = "tte").

- trt_var:

  Character. Name of treatment variable column.

## Value

Numeric. P-value from likelihood ratio test comparing model with and
without treatment-by-subgroup interaction term. Returns NA if model
fitting fails.
