# Calculate Interaction P-value

Calculate Interaction P-value

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

  Data frame

- subgroup_var:

  Subgroup variable

- endpoint_type:

  "tte" or "binary"

- time_var:

  Time variable for TTE

- event_var_use:

  Event variable for TTE

- trt_var:

  Treatment variable

## Value

Interaction p-value
