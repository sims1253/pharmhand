# Calculate Subgroup Effect (HR or OR)

Calculate Subgroup Effect (HR or OR)

## Usage

``` r
calculate_subgroup_effect(
  df,
  subgroup_var,
  subgroup_level,
  endpoint_type,
  time_var,
  event_var_use,
  trt_var,
  ref_group,
  conf_level
)
```

## Arguments

- df:

  Data frame

- subgroup_var:

  Subgroup variable name (NULL for overall)

- subgroup_level:

  Subgroup level value

- endpoint_type:

  "tte" or "binary"

- time_var:

  Time variable for TTE

- event_var_use:

  Event variable for TTE

- trt_var:

  Treatment variable

- ref_group:

  Reference group

- conf_level:

  Confidence level

## Value

List with estimate, CI, counts, etc.
