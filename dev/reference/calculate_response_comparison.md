# Calculate Response Comparison (OR, RR, or RD)

Calculate Response Comparison (OR, RR, or RD)

## Usage

``` r
calculate_response_comparison(
  df,
  trt_var,
  ref_group,
  comparison_type,
  conf_level
)
```

## Arguments

- df:

  Data frame with responder column

- trt_var:

  Treatment variable name

- ref_group:

  Reference group name

- comparison_type:

  "OR", "RR", or "RD"

- conf_level:

  Confidence level

## Value

Named list of comparison results per treatment group
