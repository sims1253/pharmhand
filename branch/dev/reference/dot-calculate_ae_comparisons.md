# Calculate AE Comparisons for All Treatment Groups vs Reference

Internal helper function that computes risk differences, risk ratios,
and associated statistics for all treatment groups compared to a
reference.

## Usage

``` r
.calculate_ae_comparisons(
  ae_wide,
  trt_n,
  trt_groups,
  ref_group,
  trt_var,
  conf_level,
  include_nnh
)
```

## Arguments

- ae_wide:

  Wide-format AE counts with columns for each treatment's n and N values

- trt_n:

  Treatment counts data frame

- trt_groups:

  Treatment groups to compare (excluding reference)

- ref_group:

  Reference group name

- trt_var:

  Treatment variable name

- conf_level:

  Confidence level for intervals

- include_nnh:

  Logical, include NNH calculations

## Value

Modified ae_wide data frame with additional columns for statistics
