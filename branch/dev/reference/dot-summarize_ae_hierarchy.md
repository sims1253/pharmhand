# Summarize AE Hierarchy Data

Internal function to summarize AE data by MedDRA hierarchy levels.

## Usage

``` r
.summarize_ae_hierarchy(
  adae,
  trt_n,
  trt_var,
  level_vars,
  available_levels,
  min_pct,
  sort_by
)
```

## Arguments

- adae:

  ADaMData object with ADAE data

- trt_n:

  Treatment counts from ADaMData property

- trt_var:

  Treatment variable name

- level_vars:

  Named list mapping level names to variable names

- available_levels:

  Character vector of available hierarchy levels

- min_pct:

  Minimum percentage threshold

- sort_by:

  Sort method ("frequency" or "alphabetical")

## Value

Data frame with summarized AE hierarchy data
