# Build League Table Matrix

Internal helper to construct the league table data frame from NMA
results. Calculates all pairwise comparisons and formats them into a
matrix structure.

## Usage

``` r
.build_league_matrix(
  nma_result,
  digits = 2,
  show_ci = TRUE,
  highlight_sig = TRUE,
  conf_level = 0.95
)
```

## Arguments

- nma_result:

  Result from network_meta()

- digits:

  Integer. Decimal places for estimates

- show_ci:

  Logical. Show confidence intervals

- highlight_sig:

  Logical. Highlight significant comparisons

- conf_level:

  Numeric. Confidence level

## Value

Data frame with league table matrix
