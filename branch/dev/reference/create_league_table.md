# Create League Table for Network Meta-Analysis

Generates a league table showing all pairwise treatment comparisons from
a network meta-analysis.

## Usage

``` r
create_league_table(
  nma_result,
  digits = 2,
  show_ci = TRUE,
  highlight_sig = TRUE
)
```

## Arguments

- nma_result:

  Result from network_meta()

- digits:

  Integer. Decimal places for estimates. Default: 2

- show_ci:

  Logical. Show confidence intervals. Default: TRUE

- highlight_sig:

  Logical. Highlight significant comparisons. Default: TRUE

## Value

ClinicalTable with league table matrix
