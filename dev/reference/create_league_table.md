# Generates a league table showing all pairwise treatment comparisons from a network meta-analysis.

Generates a league table showing all pairwise treatment comparisons from
a network meta-analysis.

## Usage

``` r
create_league_table(
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

  Integer. Decimal places for estimates. Default: 2

- show_ci:

  Logical. Show confidence intervals. Default: TRUE

- highlight_sig:

  Logical. Highlight significant comparisons. Default: TRUE

- conf_level:

  Numeric. Confidence level. Default: 0.95

## Value

ClinicalTable with league table matrix

## Examples

``` r
# Create league table for NMA
nma_data <- data.frame(
  study = c("S1", "S2", "S3"),
  treat1 = c("A", "B", "A"),
  treat2 = c("B", "C", "C"),
  effect = log(c(0.75, 0.90, 0.80)),
  se = c(0.12, 0.15, 0.18)
)
nma_result <- network_meta(nma_data, effect_measure = "hr")
table <- create_league_table(nma_result)
table@type
#> [1] "league_table"
```
