# Conducts network meta-analysis (NMA) to compare multiple treatments simultaneously using direct and indirect evidence.

Conducts network meta-analysis (NMA) to compare multiple treatments
simultaneously using direct and indirect evidence.

## Usage

``` r
network_meta(
  data,
  study_var = "study",
  treat1_var = "treat1",
  treat2_var = "treat2",
  effect_var = "effect",
  se_var = "se",
  reference = NULL,
  effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
  model = c("random", "fixed"),
  method = c("bucher", "graph"),
  conf_level = 0.95
)
```

## Arguments

- data:

  Data frame with study-level data. Required columns: study (study
  identifier), treat1 (treatment 1), treat2 (treatment 2), effect
  (effect estimate), se (standard error)

- study_var:

  Character. Study identifier column. Default: "study"

- treat1_var:

  Character. Treatment 1 column. Default: "treat1"

- treat2_var:

  Character. Treatment 2 column. Default: "treat2"

- effect_var:

  Character. Effect estimate column. Default: "effect"

- se_var:

  Character. Standard error column. Default: "se"

- reference:

  Character. Reference treatment. Default: first alphabetically

- effect_measure:

  Character. Effect type: "hr", "or", "rr", "rd", "md", "smd"

- model:

  Character. "fixed" or "random". Default: "random"

- method:

  Character. NMA method: "bucher" (simple), "graph" (not yet
  implemented, defaults to bucher)

- conf_level:

  Numeric. Confidence level. Default: 0.95

## Value

List with relative effects, rankings, and network structure

## Details

For ratio measures ("hr", "or", "rr"), effect estimates must be on the
**log scale**. For difference measures ("rd", "md", "smd"), effect
estimates must be on the **raw scale**.

## Examples

``` r
# Network meta-analysis of 4 studies
nma_data <- data.frame(
  study = c("S1", "S2", "S3", "S4"),
  treat1 = c("A", "B", "A", "B"),
  treat2 = c("B", "C", "C", "D"),
  effect = log(c(0.75, 0.90, 0.80, 0.85)),
  se = c(0.12, 0.15, 0.18, 0.14)
)
result <- network_meta(nma_data, effect_measure = "hr")
result@comparisons
#>   treatment vs estimate  ci_lower  ci_upper        se n_studies  evidence rank
#> 1         A  A   1.0000 1.0000000 1.0000000 0.0000000        NA reference   NA
#> B         B  A   0.7500 0.5928121 0.9488672 0.1200000         1    direct    2
#> C         C  A   0.8000 0.5621778 1.1384298 0.1800000         1    direct    3
#> D         D  A   0.6375 0.4441466 0.9150272 0.1843909         2  indirect    1
result@network$treatments
#> [1] "A" "B" "C" "D"
```
