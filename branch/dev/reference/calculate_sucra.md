# Calculate Treatment Rankings (P-scores/SUCRA)

Calculates ranking probabilities and SUCRA (Surface Under Cumulative
Ranking curve) or P-scores for treatments in network meta-analysis.

## Usage

``` r
calculate_sucra(nma_result, lower_better = NULL, n_sim = 1000)
```

## Arguments

- nma_result:

  Result from network_meta()

- lower_better:

  Logical. Is lower estimate better? Default: TRUE for ratios

- n_sim:

  Integer. Number of simulations for ranking. Default: 1000

## Value

List with rankings, SUCRA/P-scores, and rankogram data

## Examples

``` r
# Calculate SUCRA rankings
nma_data <- data.frame(
  study = c("S1", "S2", "S3"),
  treat1 = c("A", "B", "A"),
  treat2 = c("B", "C", "C"),
  effect = log(c(0.75, 0.90, 0.80)),
  se = c(0.12, 0.15, 0.18)
)
nma_result <- network_meta(nma_data, effect_measure = "hr")
sucra <- calculate_sucra(nma_result)
sucra$ranking
#>   treatment mean_rank sucra prob_best prob_worst final_rank
#> B         B     1.425 78.75     0.599      0.024          1
#> C         C     1.759 62.05     0.391      0.150          2
#> A         A     2.816  9.20     0.010      0.826          3
sucra$interpretation
#> [1] "Treatment ranking by lower is better (SUCRA, %). Best: B (78.8%), Worst: A (9.2%)"
```
