# Calculates ranking probabilities and SUCRA (Surface Under Cumulative Ranking curve) or P-scores for treatments in network meta-analysis.

Calculates ranking probabilities and SUCRA (Surface Under Cumulative
Ranking curve) or P-scores for treatments in network meta-analysis.

## Usage

``` r
calculate_sucra(nma_result, lower_better = NULL, n_sim = 1000, seed = 42)
```

## Arguments

- nma_result:

  Result from network_meta()

- lower_better:

  Logical. Is lower estimate better? Default: TRUE for ratios

- n_sim:

  Integer. Number of simulations for ranking. Default: 1000

- seed:

  Integer or NULL. Random seed for reproducibility. Default: 42. Set to
  NULL for non-deterministic results.

## Value

List with rankings, SUCRA/P-scores, and rankogram data

## Note

RNG state is isolated via
[`withr::with_seed`](https://withr.r-lib.org/reference/with_seed.html)
during simulation, so calling this function does not alter the global
RNG state.

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
#> B         B     1.432 78.40     0.585      0.017          1
#> C         C     1.717 64.15     0.413      0.130          2
#> A         A     2.851  7.45     0.002      0.853          3
sucra$interpretation
#> [1] "Treatment ranking by lower is better (SUCRA, %). Best: B (78.4%), Worst: A (7.4%)"
```
