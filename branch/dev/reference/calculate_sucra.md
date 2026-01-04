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
