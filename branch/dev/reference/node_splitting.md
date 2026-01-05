# Separates direct and indirect evidence for each comparison and tests for inconsistency between them.

Separates direct and indirect evidence for each comparison and tests for
inconsistency between them.

## Usage

``` r
node_splitting(nma_result, data = NULL, conf_level = 0.95)
```

## Arguments

- nma_result:

  Result from network_meta()

- data:

  Original NMA data frame

- conf_level:

  Numeric. Confidence level. Default: 0.95

## Value

Data frame with direct, indirect, and inconsistency test results

## Note

This is a simplified implementation. Full node-splitting requires
re-running the network meta-analysis excluding direct evidence for each
comparison, which is computationally intensive. Consider using
specialized NMA packages (e.g., gemtc, netmeta) for rigorous
inconsistency assessment.

## Examples

``` r
# Node-splitting for inconsistency testing
nma_data <- data.frame(
  study = c("S1", "S2", "S3"),
  treat1 = c("A", "B", "A"),
  treat2 = c("B", "C", "C"),
  effect = log(c(0.75, 0.90, 0.80)),
  se = c(0.12, 0.15, 0.18)
)
nma_result <- network_meta(nma_data, effect_measure = "hr")
ns_result <- node_splitting(nma_result)
ns_result$note
#> [1] "Full node-splitting requires re-analysis excluding direct evidence. Results shown are simplified."
```
