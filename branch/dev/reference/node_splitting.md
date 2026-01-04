# Test for Inconsistency Using Node-Splitting

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
