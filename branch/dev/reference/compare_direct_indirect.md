# Compare Direct and Indirect Evidence

Compares direct evidence (from head-to-head trials) with indirect
evidence (from anchored comparison) to assess consistency.

## Usage

``` r
compare_direct_indirect(
  direct_result,
  indirect_result,
  effect_measure = NULL,
  conf_level = 0.95
)
```

## Arguments

- direct_result:

  ComparisonResult or list with estimate and se for direct evidence

- indirect_result:

  ComparisonResult from indirect_comparison()

- effect_measure:

  Character. Effect type (if not in results)

- conf_level:

  Numeric. Confidence level. Default: 0.95

## Value

List with test for inconsistency and pooled estimate
