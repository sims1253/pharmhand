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

A list with components:

- direct_estimate:

  Direct evidence effect estimate

- indirect_estimate:

  Indirect evidence effect estimate

- pooled_estimate:

  Inverse-variance weighted pooled estimate

- pooled_ci:

  CI for pooled estimate

- inconsistency_p:

  P-value for inconsistency test

- effect_measure:

  Effect measure used

- is_consistent:

  Logical; TRUE if p \> 0.05
