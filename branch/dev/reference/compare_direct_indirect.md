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

## Examples

``` r
# Compare direct and indirect evidence
# Direct evidence: A vs B from head-to-head trial
direct <- list(estimate = log(0.80), se = 0.15)

# Indirect evidence via Bucher method
indirect <- indirect_comparison(
  effect_ab = log(0.75), se_ab = 0.12,
  effect_bc = log(0.85), se_bc = 0.10,
  effect_measure = "hr"
)

comparison <- compare_direct_indirect(
  direct_result = direct,
  indirect_result = indirect,
  effect_measure = "hr"
)
comparison$is_consistent
#> NULL
```
