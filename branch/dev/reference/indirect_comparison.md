# Perform Indirect Comparison Using Bucher Method

Calculates indirect treatment effect comparing A vs C through common
comparator B using the Bucher method (anchored indirect comparison).

## Usage

``` r
indirect_comparison(
  effect_ab,
  se_ab,
  effect_bc,
  se_bc,
  effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
  conf_level = 0.95,
  label_a = "A",
  label_b = "B",
  label_c = "C"
)
```

## Arguments

- effect_ab:

  Effect estimate for A vs B comparison

- se_ab:

  Standard error for A vs B

- effect_bc:

  Effect estimate for B vs C comparison

- se_bc:

  Standard error for B vs C

- effect_measure:

  Character. Effect type: "hr", "or", "rr", "rd", "md", "smd"

- conf_level:

  Numeric. Confidence level. Default: 0.95

- label_a:

  Character. Label for treatment A. Default: "A"

- label_b:

  Character. Label for treatment B (comparator). Default: "B"

- label_c:

  Character. Label for treatment C. Default: "C"

## Value

ComparisonResult object with indirect effect estimate A vs C

## Examples

``` r
# Bucher indirect comparison: Drug A vs Drug B via Placebo
# Drug A vs Placebo: HR = 0.75
# Drug B vs Placebo: HR = 0.85
result <- indirect_comparison(
  effect_ab = log(0.75),
  se_ab = 0.12,
  effect_bc = log(0.85),
  se_bc = 0.10,
  effect_measure = "hr",
  label_a = "Drug A",
  label_b = "Placebo",
  label_c = "Drug B"
)
result@estimate
#> [1] 0.8823529
result@ci
#> [1] 0.6496514 1.1984068
```
