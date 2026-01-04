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
if (FALSE) { # \dontrun{
# Drug A vs Placebo: HR = 0.75, SE(log) = 0.12
# Drug B vs Placebo: HR = 0.85, SE(log) = 0.10
# What is Drug A vs Drug B indirectly?
result <- indirect_comparison(
  effect_ab = log(0.75),  # A vs Placebo
  se_ab = 0.12,
  effect_bc = log(0.85),  # B vs Placebo
  se_bc = 0.10,
  effect_measure = "hr"
)
# Result: A vs B (through Placebo)
} # }
```
