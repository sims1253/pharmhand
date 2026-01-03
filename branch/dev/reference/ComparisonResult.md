# ComparisonResult Class

An S7 class for representing results of a single treatment comparison
(e.g., hazard ratio, risk difference, odds ratio).

## Usage

``` r
ComparisonResult(
  estimate = integer(0),
  ci = integer(0),
  ci_level = 0.95,
  p_value = NA_real_,
  method = "",
  n = NA_integer_,
  metadata = list(),
  effect_measure = "hr",
  treatment = "",
  control = ""
)
```

## Arguments

- estimate:

  Numeric effect estimate

- ci:

  Numeric vector c(lower, upper)

- ci_level:

  Numeric confidence level

- p_value:

  Numeric p-value

- method:

  Character string for statistical method

- n:

  Integer sample size

- metadata:

  List of additional metadata

- effect_measure:

  Character string: "hr", "or", "rr", "rd", "md", "smd"

- treatment:

  Character string for treatment arm name

- control:

  Character string for control arm name

## Value

A ComparisonResult object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ComparisonResult(
  estimate = 0.75,
  ci = c(0.60, 0.93),
  p_value = 0.008,
  effect_measure = "hr",
  treatment = "Drug A",
  control = "Placebo",
  method = "Cox proportional hazards"
)
} # }
```
