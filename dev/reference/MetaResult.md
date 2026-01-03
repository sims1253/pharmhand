# MetaResult Class

An S7 class for representing meta-analysis results including pooled
estimates, heterogeneity statistics, and study weights.

## Usage

``` r
MetaResult(
  estimate = integer(0),
  ci = integer(0),
  ci_level = 0.95,
  p_value = NA_real_,
  method = "",
  n = NA_integer_,
  metadata = list(),
  model = "random",
  effect_measure = "hr",
  heterogeneity = list(Q = NA_real_, I2 = NA_real_, tau2 = NA_real_, H2 = NA_real_),
  weights = NULL,
  prediction_interval = NULL,
  study_results = list()
)
```

## Arguments

- estimate:

  Numeric pooled effect estimate

- ci:

  Numeric vector c(lower, upper)

- ci_level:

  Numeric confidence level

- p_value:

  Numeric p-value

- method:

  Character string for statistical method

- n:

  Integer number of studies

- metadata:

  List of additional metadata

- model:

  Character string: "fixed" or "random"

- effect_measure:

  Character string: "hr", "or", "rr", "rd", "md", "smd"

- heterogeneity:

  List with Q, I2, tau2, H2 statistics

- weights:

  Numeric vector of study weights

- prediction_interval:

  Numeric vector c(lower, upper) for prediction interval

- study_results:

  List of individual study ComparisonResult objects

## Value

A MetaResult object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- MetaResult(
  estimate = 0.80,
  ci = c(0.70, 0.91),
  p_value = 0.001,
  model = "random",
  effect_measure = "hr",
  heterogeneity = list(Q = 15.2, I2 = 0.45, tau2 = 0.02),
  n = 5L,
  method = "REML with Knapp-Hartung"
)
} # }
```
