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
  heterogeneity = list(Q = NA_real_, Q_df = NA_integer_, Q_pvalue = NA_real_, I2 =
    NA_real_, H2 = NA_real_, tau2 = NA_real_, tau = NA_real_),
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

  List with Q, Q_df, Q_pvalue, I2, H2, tau2, tau statistics

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
result <- MetaResult(
  estimate = 0.80,
  ci = c(0.70, 0.91),
  ci_level = 0.95,
  p_value = 0.001,
  n = 5L,
  model = "random",
  effect_measure = "hr",
  heterogeneity = list(
    Q = 15.2, Q_df = 4L, Q_pvalue = 0.004,
    I2 = 73.7, H2 = 3.8, tau2 = 0.025, tau = 0.158
  ),
  method = "REML with Knapp-Hartung adjustment"
)
result@estimate
#> [1] 0.8
result@heterogeneity$I2
#> [1] 73.7
```
