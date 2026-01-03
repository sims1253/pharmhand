# StatResult Class (Abstract Base)

Abstract base class for statistical analysis results. Provides common
properties for effect estimates, confidence intervals, and p-values.

## Usage

``` r
StatResult(
  estimate = integer(0),
  ci = integer(0),
  ci_level = 0.95,
  p_value = NA_real_,
  method = "",
  n = NA_integer_,
  metadata = list()
)
```

## Arguments

- estimate:

  Numeric effect estimate

- ci:

  Numeric vector of length 2: c(lower, upper) confidence interval bounds

- ci_level:

  Numeric confidence level (default: 0.95)

- p_value:

  Numeric p-value (can be NA)

- method:

  Character string describing the statistical method used

- n:

  Integer sample size or number of studies

- metadata:

  List of additional metadata

## Value

A StatResult object
