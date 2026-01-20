# RMSTResult Class

An S7 class for storing RMST analysis results.

## Usage

``` r
RMSTResult(
  rmst_by_group = data.frame(),
  rmst_difference = integer(0),
  se_difference = integer(0),
  ci = NULL,
  p_value = integer(0),
  tau = integer(0),
  treatment_comparison = data.frame(),
  n_obs = integer(0),
  n_events = integer(0),
  metadata = list()
)
```

## Arguments

- rmst_by_group:

  Data frame with RMST estimates by treatment group

- rmst_difference:

  Numeric. Difference in RMST between groups

- se_difference:

  Standard error of the difference

- ci:

  Numeric vector c(lower, upper) confidence interval for difference

- p_value:

  P-value for treatment difference test

- tau:

  Numeric. Time restriction used for RMST calculation

- treatment_comparison:

  Data frame with treatment comparison results

- n_obs:

  Number of observations

- n_events:

  Vector of event counts by group

- metadata:

  List of additional metadata

## Value

An RMSTResult object
