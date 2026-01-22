# BayesianMetaFewResult Class

An S7 class for storing Bayesian meta-analysis results for few studies.

## Usage

``` r
BayesianMetaFewResult(
  posterior_summary = data.frame(),
  credible_intervals = data.frame(),
  prior_summary = data.frame(),
  few_studies_adjustment = "",
  prob_positive = 0.5,
  prob_negative = 0.5,
  heterogeneity = list(),
  tau_summary = data.frame(),
  prior_sensitivity = list(),
  model = NULL,
  n_studies = integer(0),
  effect_measure = "hr",
  metadata = list()
)
```

## Arguments

- posterior_summary:

  Data frame with posterior summary statistics

- credible_intervals:

  Data frame with credible intervals

- prior_summary:

  Data frame with prior specification summary

- few_studies_adjustment:

  Character. Description of adjustments made for few studies

- prob_positive:

  Numeric. Posterior probability that effect is positive

- prob_negative:

  Numeric. Posterior probability that effect is negative

- heterogeneity:

  List with heterogeneity statistics (tau2, I2, etc.)

- tau_summary:

  Data frame with between-study variance summary

- prior_sensitivity:

  List with prior sensitivity analysis results

- model:

  The brms model object

- n_studies:

  Integer number of studies

- effect_measure:

  Character string for effect measure type

- metadata:

  List of additional metadata

## Value

A BayesianMetaFewResult object
