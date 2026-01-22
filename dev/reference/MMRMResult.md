# MMRMResult Class

An S7 class for storing MMRM analysis results.

## Usage

``` r
MMRMResult(
  model = NULL,
  coefficients = integer(0),
  se = integer(0),
  ci = NULL,
  p_values = integer(0),
  df = integer(0),
  sigma = integer(0),
  log_likelihood = integer(0),
  aic = integer(0),
  bic = integer(0),
  covariance = "unstructured",
  df_adjustment = "Kenward-Roger",
  n_obs = integer(0),
  n_subjects = integer(0),
  metadata = list()
)
```

## Arguments

- model:

  The mmrm model object

- coefficients:

  Named numeric vector of model coefficients

- se:

  Standard errors for coefficients

- ci:

  Matrix of confidence intervals (2 columns: lower, upper)

- p_values:

  P-values for coefficients

- df:

  Degrees of freedom for coefficients

- sigma:

  Residual standard error

- log_likelihood:

  Log-likelihood value

- aic:

  Akaike Information Criterion

- bic:

  Bayesian Information Criterion

- covariance:

  Covariance structure used

- df_adjustment:

  Degrees of freedom adjustment method

- n_obs:

  Number of observations

- n_subjects:

  Number of subjects

- metadata:

  List of additional metadata

## Value

An MMRMResult object
