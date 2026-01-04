# Trim-and-Fill Method for Publication Bias

Performs the Duval & Tweedie trim-and-fill method to estimate the number
of missing studies and adjust the pooled effect for publication bias.

## Usage

``` r
trim_and_fill(
  meta_result,
  side = c("auto", "left", "right"),
  estimator = c("L0", "R0", "Q0"),
  maxiter = 100
)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis()

- side:

  Character. Side to impute: "left", "right", or "auto". Default: "auto"

- estimator:

  Character. Method to estimate missing studies: "L0", "R0", "Q0".
  Default: "L0"

- maxiter:

  Integer. Maximum iterations. Default: 100

## Value

List with original and adjusted estimates, imputed studies, and
diagnostics
