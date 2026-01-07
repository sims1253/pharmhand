# Duval & Tweedie Trim-and-Fill Publication Bias Adjustment Performs the Duval & Tweedie trim-and-fill method to estimate the number of missing studies and adjust the pooled effect for publication bias.

Duval & Tweedie Trim-and-Fill Publication Bias Adjustment

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

  Character. Side where excess studies are trimmed: "left", "right", or
  "auto". Missing studies are imputed on opposite side. Default: "auto"

- estimator:

  Character. Method to estimate missing studies: "L0", "R0", "Q0".
  Default: "L0"

- maxiter:

  Integer. Maximum iterations. Default: 100

## Value

A list with components:

- original:

  Original MetaResult object

- adjusted:

  Adjusted MetaResult with imputed studies

- n_imputed:

  Estimated number of missing studies

- side:

  Side where studies were imputed

- imputed_studies:

  Data frame with imputed effects

- estimator:

  Estimator method used

- summary:

  Text summary of the adjustment

## Examples

``` r
# Trim-and-fill for publication bias adjustment
yi <- c(-0.5, -0.4, -0.3, -0.1, 0.0, 0.5, 0.6)
sei <- c(0.1, 0.12, 0.15, 0.18, 0.2, 0.1, 0.12)
meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "md")
adjusted <- trim_and_fill(meta_res)
adjusted$n_imputed
#> [1] 0
adjusted$interpretation
#> [1] "No missing studies detected"
```
