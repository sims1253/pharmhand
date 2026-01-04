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

A list with components:

- original:

  Original MetaResult object

- adjusted:

  Adjusted MetaResult with imputed studies

- n_missing:

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
if (FALSE) { # \dontrun{
# Run meta-analysis
meta_res <- meta_analysis(
  yi = c(0.2, 0.4, 0.3, 0.5, 0.6),
  sei = c(0.1, 0.12, 0.08, 0.15, 0.11),
  study_labels = paste("Study", 1:5)
)

# Apply trim-and-fill for publication bias
adjusted <- trim_and_fill(meta_res)

# Check number of imputed studies
adjusted$n_missing
} # }
```
