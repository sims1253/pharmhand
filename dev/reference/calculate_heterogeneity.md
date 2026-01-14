# Calculate Heterogeneity Statistics

Calculates Q, I2, tau2, and H2 statistics for meta-analysis
heterogeneity.

## Usage

``` r
calculate_heterogeneity(yi, sei, method = c("REML", "DL", "PM"))
```

## Arguments

- yi:

  Numeric vector of effect estimates

- sei:

  Numeric vector of standard errors

- method:

  Character. tau2 estimation method. Default: "REML"

## Value

A list with components:

- Q:

  Cochran's Q statistic

- Q_df:

  Degrees of freedom for Q test

- Q_pvalue:

  P-value for Q test

- I2:

  I-squared heterogeneity percentage (0-100)

- I2_ci:

  95 percent CI for I-squared

- H2:

  H-squared statistic

- H:

  H statistic (sqrt of H2)

- tau2:

  Between-study variance estimate

- tau:

  Between-study standard deviation

- method:

  Estimation method used

- k:

  Number of studies

- interpretation:

  Verbal interpretation of I2 level

## Examples

``` r
# Calculate heterogeneity for 5 studies
yi <- c(0.5, 0.8, 0.3, 1.0, 0.6)
sei <- c(0.2, 0.2, 0.2, 0.2, 0.2)
het <- calculate_heterogeneity(yi, sei, method = "REML")
het$Q
#> [1] 7.3
het$I2
#> [1] 45.20548
het$tau2
#> [1] 0.0184
het$interpretation
#> [1] "Moderate heterogeneity"
```
