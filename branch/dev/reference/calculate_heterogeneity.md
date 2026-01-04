# Calculate Heterogeneity Statistics

Calculates Q, I2, tau2, and H2 statistics for meta-analysis
heterogeneity.

## Usage

``` r
calculate_heterogeneity(yi, sei, method = c("REML", "DL", "PM", "ML"))
```

## Arguments

- yi:

  Numeric vector of effect estimates

- sei:

  Numeric vector of standard errors

- method:

  Character. tau2 estimation method. Default: "REML"

## Value

List with Q, I2, tau2, H2, and related statistics
