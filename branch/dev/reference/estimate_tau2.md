# Estimate Between-Study Variance (tau-squared)

Internal function to estimate tau2 using various methods. Extracted to
avoid duplication across meta_analysis() and calculate_heterogeneity().

## Usage

``` r
estimate_tau2(yi, sei, method, wi_fe, theta_fe, Q, df)
```

## Arguments

- yi:

  Numeric vector of effect estimates

- sei:

  Numeric vector of standard errors

- method:

  Character: "DL", "PM", or "REML"

- wi_fe:

  Numeric vector of fixed-effect weights (1/sei^2)

- theta_fe:

  Numeric fixed-effect estimate

- Q:

  Numeric Q statistic

- df:

  Integer degrees of freedom

## Value

Numeric tau2 estimate
