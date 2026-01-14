# Egger's Test for Funnel Plot Asymmetry

Performs Egger's linear regression test to assess funnel plot asymmetry,
which may indicate publication bias.

## Usage

``` r
eggers_test(yi = NULL, sei = NULL, meta_result = NULL)
```

## Arguments

- yi:

  Numeric vector of effect estimates

- sei:

  Numeric vector of standard errors

- meta_result:

  A MetaResult object (alternative to yi/sei)

## Value

List with intercept, slope, standard error, t-value, p-value, df, and
interpretation

## Examples

``` r
# Egger's test for funnel plot asymmetry
yi <- c(0.5, 0.6, 0.4, 0.3, 0.8, 1.0)
sei <- c(0.2, 0.15, 0.25, 0.18, 0.12, 0.08)
result <- eggers_test(yi = yi, sei = sei)
result$p_value
#> [1] 0.005798801
result$interpretation
#> [1] "Significant asymmetry detected (p < 0.05), suggesting potential publication bias"
```
