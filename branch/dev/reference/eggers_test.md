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

List with intercept, slope, standard error, t-value, p-value, and
interpretation
