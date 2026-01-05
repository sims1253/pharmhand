# Perform Leave-One-Out Sensitivity Analysis

Recalculates meta-analysis results leaving out each study one at a time
to assess the influence of individual studies.

## Usage

``` r
leave_one_out(meta_result = NULL, yi = NULL, sei = NULL)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis()

- yi:

  Numeric vector of effect estimates (optional if in meta_result)

- sei:

  Numeric vector of standard errors (optional if in meta_result)

## Value

A list with components:

- results:

  Data frame with estimates when each study is excluded

- influential_studies:

  Character vector of influential studies

- n_influential:

  Number of influential studies

- effect_measure:

  Effect measure used

- model:

  Model type (fixed/random)

## Examples

``` r
# Leave-one-out sensitivity analysis
yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)
meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
loo <- leave_one_out(meta_res)
loo$results
#>   excluded_study   estimate         se   ci_lower    ci_upper I2
#> 1        Study 1 -0.2255565 0.06917580 -0.3611386 -0.08997440  0
#> 2        Study 2 -0.2491445 0.06537572 -0.3772785 -0.12101041  0
#> 3        Study 3 -0.2230226 0.06355725 -0.3475925 -0.09845271  0
#> 4        Study 4 -0.2739760 0.06631426 -0.4039496 -0.14400245  0
#> 5        Study 5 -0.2324773 0.07146988 -0.3725557 -0.09239892  0
#>   estimate_display ci_lower_display ci_upper_display pct_change
#> 1        0.7980720        0.6968824        0.9139546   8.121456
#> 2        0.7794674        0.6857251        0.8860247  -1.486912
#> 3        0.8000967        0.7063866        0.9062386   9.153602
#> 4        0.7603503        0.6676778        0.8658856 -11.601835
#> 5        0.7925677        0.6889713        0.9117414   5.302315
loo$influential_studies
#> [1] "Study 4"
```
