# Perform Leave-One-Out Sensitivity Analysis

Recalculates meta-analysis results leaving out each study one at a time
to assess the influence of individual studies.

## Usage

``` r
leave_one_out(meta_result = NULL, yi = NULL, sei = NULL, method = NULL)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis()

- yi:

  Numeric vector of effect estimates (optional if in meta_result)

- sei:

  Numeric vector of standard errors (optional if in meta_result)

- method:

  Character. Tau-squared estimation method for the leave-one-out refits:
  "DL", "REML", or "PM". If `NULL` (default), uses
  `meta_result@metadata$method` when available; otherwise defaults to
  "DL".

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

- method:

  Tau-squared estimation method used

## Examples

``` r
# Leave-one-out sensitivity analysis
yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)
meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
loo <- leave_one_out(meta_res)
loo$results
#>   excluded_study   estimate       se  ci_lower ci_upper I2 estimate_display
#> 1        Study 1 -0.2349472 249.4553 -489.1584 488.6885  0        0.7906126
#> 2        Study 2 -0.2572550 231.2617 -453.5218 453.0073  0        0.7731710
#> 3        Study 3 -0.2104521 221.7938 -434.9184 434.4975  0        0.8102179
#> 4        Study 4 -0.2832901 197.4333 -387.2455 386.6790  0        0.7533013
#> 5        Study 5 -0.2415265 227.5301 -446.1924 445.7094  0        0.7854280
#>   ci_lower_display ci_upper_display pct_change
#> 1    3.640832e-213    1.716828e+212   4.296221
#> 2    1.091392e-197    5.477348e+196  -4.790668
#> 3    1.310196e-189    5.010342e+188  14.274094
#> 4    6.628305e-169    8.561205e+167 -15.395834
#> 5    1.663786e-194    3.707792e+193   1.616187
loo$influential_studies
#> [1] "Study 3" "Study 4"
```
