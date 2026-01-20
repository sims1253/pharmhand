# Pool Results Using Rubin's Rules

Combines estimates and variances from multiple imputed datasets using
Rubin's rules.

## Usage

``` r
pool_rubin(estimates, variances, conf_level = 0.95)
```

## Arguments

- estimates:

  Numeric vector of point estimates from each imputed dataset

- variances:

  Numeric vector of variance estimates from each imputed dataset

- conf_level:

  Numeric. Confidence level for interval (default: 0.95)

## Value

A list containing:

- pooled_estimate:

  Pooled point estimate (mean of estimates)

- pooled_se:

  Pooled standard error

- ci:

  Confidence interval (lower, upper)

- within_var:

  Within-imputation variance

- between_var:

  Between-imputation variance

- total_var:

  Total variance

- fmi:

  Fraction of missing information

- df:

  Degrees of freedom for t-distribution

- t_statistic:

  t-statistic for hypothesis test

- p_value:

  Two-sided p-value

## Details

Rubin's rules combine estimates from m imputed datasets:

- Pooled estimate: Q_bar = mean(Q_i)

- Within-imputation variance: U_bar = mean(U_i)

- Between-imputation variance: B = var(Q_i)

- Total variance: T = U_bar + (1 + 1/m) \* B

The fraction of missing information (FMI) indicates the proportion of
total variance attributable to missing data.

## References

Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New
York: John Wiley & Sons.

## Examples

``` r
# Pool estimates from 5 imputed datasets
estimates <- c(0.52, 0.48, 0.55, 0.50, 0.53)
variances <- c(0.01, 0.012, 0.009, 0.011, 0.010)

pooled <- pool_rubin(estimates, variances)
pooled$pooled_estimate
#> [1] 0.516
pooled$ci
#> [1] 0.3074936 0.7245064
pooled$fmi
#> [1] 0.3851247
```
