# Calculate Standardized Mean Difference for Continuous Variables

Calculates the standardized mean difference (SMD) between two groups for
continuous variables using either Cohen's d or Hedges' g. SMD is a key
metric for assessing baseline balance in randomized controlled trials,
particularly important for GBA/AMNOG dossier submissions.

## Usage

``` r
calculate_smd(
  mean1,
  sd1,
  n1,
  mean2,
  sd2,
  n2,
  method = c("cohens_d", "hedges_g"),
  conf_level = 0.95
)
```

## Arguments

- mean1:

  Numeric. Mean of group 1 (treatment group).

- sd1:

  Numeric. Standard deviation of group 1. Must be positive.

- n1:

  Integer. Sample size of group 1. Must be \>= 2.

- mean2:

  Numeric. Mean of group 2 (control/reference group).

- sd2:

  Numeric. Standard deviation of group 2. Must be positive.

- n2:

  Integer. Sample size of group 2. Must be \>= 2.

- method:

  Character. Method for calculating SMD. One of:

  - `"cohens_d"` (default): Uses pooled standard deviation

  - `"hedges_g"`: Applies small-sample bias correction to Cohen's d

- conf_level:

  Numeric. Confidence level for CI calculation (default: 0.95).

## Value

A named list with components:

- `smd`: The standardized mean difference

- `ci_lower`: Lower bound of confidence interval

- `ci_upper`: Upper bound of confidence interval

- `method`: Method used ("cohens_d" or "hedges_g")

- `se`: Standard error of the SMD

## Details

**Cohen's d** is calculated as: \$\$d = \frac{\bar{x}\_1 -
\bar{x}\_2}{s\_{pooled}}\$\$

where \\s\_{pooled} = \sqrt{\frac{(n_1-1)s_1^2 +
(n_2-1)s_2^2}{n_1+n_2-2}}\\

**Hedges' g** applies a correction factor for small samples: \$\$g = d
\times \left(1 - \frac{3}{4(n_1+n_2)-9}\right)\$\$

The 95% confidence interval is calculated using the large-sample
variance: \$\$Var(d) = \frac{n_1+n_2}{n_1 n_2} +
\frac{d^2}{2(n_1+n_2)}\$\$

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

Hedges, L. V., & Olkin, I. (1985). Statistical Methods for
Meta-Analysis. Academic Press.

## See also

[`calculate_smd_binary()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd_binary.md)
for binary variables,
[`calculate_smd_from_data()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd_from_data.md)
for calculating directly from data

## Examples

``` r
# Cohen's d for age between treatment groups
calculate_smd(
  mean1 = 55.2, sd1 = 12.3, n1 = 150,
  mean2 = 53.8, sd2 = 11.9, n2 = 148
)
#> $smd
#> [1] 0.1156738
#> 
#> $ci_lower
#> [1] -0.1115965
#> 
#> $ci_upper
#> [1] 0.342944
#> 
#> $method
#> [1] "cohens_d"
#> 
#> $se
#> [1] 0.1159563
#> 

# Hedges' g with small sample correction
calculate_smd(
  mean1 = 55.2, sd1 = 12.3, n1 = 25,
  mean2 = 53.8, sd2 = 11.9, n2 = 23,
  method = "hedges_g"
)
#> $smd
#> [1] 0.1137085
#> 
#> $ci_lower
#> [1] -0.4530328
#> 
#> $ci_upper
#> [1] 0.6804498
#> 
#> $method
#> [1] "hedges_g"
#> 
#> $se
#> [1] 0.289159
#> 
```
