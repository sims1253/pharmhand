# Calculate Standardized Mean Difference for Binary Variables

Calculates the standardized mean difference (SMD) between two groups for
binary or categorical variables using the arcsine transformation or
logit method. This is essential for assessing baseline balance of
categorical covariates in clinical trials.

## Usage

``` r
calculate_smd_binary(
  p1,
  n1,
  p2,
  n2,
  method = c("arcsine", "logit", "raw"),
  conf_level = 0.95
)
```

## Arguments

- p1:

  Numeric. Proportion in group 1 (treatment). Must be between 0 and 1.

- n1:

  Integer. Sample size of group 1. Must be \>= 2.

- p2:

  Numeric. Proportion in group 2 (control). Must be between 0 and 1.

- n2:

  Integer. Sample size of group 2. Must be \>= 2.

- method:

  Character. Method for calculating SMD. One of:

  - `"arcsine"` (default): Uses arcsine square root transformation

  - `"logit"`: Uses logit transformation (log odds)

  - `"raw"`: Uses raw proportion difference standardized by pooled
    variance

- conf_level:

  Numeric. Confidence level for CI calculation (default: 0.95)

## Value

A named list with components:

- `smd`: The standardized mean difference

- `ci_lower`: Lower bound of confidence interval

- `ci_upper`: Upper bound of confidence interval

- `method`: Method used

- `se`: Standard error of the SMD

## Details

**Arcsine method** (recommended for proportions): \$\$SMD = 2 \times
(\arcsin(\sqrt{p_1}) - \arcsin(\sqrt{p_2}))\$\$

This transformation stabilizes variance across the range of proportions
and is bounded, making it suitable for proportions near 0 or 1.

**Logit method** (log odds ratio): Calculates the log odds ratio and
standardizes: \$\$SMD = \frac{\log(OR)}{\pi/\sqrt{3}}\$\$

where \\OR = \frac{p_1/(1-p_1)}{p_2/(1-p_2)}\\

**Raw method**: \$\$SMD = \frac{p_1 - p_2}{\sqrt{p(1-p)}}\$\$

where \\p = \frac{n_1 p_1 + n_2 p_2}{n_1 + n_2}\\

## References

Austin, P. C. (2009). Balance diagnostics for comparing the distribution
of baseline covariates between treatment groups in propensity-score
matched samples. Statistics in Medicine, 28(25), 3083-3107.

## See also

[`calculate_smd()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd.md)
for continuous variables,
[`calculate_smd_from_data()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd_from_data.md)
for calculating directly from data

## Examples

``` r
# Compare sex distribution (60% female in treatment, 55% in control)
calculate_smd_binary(p1 = 0.60, n1 = 150, p2 = 0.55, n2 = 148)
#> $smd
#> [1] 0.1011905
#> 
#> $ci_lower
#> [1] -0.1258899
#> 
#> $ci_upper
#> [1] 0.3282709
#> 
#> $method
#> [1] "arcsine"
#> 
#> $se
#> [1] 0.1158595
#> 

# Using logit transformation
calculate_smd_binary(
  p1 = 0.60, n1 = 150,
  p2 = 0.55, n2 = 148,
  method = "logit"
)
#> $smd
#> [1] 0.1129091
#> 
#> $ci_lower
#> [1] -0.1406897
#> 
#> $ci_upper
#> [1] 0.3665079
#> 
#> $method
#> [1] "logit"
#> 
#> $se
#> [1] 0.1293895
#> 
```
