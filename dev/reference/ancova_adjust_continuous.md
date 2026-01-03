# ANCOVA Analysis for Continuous Endpoints

Performs ANCOVA to estimate treatment effects adjusted for baseline and
other covariates.

## Usage

``` r
ancova_adjust_continuous(
  data,
  outcome_var,
  trt_var,
  baseline_var,
  covariates = NULL,
  ref_group = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  Data frame with outcome and covariates

- outcome_var:

  Character. Post-baseline outcome variable

- trt_var:

  Character. Treatment variable

- baseline_var:

  Character. Baseline value variable

- covariates:

  Character vector. Additional covariates to adjust for

- ref_group:

  Character. Reference group for contrast. If NULL, the first level of
  the treatment variable is used as reference.

- conf_level:

  Numeric. Confidence level (default: 0.95)

## Value

List with:

- treatment_effects: Data frame with adjusted differences vs reference

- model: The fitted lm object

- summary: Model summary

- anova: ANOVA table

## Details

Fits model: outcome ~ baseline + trt + covariates Treatment effects are
extracted from model coefficients (trt - ref) and confidence intervals
are computed with confint. Missing values are handled by lm() via
listwise deletion. Specified variables must exist in the data.

## References

IQWiG Methods v8.0, Section 10.3.6, p. 218-220.

## Examples

``` r
# Simulated example
set.seed(123)
n <- 100
sim_data <- data.frame(
  TRT01P = rep(c("Placebo", "Drug A"), each = n/2),
  BASE = rnorm(n, 50, 10),
  CHG = rnorm(n, 5, 8),
  AGEGR1 = sample(c("<65", ">=65"), n, replace = TRUE),
  SEX = sample(c("F", "M"), n, replace = TRUE)
)

result <- ancova_adjust_continuous(
  data = sim_data,
  outcome_var = "CHG",
  trt_var = "TRT01P",
  baseline_var = "BASE",
  ref_group = "Placebo",
  covariates = c("AGEGR1", "SEX")
)
print(result$treatment_effects)
#>   Treatment estimate   ci_lower ci_upper
#> 1    Drug A 2.312759 -0.8017789 5.427296
```
