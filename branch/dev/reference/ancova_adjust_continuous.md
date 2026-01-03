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

  Character. Reference group for contrast

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
are computed with confint.

## References

IQWiG Methods v8.0, Section 10.3.6, p. 218-220.
