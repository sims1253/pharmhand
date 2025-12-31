# Calculate SMD Directly from Data

Calculates the standardized mean difference for a variable directly from
a data frame, automatically detecting whether the variable is continuous
or categorical.

## Usage

``` r
calculate_smd_from_data(
  data,
  var,
  trt_var,
  ref_group = NULL,
  method = c("auto", "cohens_d", "hedges_g", "arcsine", "logit", "raw"),
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing the analysis data.

- var:

  Character. Name of the variable to calculate SMD for.

- trt_var:

  Character. Name of the treatment group variable.

- ref_group:

  Character or NULL. Value of the reference (control) group. If NULL,
  uses the first level of the treatment variable.

- method:

  Character. Method for SMD calculation:

  - `"cohens_d"`: Cohen's d for continuous variables

  - `"hedges_g"`: Hedges' g (bias-corrected) for continuous variables

  - `"arcsine"`: Arcsine transformation for binary/categorical

  - `"auto"` (default): Automatically selects based on variable type

- conf_level:

  Numeric. Confidence level for CI (default: 0.95)

## Value

A named list with components:

- `smd`: The standardized mean difference (absolute value for
  multi-level)

- `ci_lower`: Lower bound of confidence interval

- `ci_upper`: Upper bound of confidence interval

- `method`: Method used

- `var_type`: Detected variable type ("continuous" or "categorical")

- `se`: Standard error of the SMD

## Details

When `method = "auto"`:

- Numeric variables with \> 10 unique values are treated as continuous
  (using Cohen's d)

- Numeric variables with \<= 10 unique values are treated as categorical

- Character/factor variables are treated as categorical (using arcsine)

For categorical variables with more than 2 levels, the function
calculates the maximum absolute SMD across all pairwise level
comparisons.

## See also

[`calculate_smd()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd.md),
[`calculate_smd_binary()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd_binary.md),
[`assess_baseline_balance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_baseline_balance.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create example data
adsl <- data.frame(
  AGE = c(rnorm(100, 55, 12), rnorm(100, 54, 11)),
  SEX = c(sample(c("M", "F"), 100, replace = TRUE, prob = c(0.4, 0.6)),
          sample(c("M", "F"), 100, replace = TRUE, prob = c(0.45, 0.55))),
  TRT01P = rep(c("Treatment", "Placebo"), each = 100)
)

# Continuous variable
calculate_smd_from_data(adsl, "AGE", "TRT01P", ref_group = "Placebo")

# Categorical variable
calculate_smd_from_data(adsl, "SEX", "TRT01P", ref_group = "Placebo")
} # }
```
