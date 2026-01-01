# Assess Baseline Balance Between Treatment Groups

Performs a comprehensive baseline balance assessment calculating SMD for
multiple continuous and categorical variables. Returns a
BalanceAssessment object that includes SMD results, imbalance flags,
summary statistics, and data formatted for Love plot visualization.

## Usage

``` r
assess_baseline_balance(
  data,
  trt_var,
  continuous_vars = character(),
  categorical_vars = character(),
  ref_group = NULL,
  threshold = 0.1,
  conf_level = 0.95,
  continuous_threshold = 10,
  continuous_method = c("cohens_d", "hedges_g"),
  categorical_method = c("arcsine", "logit", "raw")
)
```

## Arguments

- data:

  A data frame containing the baseline data.

- trt_var:

  Character. Name of the treatment variable.

- continuous_vars:

  Character vector. Names of continuous variables.

- categorical_vars:

  Character vector. Names of categorical variables.

- ref_group:

  Character or NULL. Reference (control) group value.

- threshold:

  Numeric. SMD threshold for imbalance (default: 0.1).

- conf_level:

  Numeric. Confidence level for CIs (default: 0.95).

- continuous_threshold:

  Integer. Minimum number of unique values to treat numeric variables as
  continuous (default: 10).

- continuous_method:

  Character. SMD method for continuous variables. One of "cohens_d"
  (default) or "hedges_g".

- categorical_method:

  Character. SMD method for categorical variables. One of "arcsine"
  (default), "logit", or "raw".

## Value

A BalanceAssessment S7 object with:

- `smd_results`: Data frame with SMD for each variable

- `imbalanced_vars`: Character vector of imbalanced variable names

- `threshold`: The threshold used

- `n_treatment`: Sample size in treatment group

- `n_control`: Sample size in control group

- `summary_stats`: List with summary statistics

- `love_plot_data`: Data frame formatted for Love plot

## Details

A threshold of 0.1 is commonly used in clinical trials to indicate
meaningful imbalance. Variables with \|SMD\| \> threshold are flagged.

The IQWiG (German HTA agency) recommends SMD assessment for all baseline
characteristics in benefit assessment dossiers.

## References

Austin, P. C. (2009). Balance diagnostics for comparing the distribution
of baseline covariates between treatment groups in propensity-score
matched samples. Statistics in Medicine, 28(25), 3083-3107.

## See also

[`create_love_plot()`](https://sims1253.github.io/pharmhand/dev/reference/create_love_plot.md),
[`add_smd_to_table()`](https://sims1253.github.io/pharmhand/dev/reference/add_smd_to_table.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assess baseline balance
balance <- assess_baseline_balance(
  data = adsl,
  trt_var = "TRT01P",
  continuous_vars = c("AGE", "WEIGHT", "HEIGHT", "BMI"),
  categorical_vars = c("SEX", "RACE", "ETHNIC"),
  ref_group = "Placebo",
  threshold = 0.1
)

# Check if balanced
balance@balanced

# View imbalanced variables
balance@imbalanced_vars

# Create Love plot
create_love_plot(balance)
} # }
```
