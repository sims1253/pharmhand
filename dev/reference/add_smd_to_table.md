# Add SMD Column to Demographics/Baseline Table

Adds a standardized mean difference column to a baseline characteristics
table, flagging variables that exceed the imbalance threshold. This is
essential for GBA/AMNOG dossiers to demonstrate baseline comparability.

## Usage

``` r
add_smd_to_table(
  data,
  trt_var,
  vars,
  ref_group = NULL,
  threshold = 0.1,
  conf_level = 0.95,
  continuous_threshold = 10,
  flag_symbol = "*"
)
```

## Arguments

- data:

  A data frame containing the baseline data.

- trt_var:

  Character. Name of the treatment variable.

- vars:

  Character vector. Names of variables to calculate SMD for.

- ref_group:

  Character or NULL. Reference (control) group value. If NULL, uses the
  first level.

- threshold:

  Numeric. SMD threshold for flagging imbalance (default: 0.1). Common
  thresholds are 0.1 (strict) and 0.25 (lenient).

- conf_level:

  Numeric. Confidence level for CI (default: 0.95)

- continuous_threshold:

  Integer. Minimum number of unique values to treat numeric variables as
  continuous (default: 10).

- flag_symbol:

  Character. Symbol to use for flagging imbalanced variables (default:
  "\*")

## Value

A data frame with columns:

- `variable`: Variable name

- `smd`: Standardized mean difference

- `ci`: Formatted confidence interval

- `imbalanced`: Logical flag for \|SMD\| \> threshold

- `smd_display`: Formatted SMD with flag if imbalanced

## References

IQWiG (2020). General Methods: Version 6.0. Institute for Quality and
Efficiency in Health Care.

## See also

[`assess_baseline_balance()`](https://sims1253.github.io/pharmhand/dev/reference/assess_baseline_balance.md),
[`create_love_plot()`](https://sims1253.github.io/pharmhand/dev/reference/create_love_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
adsl <- data.frame(
  AGE = rnorm(200, 55, 12),
  WEIGHT = rnorm(200, 75, 15),
  SEX = sample(c("M", "F"), 200, replace = TRUE),
  TRT01P = rep(c("Treatment", "Placebo"), each = 100)
)

smd_table <- add_smd_to_table(
  data = adsl,
  trt_var = "TRT01P",
  vars = c("AGE", "WEIGHT", "SEX"),
  ref_group = "Placebo"
)
} # }
```
