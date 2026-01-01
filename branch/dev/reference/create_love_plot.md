# Create Love Plot for SMD Visualization

Creates a Love plot (also known as a covariate balance plot) showing
standardized mean differences for baseline variables with threshold
reference lines. This visualization is commonly used in HTA dossiers and
propensity score analysis to assess covariate balance.

## Usage

``` r
create_love_plot(
  balance_assessment,
  threshold = 0.1,
  show_ci = TRUE,
  title = "Standardized Mean Differences",
  xlab = "Standardized Mean Difference",
  color_by_type = TRUE,
  sort_by = c("abs_smd", "smd", "name", "none"),
  colors = NULL,
  point_size = 3,
  base_size = 11
)
```

## Arguments

- balance_assessment:

  A BalanceAssessment object from
  [`assess_baseline_balance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_baseline_balance.md),
  or a data frame with columns `variable`, `smd`, and optionally
  `ci_lower`, `ci_upper`, `var_type`.

- threshold:

  Numeric. SMD threshold for reference lines (default: 0.1). Vertical
  lines are drawn at +/- threshold. When `balance_assessment` is a
  `BalanceAssessment` object, its stored threshold is used regardless of
  this parameter.

- show_ci:

  Logical. Show confidence intervals as error bars (default: TRUE).

- title:

  Character. Plot title (default: "Standardized Mean Differences").

- xlab:

  Character. X-axis label (default: "Standardized Mean Difference").

- color_by_type:

  Logical. Color points by variable type (continuous vs categorical)
  when available (default: TRUE).

- sort_by:

  Character. How to sort variables: "abs_smd" (default), "smd", "name",
  or "none".

- colors:

  Named character vector of colors for "continuous", "categorical", and
  "threshold" elements. NULL uses defaults.

- point_size:

  Numeric. Size of points (default: 3).

- base_size:

  Numeric. Base font size (default: 11).

## Value

A ClinicalPlot object containing a ggplot2 Love plot.

## Details

The Love plot displays each baseline covariate on the y-axis with its
SMD on the x-axis. Vertical reference lines at +/- threshold help
identify imbalanced covariates.

Points falling outside the threshold lines indicate potential baseline
imbalance that may require adjustment in the analysis.

## References

Love, T. E. (2004). Demonstrating balance between groups in
observational studies. Presentation at the Cleveland Chapter of the
American Statistical Association.

## See also

[`assess_baseline_balance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_baseline_balance.md),
[`add_smd_to_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/add_smd_to_table.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# From BalanceAssessment object
balance <- assess_baseline_balance(
  data = adsl,
  trt_var = "TRT01P",
  continuous_vars = c("AGE", "WEIGHT"),
  categorical_vars = c("SEX", "RACE")
)
love_plot <- create_love_plot(balance)

# From data frame
smd_data <- data.frame(
  variable = c("Age", "Sex", "Weight"),
  smd = c(0.05, -0.12, 0.08),
  ci_lower = c(-0.02, -0.20, 0.01),
  ci_upper = c(0.12, -0.04, 0.15)
)
create_love_plot(smd_data, threshold = 0.1)
} # }
```
