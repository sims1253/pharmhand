# Sensitivity Analysis Across Risk of Bias Scenarios

Performs sensitivity analyses by re-running meta-analysis under
different RoB scenarios: including all studies, low-risk only, low+some
concerns, and excluding high-risk studies. This helps assess how RoB
affects pooled estimates.

## Usage

``` r
rob_sensitivity_analysis(
  meta_result,
  rob_results,
  method = "REML",
  conf_level = 0.95
)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis().

- rob_results:

  List of RoB2Result or ROBINSIResult objects.

- method:

  Character. Tau-squared estimation method: "DL", "REML", "PM". Default:
  "REML".

- conf_level:

  Numeric. Confidence level. Default: 0.95.

## Value

A list with components:

- results:

  Data frame with scenario, estimate, CI, I2, tau2, k

- scenarios:

  Character vector of scenario names

- original_estimate:

  Original pooled estimate (all studies)

- comparison:

  Comparison with original estimate

- effect_measure:

  Effect measure used

## Examples

``` r
if (FALSE) { # \dontrun{
# Create meta-analysis result
meta_res <- meta_analysis(
  yi = log(c(0.75, 0.82, 0.68, 0.91, 0.77)),
  sei = c(0.12, 0.15, 0.18, 0.14, 0.11),
  effect_measure = "hr"
)

# Create RoB 2 assessments
rob_results <- list(
  assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
  assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
  assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study 5", "Low", "Low", "Some concerns", "Low", "Low")
)

# Perform RoB sensitivity analysis
sensitivity <- rob_sensitivity_analysis(meta_res, rob_results)
sensitivity$results
} # }
```
