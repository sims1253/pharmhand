# Bias-Adjusted Meta-Analysis

Performs meta-analysis with adjustment for risk of bias using one of
three methods: weight downgrade, exclusion of high-risk studies, or
selection model approach.

## Usage

``` r
bias_adjusted_meta(
  meta_result,
  rob_results,
  method = c("weight_downgrade", "exclude_high", "selection_model"),
  weight_high = 0,
  weight_concerns = 0.5,
  weight_moderate = 0.75,
  weight_serious = 0.25,
  exclude_high = TRUE,
  selection_alpha = 0.05,
  conf_level = 0.95,
  ...
)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis().

- rob_results:

  List of RoB2Result or ROBINSIResult objects.

- method:

  Character. Adjustment method: "weight_downgrade", "exclude_high", or
  "selection_model". Default: "weight_downgrade".

- weight_high:

  Numeric. Weight for high-risk studies (weight_downgrade). Default: 0.

- weight_concerns:

  Numeric. Weight for "some concerns" (RoB 2) or "Moderate" (ROBINS-I)
  studies. Default: 0.5.

- weight_moderate:

  Numeric. Weight for "Moderate" risk (ROBINS-I). Default: 0.75.

- weight_serious:

  Numeric. Weight for "Serious" risk (ROBINS-I). Default: 0.25.

- exclude_high:

  Logical. If TRUE, exclude high-risk studies (used by "exclude_high"
  method). Default: TRUE.

- selection_alpha:

  Numeric. Significance level alpha for selection model (between 0 and
  1). Default: 0.05.

- conf_level:

  Numeric. Confidence level. Default: 0.95.

- ...:

  Additional arguments passed to internal methods.

## Value

A BiasAdjustedMetaResult object (extends MetaResult) with:

- estimate:

  Adjusted pooled effect estimate

- ci:

  Confidence interval

- p_value:

  P-value

- n:

  Number of studies included

- method:

  Method description

- heterogeneity:

  Heterogeneity statistics

- adjustment_details:

  List with adjustment metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Create meta-analysis result
meta_res <- meta_analysis(
  yi = log(c(0.75, 0.82, 0.68, 0.91, 0.77)),
  sei = c(0.12, 0.15, 0.18, 0.14, 0.11),
  study_labels = paste("Study", 1:5),
  effect_measure = "hr"
)

# Create RoB 2 assessments
rob_results <- list(
  assess_rob2("Study 1", "Low", "Low", "Low", "Some concerns", "Low"),
  assess_rob2("Study 2", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study 3", "High", "Low", "Low", "Low", "Low"),
  assess_rob2("Study 4", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study 5", "Low", "Low", "Low", "Some concerns", "Low")
)

# Weight downgrade method
adjusted <- bias_adjusted_meta(
  meta_res,
  rob_results,
  method = "weight_downgrade",
  weight_high = 0,
  weight_concerns = 0.5
)
adjusted@estimate
adjusted@ci

# Exclude high risk method
adjusted2 <- bias_adjusted_meta(
  meta_res,
  rob_results,
  method = "exclude_high"
)
adjusted2@estimate
adjusted2@ci
} # }
```
