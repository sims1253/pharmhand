# Create IQWiG-Compliant Forest Plot for Bayesian Meta-Analysis

Generates a forest plot formatted according to IQWiG guidelines with:

- Study weights displayed

- Pooled effect with credible interval

- Heterogeneity statistics

- Prediction interval (if applicable)

- Proper scaling and formatting

## Usage

``` r
create_bayesian_forest_plot_iqwig(
  bayesian_result,
  study_data = NULL,
  show_weights = TRUE,
  show_prediction_interval = TRUE,
  digits_estimate = 3L,
  locale = get_locale(),
  title = NULL,
  subtitle = NULL,
  null_value = NULL,
  base_size = 11,
  point_size = 2,
  ci_linewidth = 0.6
)
```

## Arguments

- bayesian_result:

  A bayesian_meta_result object from bayesian_meta_analysis()

- study_data:

  Data frame with study data containing yi, sei, and study_labels
  columns. If NULL, uses metadata from bayesian_result if available.

- show_weights:

  Logical. Display study weights (default: TRUE)

- show_prediction_interval:

  Logical. Show prediction interval (default: TRUE)

- digits_estimate:

  Integer. Decimal places for estimates (default: 3)

- locale:

  Character. Locale for formatting: "en" or "de"

- title:

  Character. Plot title (default: NULL)

- subtitle:

  Character. Plot subtitle with heterogeneity info (default:
  auto-generated)

- null_value:

  Numeric. Reference line value (default: 1 for ratios, 0 for
  differences)

- base_size:

  Numeric. Base font size (default: 11)

- point_size:

  Numeric. Size of study point estimates (default: 2)

- ci_linewidth:

  Numeric. Line width for CI lines (default: 0.6)

## Value

A ClinicalPlot object containing the forest plot

## Examples

``` r
if (FALSE) { # \dontrun{
result <- bayesian_meta_analysis(yi = yi, sei = sei, study_labels = labels)
study_df <- data.frame(yi = yi, sei = sei, study_labels = labels)
plot <- create_bayesian_forest_plot_iqwig(result, study_data = study_df)
print(plot)
} # }
```
