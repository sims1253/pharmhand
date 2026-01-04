# Create Forest Plot for Meta-Analysis

Creates a forest plot displaying individual study effects and pooled
estimate from a meta-analysis.

## Usage

``` r
create_meta_forest_plot(
  meta_result,
  title = NULL,
  xlab = NULL,
  show_weights = TRUE,
  show_heterogeneity = TRUE,
  show_prediction = TRUE,
  null_value = NULL,
  xlim = NULL,
  palette = NULL,
  base_size = 11
)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis()

- title:

  Character. Plot title. Default: NULL

- xlab:

  Character. X-axis label. Default: based on effect_measure

- show_weights:

  Logical. Show study weights. Default: TRUE

- show_heterogeneity:

  Logical. Show heterogeneity stats. Default: TRUE

- show_prediction:

  Logical. Show prediction interval. Default: TRUE

- null_value:

  Numeric. Reference line value. Default: 1 for ratios, 0 for
  differences

- xlim:

  Numeric vector. X-axis limits. Default: NULL (auto)

- palette:

  Character vector. Colors. Default: NULL

- base_size:

  Numeric. Base font size. Default: 11

## Value

A ClinicalPlot object containing the forest plot

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample meta-analysis data
studies <- data.frame(
  study = c("Study A", "Study B", "Study C", "Study D"),
  yi = c(0.3, 0.5, 0.2, 0.4),
  sei = c(0.1, 0.15, 0.12, 0.11)
)

# Run meta-analysis
result <- meta_analysis(
  yi = studies$yi,
  sei = studies$sei,
  study_labels = studies$study
)

# Create forest plot
plot <- create_meta_forest_plot(result, title = "Treatment Effect")
} # }
```
