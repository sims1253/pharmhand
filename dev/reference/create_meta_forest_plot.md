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
# Meta-analysis forest plot
yi <- log(c(0.75, 0.82, 0.68, 0.91))
sei <- c(0.12, 0.15, 0.18, 0.14)
result <- meta_analysis(
  yi = yi, sei = sei,
  study_labels = c("Study A", "Study B", "Study C", "Study D"),
  effect_measure = "hr"
)
plot <- create_meta_forest_plot(result, title = "Treatment Effect")
plot@type
#> [1] "forest_meta"
```
