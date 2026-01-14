# Create Funnel Plot for Publication Bias Assessment

Creates a funnel plot to visually assess publication bias in
meta-analysis. Studies are plotted by effect size vs. precision (1/SE).

## Usage

``` r
create_funnel_plot(
  meta_result,
  show_ci = TRUE,
  show_egger = TRUE,
  title = "Funnel Plot",
  xlab = NULL,
  ylab = "Standard Error",
  contour_levels = c(0.1, 0.05, 0.01),
  base_size = 11
)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis()

- show_ci:

  Logical. Show pseudo 95% CI region. Default: TRUE

- show_egger:

  Logical. Show Egger's regression line. Default: TRUE

- title:

  Character. Plot title. Default: "Funnel Plot"

- xlab:

  Character. X-axis label. Default: based on effect_measure

- ylab:

  Character. Y-axis label. Default: "Standard Error"

- contour_levels:

  Numeric vector. Contour p-value levels. Default: c(0.1, 0.05, 0.01)

- base_size:

  Numeric. Base font size. Default: 11

## Value

A ClinicalPlot object containing the funnel plot

## Examples

``` r
# Funnel plot for publication bias assessment
yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)
meta_res <- meta_analysis(yi = yi, sei = sei, effect_measure = "hr")
plot <- create_funnel_plot(meta_res, title = "Funnel Plot")
plot@type
#> [1] "funnel"
```
