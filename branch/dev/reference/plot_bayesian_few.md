# Plot Bayesian Meta-Analysis Results for Few Studies

Creates forest plot for Bayesian meta-analysis results for few studies.

## Usage

``` r
plot_bayesian_few(
  result,
  title = "Bayesian Meta-Analysis Results (Few Studies)"
)
```

## Arguments

- result:

  A BayesianMetaFewResult object

- title:

  Plot title

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
plot <- plot_bayesian_few(
  result, title = "Bayesian Meta-Analysis (Few Studies)"
)
} # }
```
