# Plot Imputation Distributions

Compares distributions of observed vs imputed values.

## Usage

``` r
plot_imputation_distributions(
  imputation_result,
  vars = NULL,
  title = "Observed vs Imputed Distributions"
)
```

## Arguments

- imputation_result:

  An ImputationResult object

- vars:

  Character vector of variables to plot. If NULL, plots all imputed
  numeric variables.

- title:

  Plot title

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
imp <- perform_multiple_imputation(data, m = 5)
plot_imputation_distributions(imp)
} # }
```
