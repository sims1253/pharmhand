# Plot Imputation Convergence

Creates trace plots showing MCMC convergence of the imputation
algorithm.

## Usage

``` r
plot_imputation_convergence(
  imputation_result,
  vars = NULL,
  title = "Imputation Convergence"
)
```

## Arguments

- imputation_result:

  An ImputationResult object

- vars:

  Character vector of variables to plot. If NULL, plots all imputed
  variables.

- title:

  Plot title

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
imp <- perform_multiple_imputation(data, m = 5)
plot_imputation_convergence(imp)
} # }
```
