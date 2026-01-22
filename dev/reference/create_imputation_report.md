# Create Imputation Diagnostic Report

Generates a comprehensive diagnostic report for multiple imputation.

## Usage

``` r
create_imputation_report(imputation_result)
```

## Arguments

- imputation_result:

  An ImputationResult object

## Value

A list containing:

- summary:

  Missing data summary data frame

- convergence_plot:

  ClinicalPlot of convergence diagnostics

- distribution_plot:

  ClinicalPlot comparing distributions

- missing_pattern_plot:

  ClinicalPlot of missing patterns

## Examples

``` r
if (FALSE) { # \dontrun{
imp <- perform_multiple_imputation(data, m = 5)
report <- create_imputation_report(imp)
report$convergence_plot
} # }
```
