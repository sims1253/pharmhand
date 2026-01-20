# Summarize Bayesian Meta-Analysis for Few Studies

Creates a summary table from Bayesian meta-analysis results for few
studies.

## Usage

``` r
summary_bayesian_few(result, digits = 3)
```

## Arguments

- result:

  A BayesianMetaFewResult object

- digits:

  Integer. Number of decimal places to display

## Value

A data frame with summary statistics

## Examples

``` r
if (FALSE) { # \dontrun{
summary <- summary_bayesian_few(result)
print(summary)
} # }
```
