# Get Summary Label

Get a summary label for ADaMData or AnalysisResults. For ADaMData,
returns "Population (N=X)". For AnalysisResults, returns "type (n=Y)" or
"type (empty)".

## Usage

``` r
get_summary_label(object)
```

## Arguments

- object:

  ADaMData or AnalysisResults object

## Value

Character string summary label

## Examples

``` r
if (FALSE) { # \dontrun{
adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
get_summary_label(adam)  # "FAS (N=150)"

results <- calculate_baseline(adam, vars = c("AGE"))
get_summary_label(results)  # "baseline (n=2)"
} # }
```
