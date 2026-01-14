# Clinical table from analysis results

Converts an `AnalysisResults` object into a `ClinicalTable` for
reporting.

## Usage

``` r
clinical_table_from_results(res, title = "")
```

## Arguments

- res:

  An `AnalysisResults` object with `@stats` (table data) and `@type`
  (table type) slots.

- title:

  Optional table title.

## Value

A `ClinicalTable` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert analysis results to clinical table
results <- analyze_demographics(adsl_data)
table <- clinical_table_from_results(results, title = "Demographics")
} # }
```
