# Convert Chef Results to AnalysisResults

Transform chef output to AnalysisResults.

## Usage

``` r
chef_to_analysis_results(chef_output, type = "hta", metadata = list())
```

## Arguments

- chef_output:

  A data.table from chef pipeline execution

- type:

  Character string for result type (default: "hta")

- metadata:

  Additional metadata to attach

## Value

An AnalysisResults S7 object

## Examples

``` r
if (FALSE) { # \dontrun{
# After running chef pipeline
results <- chef_to_analysis_results(chef_output)
table <- create_clinical_table(results, title = "HTA Analysis")
} # }
```
