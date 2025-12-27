# Run Chef Pipeline with pharmhand Integration

Execute a chef analysis pipeline and return results as pharmhand.

## Usage

``` r
run_chef_pipeline(
  adam_data,
  endpoints,
  output_type = c("results", "table", "report"),
  ...
)
```

## Arguments

- adam_data:

  Named list of ADaM datasets (e.g., list(adsl = adsl, adae = adae))

- endpoints:

  List of endpoint specifications from create_chef_endpoint()

- output_type:

  Type of output: "results", "table", or "report"

- ...:

  Additional arguments passed to chef functions

## Value

Depending on output_type:

- "results": AnalysisResults object

- "table": ClinicalTable object

- "report": ClinicalReport object

## Examples

``` r
if (FALSE) { # \dontrun{
# Define endpoints
endpoints <- list(
  create_chef_endpoint("AE Rate", "AEDECOD", type = "count")
)

# Run pipeline
report <- run_chef_pipeline(
  adam_data = list(adsl = adsl, adae = adae),
  endpoints = endpoints,
  output_type = "report"
)
} # }
```
