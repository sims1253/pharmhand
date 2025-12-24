# Create Analysis Metadata

Helper function to create an AnalysisMeta object with current
environment info.

## Usage

``` r
create_analysis_meta(
  source_vars = character(),
  filters = list(),
  row_id = "",
  derivation = ""
)
```

## Arguments

- source_vars:

  Character vector of source variable names

- filters:

  List of filter expressions

- row_id:

  Character identifier for the row

- derivation:

  Character description of derivation

## Value

An AnalysisMeta object with timestamp and version info populated
