# AnalysisMeta Class

An S7 class for tracking analysis metadata and audit trails, inspired by
Tplyr's tplyr_meta for regulatory traceability.

## Usage

``` r
AnalysisMeta(
  source_vars = character(0),
  filters = list(),
  row_id = "",
  derivation = "",
  timestamp = NULL,
  package_version = "",
  r_version = ""
)
```

## Arguments

- source_vars:

  Character vector of source variable names

- filters:

  List of filter expressions applied

- row_id:

  Character identifier for row-level tracing

- derivation:

  Character description of how the value was derived

- timestamp:

  POSIXct timestamp of analysis

- package_version:

  Character string of package version used

- r_version:

  Character string of R version used

## Value

An AnalysisMeta object

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- AnalysisMeta(
  source_vars = c("AGE", "SEX"),
  derivation = "count distinct USUBJID",
  row_id = "demographics_age_mean"
)
} # }
```
