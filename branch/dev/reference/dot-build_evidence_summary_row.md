# Build Evidence Summary Row

Internal helper to build a single row of data for the evidence summary
table.

## Usage

``` r
.build_evidence_summary_row(endpoint_data, name, conf_level, language)
```

## Arguments

- endpoint_data:

  List containing endpoint components (result, grade, rob)

- name:

  Character string for endpoint name

- conf_level:

  Numeric confidence level

- language:

  Language setting ("en" or "de")

## Value

List of row data with all column values
