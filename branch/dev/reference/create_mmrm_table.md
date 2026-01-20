# Create MMRM Table

Creates a clinical table from MMRM results.

## Usage

``` r
create_mmrm_table(
  result,
  title = "Mixed Model Repeated Measures Analysis",
  subtitle = NULL,
  footnotes = NULL,
  autofit = TRUE
)
```

## Arguments

- result:

  An MMRMResult object

- title:

  Table title

- subtitle:

  Optional subtitle

- footnotes:

  Optional footnotes

- autofit:

  Logical. Whether to autofit column widths

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
table <- create_mmrm_table(result, title = "MMRM Results")
} # }
```
