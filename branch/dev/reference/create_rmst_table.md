# Create RMST Table

Creates a clinical table from RMST analysis results.

## Usage

``` r
create_rmst_table(
  result,
  title = "Restricted Mean Survival Time Analysis",
  subtitle = NULL,
  footnotes = NULL,
  autofit = TRUE
)
```

## Arguments

- result:

  An RMSTResult object

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
table <- create_rmst_table(result, title = "RMST Analysis")
} # }
```
