# Convert Clinical Content to G-BA Template

Applies G-BA Module 4 formatting rules to clinical content. For
ClinicalTable objects, the G-BA theme is applied. For ClinicalReport
objects, all contained tables are formatted.

## Usage

``` r
to_gba_template(x, path = NULL, autofit = TRUE)
```

## Arguments

- x:

  A ClinicalTable, ClinicalReport, or list of these

- path:

  Optional file path. If provided with a ClinicalReport, the report is
  written to disk.

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable or ClinicalReport with G-BA formatting applied

## Examples

``` r
if (FALSE) { # \dontrun{
table <- create_hta_module4_table()
table <- to_gba_template(table)
} # }
```
