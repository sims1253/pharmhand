# Create Module 4 Table Template

Standardized G-BA Module 4 table with fixed column structure.

## Usage

``` r
create_hta_module4_table(
  data = NULL,
  title = "Module 4 Summary",
  footnotes = character(),
  columns = c("Endpoint", "Analysis Set", "Treatment", "Comparator", "Effect", "95% CI",
    "p-value"),
  col_widths = NULL,
  allow_extra = FALSE,
  autofit = TRUE
)
```

## Arguments

- data:

  Data frame to display. If NULL, an empty template is created.

- title:

  Table title (default: "Module 4 Summary")

- footnotes:

  Character vector of footnotes

- columns:

  Character vector of required column names

- col_widths:

  Named numeric vector of column widths (optional)

- allow_extra:

  Logical, allow extra columns beyond `columns` (default: FALSE)

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
table <- create_hta_module4_table()
} # }
```
