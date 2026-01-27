# ClinicalTable Class

An S7 class for representing clinical study tables with methods for
formatting and conversion to Word elements.

## Usage

``` r
ClinicalTable(
  type = character(0),
  title = NULL,
  metadata = list(),
  data = data.frame(),
  flextable = NULL,
  footnotes = character(0)
)
```

## Arguments

- type:

  Character string for table type (e.g., "demographics",
  "adverse_events")

- title:

  Character string for table title

- metadata:

  List of additional metadata

- data:

  A data frame containing the table data

- flextable:

  A flextable object (optional, created if not provided)

- footnotes:

  Character vector of table footnotes

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
table <- ClinicalTable(
  data = data.frame(x = 1:3, y = c("a", "b", "c")),
  type = "test",
  title = "Test Table"
)
} # }
```
