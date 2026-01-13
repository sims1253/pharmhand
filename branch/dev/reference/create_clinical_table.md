# Create HTA-Style Table Create Clinical Table (Factory Function)

Convenience factory function that creates a properly formatted clinical
table with flextable styling. Reduces boilerplate code across the
package.

## Usage

``` r
create_clinical_table(
  data,
  type,
  title = NULL,
  footnotes = character(),
  metadata = list(),
  col_widths = NULL,
  autofit = ph_default("autofit")
)
```

## Arguments

- data:

  Data frame to display, or an `AnalysisResults` object (in which case
  `@stats` is used).

- type:

  Character string for table type (e.g., "demographics", "ae_soc")

- title:

  Table title

- footnotes:

  Character vector of footnotes

- metadata:

  List of additional metadata

- col_widths:

  Named numeric vector of column widths (optional)

- autofit:

  Logical, whether to autofit column widths

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(Treatment = c("A", "B"), N = c(100, 95))
table <- create_clinical_table(
  data = df,
  type = "summary",
  title = "Treatment Summary",
  footnotes = "ITT Population"
)
} # }
```
