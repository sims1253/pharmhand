# Check G-BA Compliance for Tables

Validates tables before export to ensure basic G-BA Module 4 compliance.
This function checks structural requirements like column names, presence
of header/body rows, and applied G-BA theme markers.

## Usage

``` r
check_gba_compliance(
  x,
  strict = TRUE,
  require_theme = TRUE,
  require_title = FALSE,
  require_header = TRUE,
  require_body = TRUE,
  require_unique_colnames = TRUE,
  require_nonempty_colnames = TRUE
)
```

## Arguments

- x:

  A ClinicalTable, flextable, data.frame, or list of these

- strict:

  Logical, if TRUE throws an error on non-compliance

- require_theme:

  Logical, require G-BA theme marker (default: TRUE)

- require_title:

  Logical, require a title line (default: FALSE)

- require_header:

  Logical, require a header row in flextable (default: TRUE)

- require_body:

  Logical, require a body row in flextable (default: TRUE)

- require_unique_colnames:

  Logical, require unique column names (default: TRUE)

- require_nonempty_colnames:

  Logical, require non-empty column names (default: TRUE)

## Value

A list with `ok`, `errors`, and `warnings`

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(Statistic = "n", Value = 10)
check_gba_compliance(df, strict = FALSE)
} # }
```
