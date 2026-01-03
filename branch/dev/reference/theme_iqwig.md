# IQWiG Theme for Flextable

Apply IQWiG-compliant styling to a flextable. Based on formatting
standards from IQWiG Methods v8.0, with numeric formatting driven by
`decimal_separator` and missing values shown via `na_string`.

## Usage

``` r
theme_iqwig(
  ft,
  font_name = "Arial",
  font_size = 9,
  header_bold = TRUE,
  decimal_separator = ",",
  na_string = getOption("pharmhand.na_string", "--"),
  autofit = TRUE
)
```

## Arguments

- ft:

  A flextable object

- font_name:

  Font family (default: "Arial")

- font_size:

  Font size in points (default: 9)

- header_bold:

  Logical, bold header text (default: TRUE)

- decimal_separator:

  Decimal separator: "." or "," (default: ","); applies to numeric
  columns with "." as the thousands separator when ",".

- na_string:

  String to display for missing values (default:
  getOption("pharmhand.na_string", "–"))

- autofit:

  Logical, autofit column widths (default: TRUE)

## Value

A styled flextable object

## References

IQWiG (2025). Allgemeine Methoden, Version 8.0.

## Examples

``` r
if (FALSE) { # \dontrun{
ft <- flextable::flextable(mtcars[1:5, 1:4])
ft <- theme_iqwig(ft)
} # }
```
