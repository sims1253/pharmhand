# IQWiG Theme for Flextable

Apply IQWiG-compliant styling to a flextable. Based on formatting
standards from IQWiG Methods v8.0.

## Usage

``` r
theme_iqwig(
  ft,
  font_name = "Arial",
  font_size = 9,
  header_bold = TRUE,
  decimal_separator = ",",
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

  Decimal separator: "." or "," (default: ",")

- autofit:

  Logical, autofit column widths (default: TRUE)

## Value

A styled flextable object

## References

IQWiG (2023). Allgemeine Methoden, Version 8.0.

## Examples

``` r
if (FALSE) { # \dontrun{
ft <- flextable::flextable(mtcars[1:5, 1:4])
ft <- theme_iqwig(ft)
} # }
```
