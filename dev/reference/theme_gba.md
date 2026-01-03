# G-BA Module 4 Theme for Flextable

Apply G-BA Module 4 compliant styling to a flextable. Based on
formatting requirements from G-BA Dossier templates for AMNOG
submissions.

## Usage

``` r
theme_gba(
  ft,
  font_name = "Arial",
  font_size = 10,
  header_bold = TRUE,
  header_bg = "#E8E8E8",
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

  Font size in points (default: 10)

- header_bold:

  Logical, bold header text (default: TRUE)

- header_bg:

  Header background color (default: "#E8E8E8" light gray)

- decimal_separator:

  Decimal separator: "." or "," (default: ",")

- na_string:

  String to display for missing values (default:
  getOption("pharmhand.na_string", "–"))

- autofit:

  Logical, autofit column widths (default: TRUE)

## Value

A styled flextable object

## References

G-BA (2024). Dossiervorlage Modul 4. See <https://www.g-ba.de>

## Examples

``` r
if (FALSE) { # \dontrun{
ft <- flextable::flextable(mtcars[1:5, 1:4])
ft <- theme_gba(ft)
} # }
```
