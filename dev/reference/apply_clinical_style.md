# Apply Clinical Table Styling

Apply standardized clinical table styling to a flextable, inspired by
carver's tbl_display pattern.

## Usage

``` r
apply_clinical_style(
  ft,
  style = c("default", "clinical", "hta", "compact"),
  font_name = "Arial",
  font_size_body = 9,
  font_size_header = 10,
  border_color = "gray70",
  header_bg = "gray95",
  body_bg = "white",
  font_color = "black",
  zebra = FALSE,
  na_string = "--",
  autofit = TRUE
)
```

## Arguments

- ft:

  A flextable object

- style:

  Style preset: "default", "clinical", "hta", or "compact"

- font_name:

  Font family (default: "Arial")

- font_size_body:

  Body font size in points (default: 9)

- font_size_header:

  Header font size in points (default: 10)

- border_color:

  Border color (default: "gray70")

- header_bg:

  Header background color (default: "gray95")

- body_bg:

  Body background color (default: "white")

- font_color:

  Font color for all text (default: "black")

- zebra:

  Logical, apply zebra striping (default: FALSE)

- na_string:

  String to display for NA values (default: "–")

- autofit:

  Logical, perform expensive layout calculations (default: TRUE)

## Value

A styled flextable object

## Examples

``` r
if (FALSE) { # \dontrun{
ft <- flextable::flextable(mtcars[1:5, 1:4])
ft <- apply_clinical_style(ft, style = "clinical")
} # }
```
