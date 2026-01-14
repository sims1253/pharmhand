# Create a flextable formatted for HTA/AMNOG submissions.

Create a flextable formatted for HTA/AMNOG submissions.

## Usage

``` r
create_hta_table(
  data,
  title = NULL,
  footnotes = character(),
  col_widths = NULL,
  autofit = ph_default("autofit")
)
```

## Arguments

- data:

  Data frame to display

- title:

  Table title

- footnotes:

  Character vector of footnotes

- col_widths:

  Named numeric vector of column widths (optional)

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A styled flextable object
