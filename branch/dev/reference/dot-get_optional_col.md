# Get Optional Column Value

Safely extract a column value, returning a default if not present or NA.

## Usage

``` r
.get_optional_col(row, col_name, default = "")
```

## Arguments

- row:

  A single-row data frame or named list (not a multi-row data frame)

- col_name:

  Character. Column name to extract.

- default:

  Default value if column missing or NA. Default: ""

## Value

The column value or default
