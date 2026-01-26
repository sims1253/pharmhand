# Assert Column Exists

Validates that a specified column exists in a data frame.

## Usage

``` r
assert_column_exists(data, col, data_arg = "data")
```

## Arguments

- data:

  Data frame to check

- col:

  Character string name of the column to check

- data_arg:

  Character string name of the data argument (for error messages)

## Value

Invisibly returns TRUE if validation passes
