# Assert Data Frame

Validates that x is a data frame and optionally checks for required
columns.

## Usage

``` r
assert_data_frame(x, ...)
```

## Arguments

- x:

  Value to check

- ...:

  Either `arg` as character string (argument name for error messages),
  or `required_cols` (character vector of required column names)
  followed by `arg`

## Value

Invisibly returns x if validation passes
