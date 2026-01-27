# Assert Data Frame

Validates that x is a data frame and optionally checks for required
columns.

## Usage

``` r
assert_data_frame(x, required_cols = NULL, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Value to check

- required_cols:

  Optional character vector of required column names

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
