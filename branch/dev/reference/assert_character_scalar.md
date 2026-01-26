# Assert Character Scalar

Validates that x is a single character string.

## Usage

``` r
assert_character_scalar(x, arg = deparse(substitute(x)), allow_empty = FALSE)
```

## Arguments

- x:

  Value to check

- arg:

  Character string describing the argument (for error messages)

- allow_empty:

  Logical. If TRUE, empty strings are allowed. Default: FALSE

## Value

Invisibly returns x if validation passes
