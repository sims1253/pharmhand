# Assert Numeric Scalar

Validates that x is a single numeric value (length 1). NA values are
rejected.

## Usage

``` r
assert_numeric_scalar(x, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Value to check

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
