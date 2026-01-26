# Assert Positive Integer Scalar

Validates that x is a single positive integer value (\>= 1). NA values
are rejected.

## Usage

``` r
assert_positive_integer(x, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Value to check

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
