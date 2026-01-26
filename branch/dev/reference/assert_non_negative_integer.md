# Assert Non-Negative Integer Scalar

Validates that x is a single non-negative integer value (\>= 0). NA
values are rejected.

## Usage

``` r
assert_non_negative_integer(x, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Value to check

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
