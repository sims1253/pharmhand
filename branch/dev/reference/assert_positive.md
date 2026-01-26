# Assert Positive

Validates that x is a single positive number (\> 0). NA values are
rejected.

## Usage

``` r
assert_positive(x, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Value to check

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
