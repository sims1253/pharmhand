# Assert Numeric Vector

Validates that x is a numeric vector.

## Usage

``` r
assert_numeric_vector(x, len = NULL, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Value to check

- len:

  Optional integer specifying required length. If NULL, any length is
  allowed

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
