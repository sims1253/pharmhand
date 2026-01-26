# Assert All Positive

Validates that all elements in x are positive (\> 0). NA values are
rejected.

## Usage

``` r
assert_all_positive(x, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Numeric vector to check

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
