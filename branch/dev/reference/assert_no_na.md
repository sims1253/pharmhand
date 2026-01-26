# Assert No NA

Validates that x does not contain any NA values.

## Usage

``` r
assert_no_na(x, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Vector to check (any atomic type)

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
