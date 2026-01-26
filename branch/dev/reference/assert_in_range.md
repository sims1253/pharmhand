# Assert In Range

Validates that x is within the specified range (exclusive bounds). NA
values are rejected.

## Usage

``` r
assert_in_range(x, lower = -Inf, upper = Inf, arg = deparse(substitute(x)))
```

## Arguments

- x:

  Value to check

- lower:

  Lower bound (exclusive)

- upper:

  Upper bound (exclusive)

- arg:

  Character string describing the argument (for error messages)

## Value

Invisibly returns x if validation passes
