# Assert Character Vector

Validates that x is a character vector.

## Usage

``` r
assert_character_vector(x, len = NULL, arg = deparse(substitute(x)))
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
