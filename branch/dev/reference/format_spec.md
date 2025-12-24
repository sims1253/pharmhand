# Create a Format Specification

Define how numeric values should be formatted using pattern strings.
Patterns use 'x' for integer digits and decimal points for precision.

## Usage

``` r
format_spec(
  pattern = "a.a",
  null_format = "--",
  empty_format = "",
  neg_format = "sign"
)
```

## Arguments

- pattern:

  Character string defining the format pattern:

  - "xx" = 2 integer digits, no decimals

  - "xx.x" = 2 integer digits, 1 decimal

  - "xx.xxx" = 2 integer digits, 3 decimals

  - "a" = auto-width integers

  - "a.a" = auto-width with auto decimals

  - "a.a+1" = auto decimals plus 1

- null_format:

  Format for NULL/NA values (default: "–")

- empty_format:

  Format for empty strings (default: "")

- neg_format:

  How to handle negatives: "sign", "parens", or "abs"

## Value

A FormatSpec S7 object

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed format: 2 integers, 1 decimal
fmt <- format_spec("xx.x")
apply_format(fmt, 12.345) # "12.3"

# Auto-width with 2 decimals
fmt <- format_spec("a.xx")
apply_format(fmt, c(1.5, 123.456)) # "  1.50", "123.46"
} # }
```
