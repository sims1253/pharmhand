# Create Format Specification

Define numeric formats using pattern strings.

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

  Format pattern string (default "a.a")

- null_format:

  String to display for NULL/NA values (default "–")

- empty_format:

  String to display for empty values (default "")

- neg_format:

  How to format negative numbers: "sign", "parens", or "abs"

## Value

A FormatSpec object

## Examples

``` r
if (FALSE) { # \dontrun{
fmt <- format_spec("xx.x")
apply_format(fmt, 12.345) # "12.3"

# Auto-width with 2 decimals
fmt <- format_spec("a.xx")
apply_format(fmt, c(1.5, 123.456)) # "  1.50", "123.46"
} # }
```
