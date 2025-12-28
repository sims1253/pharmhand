# Composite Format Specification

Create a format that combines multiple values (e.g., "n (pct%)").

## Usage

``` r
composite_format(template, ...)
```

## Arguments

- template:

  Character template with placeholders

- ...:

  Named format specs for each placeholder

## Value

A CompositeFormat object

## Examples

``` r
if (FALSE) { # \dontrun{
fmt <- composite_format(
  "{n} ({pct}%)",
  n = "a",
  pct = "xx.x"
)
apply_composite(fmt, n = 15, pct = 23.456) # "15 (23.5%)"
} # }
```
