# Apply a composite format

Substitutes placeholder values into a composite format template. Each
placeholder in the template (e.g., `{n}`) is replaced with the
corresponding formatted value.

## Usage

``` r
apply_composite(fmt, ...)
```

## Arguments

- fmt:

  CompositeFormat object

- ...:

  Named values matching template placeholders

## Value

Formatted character string

## Note

This function uses simple string substitution via
[`gsub()`](https://rdrr.io/r/base/grep.html). If a placeholder name
accidentally appears as literal text in the template (not as a
placeholder), it will also be replaced. Ensure placeholder names are
unique and unlikely to appear as regular text in templates.
