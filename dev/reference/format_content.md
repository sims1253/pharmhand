# Format clinical content to different output formats

Generic function for formatting clinical content to various output
formats. Uses multiple dispatch on both `x` and `format`.

## Usage

``` r
format_content(x, format, ...)
```

## Arguments

- x:

  A ClinicalContent object

- format:

  Character string specifying output format

- ...:

  Additional arguments passed to methods

## Value

Formatted content in the specified format

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert table to different formats
png_data <- format_content(clinical_table, "png")
} # }
```
