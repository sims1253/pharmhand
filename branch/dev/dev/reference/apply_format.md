# Apply a format specification to values

Format numeric values according to the specification.

## Usage

``` r
apply_format(spec, x, align = TRUE)
```

## Arguments

- spec:

  A FormatSpec object or pattern string

- x:

  Numeric vector to format

- align:

  Logical, right-align values for tabular display

## Value

Character vector of formatted values

## Examples

``` r
if (FALSE) { # \dontrun{
apply_format("xx.x", 12.345) # "12.3"
apply_format("a.xx", c(1, 123)) # "  1.00", "123.00"
} # }
```
