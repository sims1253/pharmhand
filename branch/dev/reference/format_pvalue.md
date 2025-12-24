# Format p-values

Formats p-values according to common scientific conventions. Values less
than 0.001 are formatted as "\<0.001".

## Usage

``` r
format_pvalue(p, digits = 3, threshold = 0.001)
```

## Arguments

- p:

  Numeric vector of p-values.

- digits:

  Integer specifying the number of decimal places for values \>= 0.001.
  Default is 3.

- threshold:

  Numeric value below which p-values are formatted as "\<threshold".
  Default is 0.001.

## Value

Character vector with formatted p-values.

## Examples

``` r
format_pvalue(0.0005, digits = 3) # Returns "<0.001"
#> [1] "<0.001"
format_pvalue(0.0234, digits = 3) # Returns "0.023"
#> [1] "0.023"
format_pvalue(c(0.0005, 0.0234, 0.4567), digits = 3)
#> [1] "<0.001" "0.023"  "0.457" 
# Returns c("<0.001", "0.023", "0.457")
```
