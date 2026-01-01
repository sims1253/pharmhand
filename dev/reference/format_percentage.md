# Format values as percentages

Formats numeric values as percentages with specified decimal places.

## Usage

``` r
format_percentage(x, digits = 1)
```

## Arguments

- x:

  Numeric vector (decimal form, e.g., 0.5 for 50%).

- digits:

  Integer specifying the number of decimal places. Default is 1.

## Value

Character vector with formatted percentages.

## Examples

``` r
format_percentage(0.5, digits = 1) # Returns "50.0%"
#> [1] "50.0%"
format_percentage(c(0.123, 0.456, 0.789), digits = 1) # c("12.3%", "45.6%")
#> [1] "12.3%" "45.6%" "78.9%"
```
