# Format numbers with specified decimal places

Formats numeric values with a specified number of decimal places.

## Usage

``` r
format_number(x, digits = 2)
```

## Arguments

- x:

  Numeric vector to format.

- digits:

  Integer specifying the number of decimal places. Default is 2.

## Value

Character vector with formatted numbers.

## Examples

``` r
format_number(45.2345, digits = 2) # Returns "45.23"
#> [1] "45.23"
format_number(c(1.5, 2.345, 3.6789), digits = 1) # Returns c("1.5", "2.3", "3.7")
#> [1] "1.5" "2.3" "3.7"
```
