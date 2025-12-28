# Format confidence intervals

Formats lower and upper bounds of a confidence interval into a string.

## Usage

``` r
format_ci(lower, upper, digits = 2, separator = ", ")
```

## Arguments

- lower:

  Numeric vector of lower bounds.

- upper:

  Numeric vector of upper bounds.

- digits:

  Integer specifying the number of decimal places. Default is 2.

- separator:

  Character string to use as separator. Default is ", ".

## Value

Character vector with formatted confidence intervals.

## Examples

``` r
format_ci(1.23, 4.56, digits = 2) # Returns "1.23, 4.56"
#> [1] "1.23, 4.56"
format_ci(c(1.23, 5.67), c(4.56, 8.90), digits = 1) # c("1.2, 4.6")
#> [1] "1.2, 4.6" "5.7, 8.9"
```
