# Summarize Missing Data Patterns

Creates a summary of missing data in a data frame.

## Usage

``` r
summarize_missing(data)
```

## Arguments

- data:

  A data frame

## Value

A data frame with columns:

- variable:

  Variable name

- n_missing:

  Count of missing values

- n_complete:

  Count of non-missing values

- pct_missing:

  Percentage missing

## Examples

``` r
data <- data.frame(
  x = c(1, NA, 3, NA, 5),
  y = c("a", "b", NA, "d", "e"),
  z = 1:5
)
summarize_missing(data)
#>   variable n_missing n_complete pct_missing
#> 1        x         2          3          40
#> 2        y         1          4          20
#> 3        z         0          5           0
```
