# Format n/N Values

Vectorized helper for formatting n/N strings with optional percentage.

## Usage

``` r
.format_n_over_n(n, N, pct = NULL, digits = 1, na_label = "N/A")
```

## Arguments

- n:

  Numeric vector of numerators

- N:

  Numeric vector of denominators (recycled to length of n)

- pct:

  Numeric vector of percentages (optional, recycled to length of n)

- digits:

  Integer, digits for percentage formatting (default: 1)

- na_label:

  Character string to use when N is NA or 0 (default: "N/A")

## Value

Character vector of formatted n/N strings
