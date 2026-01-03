# Format Number with Locale

Helper function to format a number with locale-aware decimal separator.

## Usage

``` r
format_number(
  x,
  digits = 2L,
  locale = get_locale(),
  trim = FALSE,
  na_string = getOption("pharmhand.na_string", "NA")
)
```

## Arguments

- x:

  Numeric value

- digits:

  Integer number of decimal places

- locale:

  Character locale: "en" (period) or "de" (comma)

- trim:

  Logical, if TRUE remove trailing zeros (default: FALSE)

- na_string:

  String for missing values (default: getOption("pharmhand.na_string",
  "NA"))

## Value

Character formatted number

## Examples

``` r
format_number(1234.567, 2, "en")
#> [1] "1234.57"
# [1] "1234.57"

format_number(1234.567, 2, "de")
#> [1] "1234,57"
# [1] "1234,57"
```
