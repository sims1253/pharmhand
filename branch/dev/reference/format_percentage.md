# Format Percentage

Formats percentages with locale-aware decimal separator.

## Usage

``` r
format_percentage(
  x,
  digits = 1L,
  locale = get_locale(),
  trim = FALSE,
  na_string = getOption("pharmhand.na_string", "NA"),
  is_proportion = FALSE,
  symbol = TRUE
)
```

## Arguments

- x:

  Numeric value (proportion 0-1 or percentage 0-100)

- digits:

  Integer number of decimal places (default: 1)

- locale:

  Character locale: "en" or "de"

- trim:

  Logical, if TRUE remove trailing zeros (default: FALSE)

- na_string:

  String for missing values (default: getOption("pharmhand.na_string",
  "NA"))

- is_proportion:

  Logical, if TRUE treats x as proportion (0-1), otherwise as percentage
  (0-100). Default: FALSE

- symbol:

  Logical, if TRUE appends "%" symbol. Default: TRUE

## Value

Character formatted percentage

## Examples

``` r
format_percentage(23.5)
#> [1] "23.5%"
# [1] "23.5%"

format_percentage(0.235, is_proportion = TRUE)
#> [1] "23.5%"
# [1] "23.5%"

format_percentage(23.5, locale = "de")
#> [1] "23,5%"
# [1] "23,5%"
```
