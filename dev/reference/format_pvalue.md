# Format P-Value (IQWiG-Compliant)

Formats p-values according to IQWiG Methods v8.0, Chapter 10.3.2 (p.
212):

- 3 decimal places

- Values \< 0.001 displayed as "\<0.001" (or "\<0,001" in German locale)

- Locale-aware decimal separator

## Usage

``` r
format_pvalue(
  p,
  digits = 3L,
  threshold = 0.001,
  locale = get_locale(),
  trim = FALSE,
  na_string = getOption("pharmhand.na_string", "NA")
)
```

## Arguments

- p:

  Numeric p-value or vector of p-values

- digits:

  Integer number of decimal places (default: 3, per IQWiG)

- threshold:

  Numeric threshold below which to display "\<threshold" (default:
  0.001)

- locale:

  Character locale for decimal separator: "en" or "de" (default: current
  pharmhand locale)

- trim:

  Logical, if TRUE remove trailing zeros (default: FALSE)

- na_string:

  String for missing values (default: getOption("pharmhand.na_string",
  "NA"))

## Value

Character vector of formatted p-values

## References

IQWiG (2025). Allgemeine Methoden, Version 8.0, Chapter 10.3.2, p. 212.

## Examples

``` r
format_pvalue(0.0234)
#> [1] "0.023"
# [1] "0.023"

format_pvalue(0.0234, locale = "de")
#> [1] "0,023"
# [1] "0,023"

format_pvalue(0.00005)
#> [1] "<0.001"
# [1] "<0.001"

format_pvalue(c(0.05, 0.001, 0.0001))
#> [1] "0.050"  "0.001"  "<0.001"
# [1] "0.050" "0.001" "<0.001"
```
