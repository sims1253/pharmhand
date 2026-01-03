# Format Confidence Interval (IQWiG-Compliant)

Formats confidence intervals according to IQWiG Methods v8.0, Chapter
10.3.2:

- Report whether CIs are 1- or 2-sided and the confidence level (e.g.,
  95%); IQWiG does not prescribe punctuation between bounds

- Semicolon separator is a formatting choice: `[lower; upper]`

- Locale-aware decimal separator

## Usage

``` r
format_ci(
  lower,
  upper,
  digits = 2L,
  locale = get_locale(),
  trim = FALSE,
  na_string = getOption("pharmhand.na_string", "NA"),
  brackets = c("[", "]")
)
```

## Arguments

- lower:

  Numeric lower bound (or vector)

- upper:

  Numeric upper bound (or vector)

- digits:

  Integer number of decimal places (default: 2)

- locale:

  Character locale for decimal separator: "en" or "de" (default: current
  pharmhand locale)

- trim:

  Logical, if TRUE remove trailing zeros (default: FALSE)

- na_string:

  String for missing values (default: getOption("pharmhand.na_string",
  "NA"))

- brackets:

  Character vector of length 2 for opening/closing brackets (default:
  `c("[", "]")`)

## Value

Character vector of formatted confidence intervals

## References

IQWiG (2025). Allgemeine Methoden, Version 8.0, Chapter 10.3.2, p. 212.

## Examples

``` r
format_ci(0.85, 1.23)
#> [1] "[0.85; 1.23]"
# [1] "[0.85; 1.23]"

format_ci(0.85, 1.23, locale = "de")
#> [1] "[0,85; 1,23]"
# [1] "[0,85; 1,23]"

format_ci(c(0.5, 0.7), c(0.9, 1.1))
#> [1] "[0.50; 0.90]" "[0.70; 1.10]"
# [1] "[0.50; 0.90]" "[0.70; 1.10]"
```
