# Translate Column Names

Translates a vector of column names to the current locale. This is a
convenience wrapper around
[`tr()`](https://sims1253.github.io/pharmhand/dev/reference/tr.md)
designed for translating data frame column names or table headers.

## Usage

``` r
tr_col(cols, locale = NULL)
```

## Arguments

- cols:

  Character vector of column names to translate.

- locale:

  Character or NULL. Optional locale override.

## Value

Named character vector where names are original column names and values
are translated strings. This makes it easy to use with
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
or similar functions.

## See also

[`tr()`](https://sims1253.github.io/pharmhand/dev/reference/tr.md),
[`get_translations()`](https://sims1253.github.io/pharmhand/dev/reference/get_translations.md)

## Examples

``` r
# Translate column names
cols <- c("treatment", "placebo", "total")
tr_col(cols)
#>    treatment      placebo        total 
#> "Behandlung"    "Placebo"     "Gesamt" 
# treatment   placebo     total
# "Treatment" "Placebo"   "Total"

# Use with dplyr::rename
if (FALSE) { # \dontrun{
set_locale("de")
df |> dplyr::rename(!!!tr_col(c("treatment", "total")))
} # }
```
