# Translate a Key to the Current Locale

Translates a single key or vector of keys to the currently active
locale. This is the primary function for internationalization in
pharmhand.

## Usage

``` r
tr(key, locale = NULL)
```

## Arguments

- key:

  Character. A translation key or vector of keys (e.g., `"treatment"`,
  `"hazard_ratio"`, `c("age", "sex")`).

- locale:

  Character or NULL. Optional locale to use instead of the current
  default. If NULL (default), uses the locale set by
  [`set_locale()`](https://sims1253.github.io/pharmhand/branch/dev/reference/set_locale.md).

## Value

Character vector of translated strings. If a key is not found, returns
the key itself with a warning.

## Details

Translation keys are case-insensitive and use underscores to separate
words (snake_case). Available keys can be viewed using
[`get_translations()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_translations.md).

Custom translations can be added using
[`add_translation()`](https://sims1253.github.io/pharmhand/branch/dev/reference/add_translation.md).
Custom translations take precedence over built-in translations.

## See also

[`tr_col()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr_col.md),
[`set_locale()`](https://sims1253.github.io/pharmhand/branch/dev/reference/set_locale.md),
[`get_translations()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_translations.md),
[`add_translation()`](https://sims1253.github.io/pharmhand/branch/dev/reference/add_translation.md)

## Examples

``` r
# English (default)
tr("treatment")
#> [1] "Behandlung"
# [1] "Treatment"

# German
tr("treatment", locale = "de")
#> [1] "Behandlung"
# [1] "Behandlung"

# Multiple keys
tr(c("age", "sex", "race"))
#> [1] "Alter"      "Geschlecht" "Ethnie"    
# [1] "Age" "Sex" "Race"

# Using current locale
set_locale("de")
tr("hazard_ratio")
#> [1] "Hazard Ratio"
# [1] "Hazard Ratio"
```
