# Set the Current Locale

Sets the current locale for pharmhand translations. This affects all
subsequent calls to
[`tr()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr.md)
and related translation functions.

## Usage

``` r
set_locale(locale = c("en", "de"))
```

## Arguments

- locale:

  Character. The locale to set. Currently supported values are `"en"`
  (English, default) and `"de"` (German). German translations are
  particularly important for G-BA/AMNOG dossier submissions.

## Value

Invisibly returns the previous locale setting.

## Details

The locale setting is stored in a package-level environment and persists
for the duration of the R session. To temporarily change the locale for
a specific operation, save the current locale, change it, perform the
operation, and restore the original locale.

## See also

[`get_locale()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_locale.md),
[`tr()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr.md),
[`get_translations()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_translations.md)

## Examples

``` r
# Set locale to German for GBA dossier
old_locale <- set_locale("de")
tr("treatment")
#> [1] "Behandlung"
# [1] "Behandlung"

# Restore previous locale
set_locale(old_locale)
```
