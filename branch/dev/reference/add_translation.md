# Add Custom Translation

Adds a custom translation for a key in one or more locales. Custom
translations take precedence over built-in translations, allowing users
to override defaults or add study-specific terminology.

## Usage

``` r
add_translation(key, translations)
```

## Arguments

- key:

  Character. The translation key to add or override. Keys should use
  snake_case (lowercase with underscores).

- translations:

  Named list or character vector. Translations for each locale. Names
  should be locale codes (e.g.,
  `c(en = "English Term", de = "German Term")`).

## Value

Invisibly returns the previous translation for the key (or NULL if it
was new).

## Details

Custom translations persist for the duration of the R session. They are
stored separately from built-in translations and take precedence when
translating keys.

This is particularly useful for:

- Study-specific terminology

- Regulatory-specific terms not in the default dictionary

- Correcting or adjusting default translations for specific contexts

## See also

[`tr()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr.md),
[`get_translations()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_translations.md)

## Examples

``` r
# Add a custom translation
add_translation(
  "study_drug",
  c(en = "DrugX 100mg", de = "MedikamentX 100mg")
)

tr("study_drug")
#> [1] "DrugX 100mg"
# [1] "DrugX 100mg"

tr("study_drug", locale = "de")
#> [1] "MedikamentX 100mg"
# [1] "MedikamentX 100mg"

# Override existing translation
add_translation("treatment", c(en = "Active Treatment", de = "Aktive Behandlung"))
```
