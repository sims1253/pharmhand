# Reset Custom Translations

Removes all custom translations, reverting to built-in translations
only.

## Usage

``` r
reset_custom_translations(locale = NULL)
```

## Arguments

- locale:

  Character or NULL. If provided, only reset translations for the
  specified locale. If NULL (default), reset all custom translations.

## Value

Invisibly returns the removed custom translations.

## See also

[`add_translation()`](https://sims1253.github.io/pharmhand/dev/reference/add_translation.md),
[`get_translations()`](https://sims1253.github.io/pharmhand/dev/reference/get_translations.md)

## Examples

``` r
# Add custom translation
add_translation("test_key", c(en = "Test", de = "Test"))

# Reset all custom translations
reset_custom_translations()
```
