# Get the Current Locale

Returns the currently active locale for pharmhand translations.

## Usage

``` r
get_locale()
```

## Value

Character string indicating the current locale (`"en"` or `"de"`).

## See also

[`set_locale()`](https://sims1253.github.io/pharmhand/branch/dev/reference/set_locale.md),
[`tr()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr.md)

## Examples

``` r
get_locale()
#> [1] "en"
# [1] "en"

set_locale("de")
get_locale()
#> [1] "de"
# [1] "de"
```
