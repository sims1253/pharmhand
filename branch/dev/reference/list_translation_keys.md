# List Available Translation Keys

Returns a character vector of all available translation keys.

## Usage

``` r
list_translation_keys(locale = NULL, pattern = NULL)
```

## Arguments

- locale:

  Character or NULL. Locale to check for keys. If NULL, returns keys
  from all locales combined.

- pattern:

  Character or NULL. Optional regex pattern to filter keys.

## Value

Character vector of translation key names.

## See also

[`tr()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr.md),
[`get_translations()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_translations.md)

## Examples

``` r
# All keys
head(list_translation_keys())
#> [1] "active_control" "adverse_event"  "ae"             "ae_short"      
#> [5] "age"            "age_group"     

# Keys containing "ae"
list_translation_keys(pattern = "ae")
#>  [1] "ae"                     "ae_short"               "fn_sae"                
#>  [4] "fn_teae"                "sae"                    "sae_short"             
#>  [7] "subjects_with_any_ae"   "subjects_with_any_sae"  "subjects_with_any_teae"
#> [10] "teae"                   "teae_short"            

# Keys for demographics
list_translation_keys(pattern = "^(age|sex|race|ethnic)")
#> [1] "age"       "age_group" "age_years" "ethnicity" "race"      "sex"      
```
