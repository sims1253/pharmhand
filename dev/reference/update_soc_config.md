# Update SOC configuration

Updates the SOC configuration with new settings.

## Usage

``` r
update_soc_config(
  registry,
  variable = NULL,
  sort_by = NULL,
  min_subjects = NULL,
  top_n = NULL
)
```

## Arguments

- registry:

  A ConfigurationRegistry object

- variable:

  Character string for SOC variable name

- sort_by:

  Character string for sorting method

- min_subjects:

  Numeric minimum subjects

- top_n:

  Numeric maximum number of SOCs

## Value

The modified ConfigurationRegistry object

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
registry <- update_soc_config(
  registry,
  sort_by = "alphabetical",
  top_n = 10
)
} # }
```
