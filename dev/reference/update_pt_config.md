# Update PT configuration

Updates the PT configuration with new settings.

## Usage

``` r
update_pt_config(
  registry,
  variable = NULL,
  sort_by = NULL,
  min_subjects = NULL,
  top_n_per_soc = NULL
)
```

## Arguments

- registry:

  A ConfigurationRegistry object

- variable:

  Character string for PT variable name

- sort_by:

  Character string for sorting method

- min_subjects:

  Numeric minimum subjects

- top_n_per_soc:

  Numeric maximum PTs per SOC

## Value

The modified ConfigurationRegistry object

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
registry <- update_pt_config(
  registry,
  sort_by = "frequency"
)
} # }
```
