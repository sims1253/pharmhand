# Define or override a subgroup configuration

Adds a new subgroup configuration or overrides an existing one. Runtime
overrides have higher priority than YAML configurations.

## Usage

``` r
define_subgroup_config(
  registry,
  name,
  variable,
  labels = NULL,
  order = NULL,
  filter_values = NULL,
  priority = 100
)
```

## Arguments

- registry:

  A ConfigurationRegistry object

- name:

  Character string name for the subgroup

- variable:

  Character string variable name in the data

- labels:

  Named list mapping values to display labels

- order:

  Character vector for display order

- filter_values:

  Optional character vector of values to include

- priority:

  Integer priority (default 100, higher than YAML)

## Value

The modified ConfigurationRegistry object

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
registry <- define_subgroup_config(
  registry,
  name = "age_custom",
  variable = "AGEGR1",
  labels = list(YOUNG = "< 75", OLD = ">= 75")
)
} # }
```
