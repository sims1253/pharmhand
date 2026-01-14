# Get subgroup configuration

Retrieves a subgroup configuration by name.

## Usage

``` r
get_subgroup_config(registry, name)
```

## Arguments

- registry:

  A ConfigurationRegistry object

- name:

  Character string name of the subgroup

## Value

A SubgroupConfig object or NULL if not found

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
config <- get_subgroup_config(registry, "age_groups")
} # }
```
