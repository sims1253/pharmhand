# Get population configuration

Retrieves a population configuration by name.

## Usage

``` r
get_population_config(registry, name)
```

## Arguments

- registry:

  A ConfigurationRegistry object

- name:

  Character string name of the population

## Value

A PopulationConfig object or NULL if not found

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
config <- get_population_config(registry, "SAF")
} # }
```
