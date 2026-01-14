# Define or override a population configuration

Define or override a population configuration

## Usage

``` r
define_population_config(
  registry,
  name,
  variable,
  label = NULL,
  description = NULL,
  flag_value = "Y",
  priority = 100L
)
```

## Arguments

- registry:

  A ConfigurationRegistry object

- name:

  Character string name for the population

- variable:

  Character string variable name in the data

- label:

  Character string display label

- description:

  Character string description

- flag_value:

  Character string indicating membership

- priority:

  Integer priority

## Value

The modified ConfigurationRegistry object

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
registry <- define_population_config(
  registry,
  name = "CUSTOM_POP",
  variable = "POPFL",
  label = "Custom Population"
)
} # }
```
