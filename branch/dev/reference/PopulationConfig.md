# PopulationConfig Class

Configuration for analysis populations (e.g., SAF, FAS, PPS).

## Usage

``` r
PopulationConfig(
  source = "yaml",
  priority = 0L,
  variable = character(0),
  label = character(0),
  description = NULL,
  flag_value = "Y"
)
```

## Arguments

- source:

  Character string for configuration source

- priority:

  Integer priority

- variable:

  Character string specifying the variable name in the data

- label:

  Character string for population display label

- description:

  Character string describing the population

- flag_value:

  Character string value indicating membership (default "Y")

## Value

A PopulationConfig object

## Examples

``` r
if (FALSE) { # \dontrun{
config <- PopulationConfig(
  variable = "SAFFL",
  label = "Safety Analysis Set",
  description = "Subjects who received at least one dose"
)
} # }
```
