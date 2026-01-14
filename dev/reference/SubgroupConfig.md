# SubgroupConfig Class

Configuration for analysis subgroups (e.g., age groups, sex, race).

## Usage

``` r
SubgroupConfig(
  source = "yaml",
  priority = 0L,
  variable = character(0),
  labels = list(),
  order = NULL,
  filter_values = NULL
)
```

## Arguments

- source:

  Character string for configuration source

- priority:

  Integer priority (higher overrides lower)

- variable:

  Character string specifying the variable name in the data

- labels:

  Named list mapping variable values to display labels

- order:

  Character vector specifying the display order

- filter_values:

  Optional character vector of values to include

## Value

A SubgroupConfig object

## Examples

``` r
if (FALSE) { # \dontrun{
config <- SubgroupConfig(
  variable = "AGEGR1",
  labels = list(Y_LT65 = "< 65", Y_GE65 = ">= 65")
)
} # }
```
