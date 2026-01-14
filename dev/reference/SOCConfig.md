# SOCConfig Class

Configuration for System Organ Class (SOC) handling in adverse events.

## Usage

``` r
SOCConfig(
  source = "yaml",
  priority = 0L,
  variable = character(0),
  include_all = TRUE,
  custom_order = NULL,
  sort_by = "frequency",
  min_subjects = 1,
  top_n = NULL
)
```

## Arguments

- source:

  Character string for configuration source

- priority:

  Integer priority

- variable:

  Character string for SOC variable name (e.g., "AEBODSYS")

- include_all:

  Logical, include all SOCs found in data

- custom_order:

  Character vector for custom SOC ordering

- sort_by:

  Character string for sorting ("frequency" or "alphabetical")

- min_subjects:

  Numeric minimum subjects required to include SOC

- top_n:

  Numeric maximum number of SOCs (NULL = no limit)

## Value

An SOCConfig object

## Examples

``` r
if (FALSE) { # \dontrun{
config <- SOCConfig(
  variable = "AEBODSYS",
  sort_by = "frequency"
)
} # }
```
