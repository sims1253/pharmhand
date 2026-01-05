# PTConfig Class

Configuration for Preferred Term (PT) handling in adverse events.

## Usage

``` r
PTConfig(
  source = "yaml",
  priority = 0L,
  variable = character(0),
  include_all = TRUE,
  sort_by = "frequency",
  min_subjects = 1,
  top_n_per_soc = NULL,
  show_pt_codes = FALSE
)
```

## Arguments

- source:

  Character string for configuration source

- priority:

  Integer priority

- variable:

  Character string for PT variable name (e.g., "AEDECOD")

- include_all:

  Logical, include all PTs found in data

- sort_by:

  Character string for sorting ("frequency" or "alphabetical")

- min_subjects:

  Numeric minimum subjects required to include PT

- top_n_per_soc:

  Numeric maximum PTs per SOC (NULL = no limit)

- show_pt_codes:

  Logical, show PT codes alongside names

## Value

A PTConfig object

## Examples

``` r
if (FALSE) { # \dontrun{
config <- PTConfig(
  variable = "AEDECOD",
  sort_by = "frequency"
)
} # }
```
