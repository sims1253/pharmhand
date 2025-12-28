# PopulationSection Class

An S7 class for representing population analysis sections in clinical
study reports.

## Usage

``` r
PopulationSection(
  title = NULL,
  section_type = character(0),
  content = list(),
  metadata = list(),
  pop_var = character(0),
  group_var = character(0)
)
```

## Arguments

- title:

  Character string for section title

- section_type:

  Character string for section type

- content:

  List of ClinicalContent objects

- metadata:

  List of additional metadata

- pop_var:

  Character string for population variable name

- group_var:

  Character string for treatment group variable

## Value

A PopulationSection object

## Examples

``` r
if (FALSE) { # \dontrun{
section <- PopulationSection(
  title = "Population Analysis",
  pop_var = "FASFL",
  group_var = "TRT"
)
} # }
```
