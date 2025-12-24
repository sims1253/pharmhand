# SubgroupSection Class

An S7 class for representing subgroup analysis sections in clinical
study reports.

## Usage

``` r
SubgroupSection(
  title = NULL,
  section_type = character(0),
  content = list(),
  metadata = list(),
  subgroup_var = character(0),
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

- subgroup_var:

  Character string for subgroup variable name

- group_var:

  Character string for treatment group variable

## Value

A SubgroupSection object

## Examples

``` r
if (FALSE) { # \dontrun{
section <- SubgroupSection(
  title = "Subgroup Analysis",
  subgroup_var = "AGEGR1",
  group_var = "TRT"
)
} # }
```
