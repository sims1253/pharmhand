# ReportSection Class

An S7 class representing a report section containing multiple clinical
content items (tables and plots).

## Usage

``` r
ReportSection(
  title = NULL,
  section_type = character(0),
  content = list(),
  metadata = list()
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

## Value

A ReportSection object

## Examples

``` r
if (FALSE) { # \dontrun{
section <- ReportSection(
  title = "Demographics",
  section_type = "baseline"
)
} # }
```
