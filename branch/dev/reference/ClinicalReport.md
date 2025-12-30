# ClinicalReport Class

An S7 class representing a complete clinical study report with multiple
sections for baseline characteristics, safety, and efficacy analyses.

## Usage

``` r
ClinicalReport(
  study_id = character(0),
  study_title = character(0),
  sections = list(),
  config = NULL,
  metadata = list()
)
```

## Arguments

- study_id:

  Character string for study identifier

- study_title:

  Character string for study title

- sections:

  List of ReportSection objects

- config:

  ConfigurationRegistry object (optional)

- metadata:

  List of additional metadata

## Value

A ClinicalReport object

## Examples

``` r
if (FALSE) { # \dontrun{
report <- ClinicalReport(
  study_id = "STUDY001",
  study_title = "Phase III Study"
)
} # }
```
