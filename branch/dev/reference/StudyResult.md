# StudyResult Class

An S7 class for representing complete clinical study results containing
multiple tables and plots with methods for generating comprehensive Word
documents.

## Usage

``` r
StudyResult(
  study_id = NULL,
  study_title = NULL,
  tables = list(),
  plots = list(),
  metadata = list()
)
```

## Arguments

- study_id:

  Character string for study identifier

- study_title:

  Character string for study title

- tables:

  Named list of ClinicalTable objects

- plots:

  Named list of ClinicalPlot objects

- metadata:

  List of additional metadata

## Value

A StudyResult object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- StudyResult(
  study_id = "STUDY001",
  study_title = "Phase III Study"
)
} # }
```
