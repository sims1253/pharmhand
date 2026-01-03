# SingleArmStudy Class

An S7 class for representing and analyzing single-arm clinical studies.
Inherits from Study.

## Usage

``` r
SingleArmStudy(
  study_id = character(0),
  study_title = character(0),
  design = "single-arm",
  population = "ITT",
  endpoints = list(),
  results = list(),
  risk_of_bias = NULL,
  metadata = list(),
  data = structure(list(), names = character(0), row.names = integer(0), class =
    "data.frame")
)
```

## Arguments

- study_id:

  Character string for study identifier

- study_title:

  Character string for study title

- design:

  Character string for study design (default: "single-arm")

- population:

  Character string for population

- endpoints:

  List of Endpoint objects

- results:

  List of analysis results

- risk_of_bias:

  Risk of bias assessment result

- metadata:

  List of additional metadata

- data:

  A data frame containing the study data

## Value

A SingleArmStudy object

## Examples

``` r
if (FALSE) { # \dontrun{
study <- SingleArmStudy(
  data = my_data,
  study_id = "STUDY001",
  study_title = "Phase II Study"
)
} # }
```
