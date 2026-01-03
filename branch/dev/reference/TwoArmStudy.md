# TwoArmStudy Class

An S7 class for representing and analyzing two-arm clinical studies.
Inherits from Study.

## Usage

``` r
TwoArmStudy(
  study_id = character(0),
  study_title = character(0),
  design = "rct",
  population = "ITT",
  endpoints = list(),
  results = list(),
  risk_of_bias = NULL,
  metadata = list(),
  data = structure(list(), names = character(0), row.names = integer(0), class =
    "data.frame"),
  treatment_var = "TRT01P",
  comparator = ""
)
```

## Arguments

- study_id:

  Character string for study identifier

- study_title:

  Character string for study title

- design:

  Character string for study design (default: "rct")

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

- treatment_var:

  Character string for treatment variable name

- comparator:

  Character string describing the comparator arm

## Value

A TwoArmStudy object

## Examples

``` r
if (FALSE) { # \dontrun{
study <- TwoArmStudy(
  data = my_data,
  study_id = "STUDY001",
  study_title = "Phase III RCT",
  treatment_var = "TRT01P",
  comparator = "Placebo"
)
} # }
```
