# Study Class (Abstract Base)

Abstract base class for clinical studies. Provides common properties
shared by all study types.

## Usage

``` r
Study(
  study_id = character(0),
  study_title = character(0),
  design = "rct",
  population = "ITT",
  endpoints = list(),
  results = list(),
  risk_of_bias = NULL,
  metadata = list()
)
```

## Arguments

- study_id:

  Character string for study identifier

- study_title:

  Character string for study title

- design:

  Character string for study design: "rct", "observational",
  "single-arm"

- population:

  Character string for population (e.g., "ITT", "FAS", "PP")

- endpoints:

  List of Endpoint objects

- results:

  List of analysis results

- risk_of_bias:

  Risk of bias assessment result (optional)

- metadata:

  List of additional metadata

## Value

A Study object
