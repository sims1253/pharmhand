# MultiArmStudy Class

An S7 class for representing and analyzing multi-arm clinical studies
(3+ treatment arms). Inherits from Study.

## Usage

``` r
MultiArmStudy(
  study_id = character(0),
  study_title = character(0),
  design = "rct",
  population = "ITT",
  endpoints = list(),
  results = list(),
  risk_of_bias = NULL,
  metadata = list(),
  data = data.frame(),
  treatment_var = "TRT01P",
  arms = character(0),
  reference_arm = ""
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

- arms:

  Character vector of treatment arm names

- reference_arm:

  Character string for the reference/control arm

## Value

A MultiArmStudy object

## Examples

``` r
if (FALSE) { # \dontrun{
study <- MultiArmStudy(
  data = my_data,
  study_id = "STUDY001",
  study_title = "Phase III Multi-Arm RCT",
  treatment_var = "TRT01P",
  arms = c("Drug A", "Drug B", "Drug C", "Placebo"),
  reference_arm = "Placebo"
)
} # }
```
