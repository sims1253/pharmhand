# ROBINSIResult S7 Class

S7 class for storing ROBINS-I assessment results for a single study.

## Usage

``` r
ROBINSIResult(
  study_id = character(0),
  domains = list(),
  overall = character(0),
  overall_justification = "",
  outcome = "",
  intervention = "",
  comparator = "",
  metadata = list()
)
```

## Arguments

- study_id:

  Character. Study identifier.

- domains:

  List. Named list of domain judgments with supporting text.

- overall:

  Character. Overall risk of bias judgment.

- overall_justification:

  Character. Justification for overall judgment.

- outcome:

  Character. Outcome being assessed (optional).

- intervention:

  Character. Intervention being assessed (optional).

- comparator:

  Character. Comparator intervention (optional).

- metadata:

  List. Additional metadata (optional).

## Value

A ROBINSIResult object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ROBINSIResult(
  study_id = "OBS001",
  domains = list(
    D1_confounding = list(
      judgment = "Serious", support = "No adjustment for confounders"
    ),
    D2_selection = list(judgment = "Low", support = "Appropriate selection"),
    D3_classification = list(
      judgment = "Low", support = "Intervention clearly defined"
    ),
    D4_deviations = list(
      judgment = "Moderate", support = "Some deviations noted"
    ),
    D5_missing_data = list(judgment = "Low", support = "No missing data"),
    D6_measurement = list(
      judgment = "Moderate", support = "Blinded outcome assessment"
    ),
    D7_selection_report = list(
      judgment = "Low", support = "Pre-specified outcomes reported"
    )
  ),
  overall = "Serious",
  overall_justification = "D1 rated Serious due to unadjusted confounding",
  outcome = "Mortality",
  intervention = "Drug A",
  comparator = "Standard of Care"
)
} # }
```
