# RoB2Result S7 Class

S7 class for storing RoB 2 assessment results for a single study.

## Usage

``` r
RoB2Result(
  study_id = character(0),
  domains = list(),
  overall = character(0),
  overall_justification = "",
  outcome = "",
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

- metadata:

  List. Additional metadata (optional).

## Value

A RoB2Result object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- RoB2Result(
  study_id = "STUDY001",
  domains = list(
    D1_randomization = list(
      judgment = "Low", support = "Allocation concealed"
    ),
    D2_deviations = list(judgment = "Low", support = "No deviations"),
    D3_missing_data = list(judgment = "Low", support = "No missing data"),
    D4_measurement = list(
      judgment = "Some concerns", support = "Blinded assessment"
    ),
    D5_selection = list(judgment = "Low", support = "Pre-specified analysis")
  ),
  overall = "Some concerns",
  overall_justification = "One domain rated as Some concerns",
  outcome = "OS"
)
} # }
```
