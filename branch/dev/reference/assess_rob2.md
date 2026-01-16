# Assess Risk of Bias using RoB 2

Assesses the risk of bias for a single study using the Cochrane RoB 2
tool. The overall judgment is automatically calculated based on domain
judgments using the following rules:

- "Low": All domains rated "Low"

- "High": Any domain rated "High" OR 2+ domains rated "Some concerns"

- "Some concerns": All other combinations

## Usage

``` r
assess_rob2(
  study_id,
  d1_randomization,
  d2_deviations,
  d3_missing_data,
  d4_measurement,
  d5_selection,
  d1_support = "",
  d2_support = "",
  d3_support = "",
  d4_support = "",
  d5_support = "",
  outcome = "",
  overall = NULL,
  overall_justification = NULL,
  metadata = list()
)
```

## Arguments

- study_id:

  Character. Study identifier.

- d1_randomization:

  Character. Domain 1 judgment: "Low", "Some concerns", or "High". For
  randomization process.

- d2_deviations:

  Character. Domain 2 judgment: "Low", "Some concerns", or "High". For
  deviations from intended interventions.

- d3_missing_data:

  Character. Domain 3 judgment: "Low", "Some concerns", or "High". For
  missing outcome data.

- d4_measurement:

  Character. Domain 4 judgment: "Low", "Some concerns", or "High". For
  measurement of the outcome.

- d5_selection:

  Character. Domain 5 judgment: "Low", "Some concerns", or "High". For
  selection of the reported result.

- d1_support:

  Character. Supporting text for Domain 1 (optional).

- d2_support:

  Character. Supporting text for Domain 2 (optional).

- d3_support:

  Character. Supporting text for Domain 3 (optional).

- d4_support:

  Character. Supporting text for Domain 4 (optional).

- d5_support:

  Character. Supporting text for Domain 5 (optional).

- outcome:

  Character. Outcome being assessed (optional).

- overall:

  Character. Overall judgment. If NULL, auto-calculated.

- overall_justification:

  Character. Justification for overall judgment. If NULL and overall is
  auto-calculated, a default justification is generated.

- metadata:

  List. Additional metadata (optional).

## Value

A RoB2Result object with the assessment results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic assessment with auto-calculated overall
result <- assess_rob2(
  study_id = "STUDY001",
  d1_randomization = "Low",
  d2_deviations = "Low",
  d3_missing_data = "Low",
  d4_measurement = "Some concerns",
  d5_selection = "Low",
  d4_support = "Outcome assessor not blinded",
  outcome = "Overall Survival"
)
result@overall
result@judgment_counts

# Manual overall judgment
result2 <- assess_rob2(
  study_id = "STUDY002",
  d1_randomization = "High",
  d2_deviations = "Low",
  d3_missing_data = "Low",
  d4_measurement = "Low",
  d5_selection = "Low",
  overall = "High",
  overall_justification = "Inadequate randomization procedure"
)
} # }
```
