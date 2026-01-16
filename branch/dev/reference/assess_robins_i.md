# Assess Risk of Bias using ROBINS-I

Assesses the risk of bias for a single non-randomized study using the
ROBINS-I tool. The overall judgment is automatically calculated based on
domain judgments using the following algorithm:

- "Low": All domains rated "Low"

- "Moderate": At least one "Moderate" but no "Serious" or "Critical"

- "Serious": At least one "Serious" but no "Critical"

- "Critical": Any domain rated "Critical"

- "No information": At least one "No information" with all others "Low"
  or "Moderate"

## Usage

``` r
assess_robins_i(
  study_id,
  d1_confounding,
  d2_selection,
  d3_classification,
  d4_deviations,
  d5_missing_data,
  d6_measurement,
  d7_selection_report,
  d1_support = "",
  d2_support = "",
  d3_support = "",
  d4_support = "",
  d5_support = "",
  d6_support = "",
  d7_support = "",
  outcome = "",
  intervention = "",
  comparator = "",
  overall = NULL,
  overall_justification = NULL,
  metadata = list()
)
```

## Arguments

- study_id:

  Character. Study identifier.

- d1_confounding:

  Character. Domain 1 judgment: "Low", "Moderate", "Serious",
  "Critical", or "No information". For confounding.

- d2_selection:

  Character. Domain 2 judgment: "Low", "Moderate", "Serious",
  "Critical", or "No information". For selection of participants.

- d3_classification:

  Character. Domain 3 judgment: "Low", "Moderate", "Serious",
  "Critical", or "No information". For classification of interventions.

- d4_deviations:

  Character. Domain 4 judgment: "Low", "Moderate", "Serious",
  "Critical", or "No information". For deviations from interventions.

- d5_missing_data:

  Character. Domain 5 judgment: "Low", "Moderate", "Serious",
  "Critical", or "No information". For missing data.

- d6_measurement:

  Character. Domain 6 judgment: "Low", "Moderate", "Serious",
  "Critical", or "No information". For measurement of outcomes.

- d7_selection_report:

  Character. Domain 7 judgment: "Low", "Moderate", "Serious",
  "Critical", or "No information". For selection of reported result.

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

- d6_support:

  Character. Supporting text for Domain 6 (optional).

- d7_support:

  Character. Supporting text for Domain 7 (optional).

- outcome:

  Character. Outcome being assessed (optional).

- intervention:

  Character. Intervention being assessed (optional).

- comparator:

  Character. Comparator intervention (optional).

- overall:

  Character. Overall judgment. If NULL, auto-calculated.

- overall_justification:

  Character. Justification for overall judgment. If NULL and overall is
  auto-calculated, a default justification is generated.

- metadata:

  List. Additional metadata (optional).

## Value

A ROBINSIResult object with the assessment results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic assessment with auto-calculated overall
result <- assess_robins_i(
  study_id = "OBS001",
  d1_confounding = "Serious",
  d2_selection = "Low",
  d3_classification = "Low",
  d4_deviations = "Low",
  d5_missing_data = "Low",
  d6_measurement = "Moderate",
  d7_selection_report = "Low",
  d6_support = "Outcome assessor not fully blinded",
  outcome = "Overall Survival",
  intervention = "Drug A",
  comparator = "Standard of Care"
)
result@overall
result@judgment_counts

# Assessment with all Low risk
result2 <- assess_robins_i(
  study_id = "OBS002",
  d1_confounding = "Low",
  d2_selection = "Low",
  d3_classification = "Low",
  d4_deviations = "Low",
  d5_missing_data = "Low",
  d6_measurement = "Low",
  d7_selection_report = "Low",
  outcome = "Response Rate"
)
} # }
```
