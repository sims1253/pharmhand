# Assess Multiple Studies with ROBINS-I

Performs ROBINS-I risk of bias assessment for multiple non-randomized
studies from a data frame.

## Usage

``` r
assess_robins_i_batch(data, .suppress_messages = FALSE)
```

## Arguments

- data:

  Data frame with columns for study_id, domain judgments, and optionally
  supporting text. Required columns:

  - study_id: Study identifier

  - d1_confounding: Domain 1 judgment

  - d2_selection: Domain 2 judgment

  - d3_classification: Domain 3 judgment

  - d4_deviations: Domain 4 judgment

  - d5_missing_data: Domain 5 judgment

  - d6_measurement: Domain 6 judgment

  - d7_selection_report: Domain 7 judgment

  Optional columns: d1_support, d2_support, d3_support, d4_support,
  d5_support, d6_support, d7_support, outcome, intervention, comparator,
  overall, overall_justification

- .suppress_messages:

  Logical. If TRUE, suppresses individual assessment messages. Default:
  FALSE

## Value

A list of ROBINSIResult objects, named by study_id.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create data frame with study assessments
robins_data <- data.frame(
  study_id = c("OBS001", "OBS002", "OBS003"),
  d1_confounding = c("Serious", "Moderate", "Low"),
  d2_selection = c("Low", "Low", "Low"),
  d3_classification = c("Low", "Low", "Low"),
  d4_deviations = c("Low", "Moderate", "Low"),
  d5_missing_data = c("Low", "Low", "Low"),
  d6_measurement = c("Moderate", "Low", "Low"),
  d7_selection_report = c("Low", "Low", "Low"),
  d1_support = c("No adjustment for confounders", "", ""),
  outcome = c("OS", "PFS", "ORR"),
  stringsAsFactors = FALSE
)

results <- assess_robins_i_batch(robins_data)
results[["OBS001"]]@overall

# Get summary of all assessments
summary_df <- do.call(rbind, lapply(results, function(r) r@summary_df))
} # }
```
