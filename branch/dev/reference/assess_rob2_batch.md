# Assess Multiple Studies with RoB 2

Performs RoB 2 risk of bias assessment for multiple studies from a data
frame.

## Usage

``` r
assess_rob2_batch(data, .suppress_messages = FALSE)
```

## Arguments

- data:

  Data frame with columns for study_id, domain judgments, and optionally
  supporting text. Required columns:

  - study_id: Study identifier

  - d1_randomization: Domain 1 judgment

  - d2_deviations: Domain 2 judgment

  - d3_missing_data: Domain 3 judgment

  - d4_measurement: Domain 4 judgment

  - d5_selection: Domain 5 judgment

  Optional columns: d1_support, d2_support, d3_support, d4_support,
  d5_support, outcome, overall, overall_justification

- .suppress_messages:

  Logical. If TRUE, suppresses individual assessment messages. Default:
  FALSE

## Value

A list of RoB2Result objects, named by study_id.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create data frame with study assessments
rob_data <- data.frame(
  study_id = c("STUDY001", "STUDY002", "STUDY003"),
  d1_randomization = c("Low", "Low", "High"),
  d2_deviations = c("Low", "Some concerns", "Low"),
  d3_missing_data = c("Low", "Low", "Low"),
  d4_measurement = c("Some concerns", "Low", "Low"),
  d5_selection = c("Low", "Low", "Low"),
  d4_support = c("Unblinded assessment", "", ""),
  outcome = c("OS", "PFS", "OS"),
  stringsAsFactors = FALSE
)

results <- assess_rob2_batch(rob_data)
results[["STUDY001"]]@overall

# Get summary of all assessments
summary_df <- do.call(rbind, lapply(results, function(r) r@summary_df))
} # }
```
