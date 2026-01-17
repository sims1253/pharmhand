# Export ROBINS-I Assessment to Data Frame

Converts ROBINS-I assessment results to a tidy data frame suitable for
export to CSV or inclusion in reports.

## Usage

``` r
robins_i_to_df(result, wide_format = FALSE)
```

## Arguments

- result:

  A ROBINSIResult object.

- wide_format:

  Logical. If TRUE, returns wide format with one row per study. If
  FALSE, returns long format. Default: FALSE.

## Value

A data frame with assessment details.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- assess_robins_i(
  study_id = "OBS001",
  d1_confounding = "Serious",
  d2_selection = "Low",
  d3_classification = "Low",
  d4_deviations = "Low",
  d5_missing_data = "Low",
  d6_measurement = "Moderate",
  d7_selection_report = "Low",
  d6_support = "Outcome assessor not blinded",
  outcome = "Overall Survival"
)

# Long format (default)
long_df <- robins_i_to_df(result)
print(long_df)

# Wide format
wide_df <- robins_i_to_df(result, wide_format = TRUE)
print(wide_df)
} # }
```
