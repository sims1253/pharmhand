# Export Risk of Bias Data to Tidy Format

Converts a list of RoB2Result or ROBINSIResult objects to a tidy data
frame suitable for further analysis or export.

## Usage

``` r
rob_data_to_tidy(results, wide_format = FALSE)
```

## Arguments

- results:

  List of RoB2Result or ROBINSIResult objects

- wide_format:

  Logical. If TRUE, returns wide format with one row per study and
  columns for each domain judgment. If FALSE, returns long format with
  one row per domain per study. Default: FALSE

## Value

A data frame with risk of bias assessment data

## Examples

``` r
if (FALSE) { # \dontrun{
results <- list(
  assess_rob2(
    study_id = "STUDY001",
    d1_randomization = "Low",
    d2_deviations = "Low",
    d3_missing_data = "Low",
    d4_measurement = "Some concerns",
    d5_selection = "Low",
    outcome = "OS"
  )
)

# Long format (default)
long_df <- rob_data_to_tidy(results)
print(long_df)

# Wide format
wide_df <- rob_data_to_tidy(results, wide_format = TRUE)
print(wide_df)
} # }
```
