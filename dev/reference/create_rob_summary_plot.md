# Create Summary Plot for Risk of Bias

Creates a stacked bar plot showing the proportion of studies at each
risk level for each domain. This provides an overview of risk of bias
distribution across the evidence base.

## Usage

``` r
create_rob_summary_plot(
  results,
  title = "Risk of Bias Summary",
  weighted = FALSE,
  base_size = 11,
  colors = NULL,
  horizontal = TRUE
)
```

## Arguments

- results:

  List of RoB2Result or ROBINSIResult objects

- title:

  Plot title. Default: "Risk of Bias Summary"

- weighted:

  Logical. Weight by sample size if available in metadata. Currently not
  implemented - included for future extension. Default: FALSE

- base_size:

  Base font size. Default: 11

- colors:

  Named character vector of colors for judgments. If NULL, uses the
  standard .ROB_COLORS palette. Default: NULL

- horizontal:

  Logical. Display bars horizontally (domains on y-axis). Default: TRUE

## Value

A ClinicalPlot object containing the summary plot

## Examples

``` r
if (FALSE) { # \dontrun{
# RoB 2 example
rob2_results <- list(
  assess_rob2(
    study_id = "STUDY001",
    d1_randomization = "Low",
    d2_deviations = "Low",
    d3_missing_data = "Low",
    d4_measurement = "Some concerns",
    d5_selection = "Low",
    outcome = "OS"
  ),
  assess_rob2(
    study_id = "STUDY002",
    d1_randomization = "Low",
    d2_deviations = "Low",
    d3_missing_data = "High",
    d4_measurement = "Low",
    d5_selection = "Low",
    outcome = "OS"
  ),
  assess_rob2(
    study_id = "STUDY003",
    d1_randomization = "Some concerns",
    d2_deviations = "Some concerns",
    d3_missing_data = "Low",
    d4_measurement = "Low",
    d5_selection = "Low",
    outcome = "PFS"
  )
)

plot <- create_rob_summary_plot(rob2_results)
plot(plot)
} # }
```
