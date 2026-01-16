# Create Traffic Light Plot for Risk of Bias

Creates a traffic light plot showing domain-level risk of bias judgments
for each study. Works with both RoB 2 and ROBINS-I assessment results.

## Usage

``` r
create_rob_traffic_light_plot(
  results,
  title = "Risk of Bias Assessment",
  show_overall = TRUE,
  base_size = 11,
  colors = NULL
)
```

## Arguments

- results:

  List of RoB2Result or ROBINSIResult objects

- title:

  Plot title. Default: "Risk of Bias Assessment"

- show_overall:

  Logical. Include overall judgment column. Default: TRUE

- base_size:

  Base font size. Default: 11

- colors:

  Named character vector of colors for judgments. If NULL, uses the
  standard .ROB_COLORS palette. Default: NULL

## Value

A ClinicalPlot object containing the traffic light plot

## Details

The traffic light plot displays individual study assessments with
colored squares for each domain, following the robvis convention:

- Green: Low risk

- Yellow: Some concerns (RoB 2) / Moderate (ROBINS-I)

- Orange: Serious (ROBINS-I)

- Red: High risk (RoB 2) / Critical (ROBINS-I)

- Gray: No information

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
    outcome = "Overall Survival"
  ),
  assess_rob2(
    study_id = "STUDY002",
    d1_randomization = "Low",
    d2_deviations = "Low",
    d3_missing_data = "High",
    d4_measurement = "Low",
    d5_selection = "Low",
    outcome = "Overall Survival"
  )
)

plot <- create_rob_traffic_light_plot(rob2_results)
plot(plot)

# ROBINS-I example
robinsi_results <- list(
  assess_robins_i(
    study_id = "OBS001",
    d1_confounding = "Serious",
    d2_selection = "Low",
    d3_classification = "Low",
    d4_deviations = "Low",
    d5_missing_data = "Low",
    d6_measurement = "Moderate",
    d7_selection_report = "Low",
    outcome = "Mortality"
  )
)

plot <- create_rob_traffic_light_plot(robinsi_results)
} # }
```
