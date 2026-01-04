# Meta-Analysis Functions

Functions for performing meta-analyses and evidence synthesis.

Conducts fixed-effect or random-effects meta-analysis on a set of
studies. Supports binary, continuous, and time-to-event outcomes.

## Usage

``` r
meta_analysis(
  data = NULL,
  yi = NULL,
  sei = NULL,
  ni = NULL,
  study_labels = NULL,
  effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
  model = c("random", "fixed"),
  method = c("REML", "DL", "PM", "ML"),
  knapp_hartung = TRUE,
  conf_level = 0.95,
  prediction = TRUE
)
```

## Arguments

- data:

  Data frame with study-level summary statistics OR a StudySet object

- yi:

  Numeric vector of effect estimates (log scale for ratios)

- sei:

  Numeric vector of standard errors

- ni:

  Numeric vector of sample sizes (optional)

- study_labels:

  Character vector of study names

- effect_measure:

  Character. Type of effect: "hr", "or", "rr", "rd", "md", "smd"

- model:

  Character. "fixed" or "random". Default: "random"

- method:

  Character. Estimation method for random effects: "DL"
  (DerSimonian-Laird), "REML", "PM" (Paule-Mandel), "ML". Default:
  "REML"

- knapp_hartung:

  Logical. Apply Knapp-Hartung adjustment. Default: TRUE

- conf_level:

  Numeric. Confidence level. Default: 0.95

- prediction:

  Logical. Calculate prediction interval. Default: TRUE

## Value

A MetaResult S7 object with pooled estimate and heterogeneity stats

## Examples

``` r
# Random-effects meta-analysis of 5 studies with hazard ratios
result <- meta_analysis(
  yi = log(c(0.75, 0.82, 0.68, 0.91, 0.77)),
  sei = c(0.12, 0.15, 0.18, 0.14, 0.11),
  study_labels = paste("Study", 1:5),
  effect_measure = "hr",
  model = "random",
  method = "REML",
  knapp_hartung = TRUE
)
result@estimate
#> [1] 0.7823178
result@ci
#> [1] 0.6840484 0.8947045
result@heterogeneity$I2
#> [1] 0
```
