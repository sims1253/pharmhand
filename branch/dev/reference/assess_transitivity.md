# Assess Transitivity for Indirect Comparisons

Evaluates the transitivity assumption by comparing population
characteristics across studies in a network. Transitivity requires that
studies are similar enough that indirect comparisons are valid.

## Usage

``` r
assess_transitivity(
  study_characteristics,
  char_vars,
  treatment_var = "treatment",
  continuous_vars = NULL,
  threshold_smd = 0.1
)
```

## Arguments

- study_characteristics:

  Data frame with study-level characteristics. Must include 'study_id'
  and 'treatment' columns, plus characteristics to compare.

- char_vars:

  Character vector. Names of characteristic variables to assess.

- treatment_var:

  Character. Name of treatment variable. Default: "treatment"

- continuous_vars:

  Character vector. Which char_vars are continuous (vs categorical)

- threshold_smd:

  Numeric. SMD threshold for imbalance. Default: 0.1

## Value

A list with components:

- summaries:

  Summary statistics by treatment

- comparison_tables:

  Pairwise comparison tables

- imbalance_scores:

  SMDs for continuous variables

- overall_assessment:

  Text summary of concerns

- n_treatments:

  Number of unique treatments

- n_characteristics:

  Number of characteristics assessed

## Examples

``` r
if (FALSE) { # \dontrun{
chars <- data.frame(
  study_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
  treatment = c("A", "B", "B", "C", "A", "C"),
  mean_age = c(55, 55, 58, 58, 52, 52),
  pct_male = c(60, 60, 65, 65, 55, 55),
  disease_stage = c("II", "II", "III", "III", "II", "II")
)

result <- assess_transitivity(
  study_characteristics = chars,
  char_vars = c("mean_age", "pct_male", "disease_stage"),
  continuous_vars = c("mean_age", "pct_male")
)
} # }
```
