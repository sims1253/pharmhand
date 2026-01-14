# Assess Subgroup Credibility Using ICEMAN Criteria

Evaluates the credibility of subgroup analyses using the ICEMAN
criteria. The 10 criteria assess whether an apparent subgroup effect is
likely to be a true effect modification.

## Usage

``` r
assess_iceman(
  subgroup_result = NULL,
  is_prespecified = FALSE,
  hypothesis_direction = c("none", "correct", "opposite"),
  n_subgroups = 1,
  biological_rationale = c("none", "weak", "moderate", "strong"),
  effect_measure = c("uncertain", "consistent", "opposite"),
  within_study = TRUE,
  statistical_test = c("none", "informal", "formal"),
  interaction_pvalue = NA_real_,
  replication = c("not_applicable", "no", "yes"),
  other_evidence = c("none", "weak", "moderate", "strong")
)
```

## Arguments

- subgroup_result:

  Subgroup analysis result (from create_subgroup_table)

- is_prespecified:

  Logical. Was the subgroup prespecified?

- hypothesis_direction:

  Character. Was direction prespecified? Options: "correct", "opposite",
  "none"

- n_subgroups:

  Integer. Total number of subgroups analyzed

- biological_rationale:

  Character. Strength of biological rationale: "strong", "moderate",
  "weak", "none"

- effect_measure:

  Character. Consistent with overall effect? "consistent", "opposite",
  "uncertain"

- within_study:

  Logical. Is this within-study comparison?

- statistical_test:

  Character. Type of interaction test used: "formal", "informal", "none"

- interaction_pvalue:

  Numeric. P-value for interaction (if tested)

- replication:

  Character. Has finding been replicated? "yes", "no", "not_applicable"

- other_evidence:

  Character. Supporting evidence from other sources: "strong",
  "moderate", "weak", "none"

## Value

ICEMANResult object with criteria assessments and overall credibility
