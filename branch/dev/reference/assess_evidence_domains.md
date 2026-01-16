# Assess Individual IQWiG Evidence Domains

Evaluates each of the five GRADE/IQWiG evidence domains: study
limitations (risk of bias), inconsistency, imprecision, indirectness,
and publication bias.

## Usage

``` r
assess_evidence_domains(
  estimate,
  ci,
  p_value,
  heterogeneity,
  rob_results = NULL,
  n_studies,
  publication_bias = NULL,
  indirectness = NULL,
  ci_level = 0.95,
  effect_measure = "hr"
)
```

## Arguments

- estimate:

  Numeric. Pooled effect estimate.

- ci:

  Numeric vector of length 2: c(lower, upper) confidence interval.

- p_value:

  Numeric. P-value for the effect estimate.

- heterogeneity:

  List. Heterogeneity statistics from meta-analysis. Should contain I2,
  tau2, and optionally Q_pvalue.

- rob_results:

  List of RoB2Result objects or NULL. Risk of bias assessments for each
  study.

- n_studies:

  Integer. Number of studies contributing to the evidence.

- publication_bias:

  List or NULL. Publication bias assessment.

- indirectness:

  Numeric or NULL. Indirectness score (0-1).

- ci_level:

  Numeric. Confidence level for imprecision assessment. Default: 0.95.

- effect_measure:

  Character. Type of effect measure (hr, or, rr, etc.). Default: "hr".

## Value

A list with five domain assessments, each containing:

- level:

  Character: "low", "some_concerns", "high", or "unknown"

- rating:

  Numeric rating (0-1, lower is worse)

- notes:

  Character notes explaining the assessment

## Examples

``` r
if (FALSE) { # \dontrun{
domains <- assess_evidence_domains(
  estimate = 0.72,
  ci = c(0.62, 0.84),
  p_value = 0.0001,
  heterogeneity = list(I2 = 25, tau2 = 0.01),
  n_studies = 5L
)
domains$limitations
domains$inconsistency
} # }
```
