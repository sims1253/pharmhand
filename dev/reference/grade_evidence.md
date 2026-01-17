# Grade Evidence According to IQWiG Criteria

Assigns an evidence grade (Beleg/Hinweis/Anhaltspunkt/Kein Beleg) based
on study quality, risk of bias, effect size, precision, heterogeneity,
and consistency across studies. This implements the IQWiG evidence
grading system as described in IQWiG Allgemeine Methoden Version 3.1.4,
p.51-56.

## Usage

``` r
grade_evidence(
  meta_result,
  rob_results = NULL,
  n_studies_override = NULL,
  direction = "none",
  ci_level = 0.95,
  publication_bias = NULL,
  indirectness = NULL,
  metadata = list()
)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis() containing pooled effect
  estimate, confidence interval, p-value, and heterogeneity statistics.
  Alternatively, a ComparisonResult object for single-study assessments.

- rob_results:

  A list of RoB2Result objects (one per study), or a single RoB2Result
  for single-study assessment. If NULL, defaults to "Low" risk of bias
  for all studies.

- n_studies_override:

  Integer. Override the number of studies for single study assessment.
  Default: NULL (use meta_result@n or 1 for ComparisonResult).

- direction:

  Character. Direction of effect: "benefit", "harm", or "none". Default:
  "none" (effect direction determined from effect estimate).

- ci_level:

  Numeric. Confidence level for significance assessment. Default: 0.95.

- publication_bias:

  List or NULL. Publication bias assessment from trim_and_fill() or
  egger's_test(). If provided, used in grading. Expected list elements:
  p_value, adjusted_estimates.

- indirectness:

  Numeric or NULL. Indirectness score (0-1, where 1 = no indirectness).
  Default: NULL (assumed 1 = no indirectness).

- metadata:

  List. Additional metadata for the EvidenceGrade object. Default: empty
  list.

## Value

An EvidenceGrade object with grade, justification, and domain
assessments populated.

## Examples

``` r
if (FALSE) { # \dontrun{
# Meta-analysis with 5 RCTs, all low RoB
meta_res <- MetaResult(
  estimate = 0.72,
  ci = c(0.62, 0.84),
  p_value = 0.0001,
  n = 5L,
  effect_measure = "hr",
  heterogeneity = list(I2 = 25, tau2 = 0.01),
  method = "REML meta-analysis"
)

# Create RoB results for all studies (all low)
rob_results <- list(
  assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study2", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study3", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study4", "Low", "Low", "Low", "Low", "Low"),
  assess_rob2("Study5", "Low", "Low", "Low", "Low", "Low")
)

# Grade the evidence
grade <- grade_evidence(meta_res, rob_results, direction = "benefit")
grade@grade
grade@grade_de

# Single study assessment
comp_res <- ComparisonResult(
  estimate = 0.75,
  ci = c(0.60, 0.93),
  p_value = 0.008,
  effect_measure = "hr"
)
single_grade <- grade_evidence(
  comp_res,
  rob_results = assess_rob2("Study1", "Low", "Low", "Low", "Low", "Low"),
  n_studies_override = 1L
)
} # }
```
