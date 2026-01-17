# Generate Endpoint-Specific Narrative

Creates a formatted paragraph for a single endpoint with all available
data including meta-analysis results, risk of bias, and evidence
grading.

## Usage

``` r
generate_endpoint_narrative(
  endpoint,
  result,
  evidence_grade = NULL,
  rob_results = NULL,
  n_patients = NULL,
  template = "iqwig",
  language = c("en", "de"),
  ci_level = 0.95
)
```

## Arguments

- endpoint:

  Character. Name of the endpoint.

- result:

  A MetaResult or ComparisonResult object.

- evidence_grade:

  An EvidenceGrade object (optional).

- rob_results:

  List of RoB2Result objects or single RoB2Result (optional).

- n_patients:

  Integer. Total number of patients.

- template:

  Character. Template name. Default: "iqwig".

- language:

  Character. Output language. Default: "en".

- ci_level:

  Numeric. Confidence level. Default: 0.95.

## Value

Character string with formatted endpoint narrative.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate endpoint narrative
meta_res <- MetaResult(
  estimate = 0.80,
  ci = c(0.70, 0.91),
  p_value = 0.001,
  n = 5L,
  effect_measure = "hr",
  heterogeneity = list(I2 = 32, tau2 = 0.015)
)

grade <- EvidenceGrade(
  grade = "proof",
  grade_de = "Beleg",
  direction = "benefit",
  n_studies = 5L
)

# German IQWiG style
narrative <- generate_endpoint_narrative(
  endpoint = "Gesamt\u00fcberleben",
  result = meta_res,
  evidence_grade = grade,
  n_patients = 2125,
  language = "de"
)
cat(narrative)

# English clinical style
narrative_en <- generate_endpoint_narrative(
  endpoint = "Overall Survival",
  result = meta_res,
  evidence_grade = grade,
  n_patients = 2125,
  template = "clinical"
)
} # }
```
