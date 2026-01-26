# Create Evidence Summary Table

Creates a formatted evidence summary table displaying study results,
effect estimates, heterogeneity statistics, risk of bias assessments,
and evidence grades for multiple endpoints.

## Usage

``` r
create_evidence_summary_table(
  data,
  title = "Evidence Summary",
  columns = c("Endpoint", "N Studies", "Effect (95% CI)", "p-value", "I2", "RoB",
    "Grade"),
  conf_level = 0.95,
  language = c("en", "de"),
  footnotes = character(),
  ...
)
```

## Arguments

- data:

  List of named elements containing endpoint data. Each element should
  be a list with components:

  - `result`: MetaResult or ComparisonResult object with effect
    estimate, confidence interval, p-value, and study count

  - `grade`: EvidenceGrade object with evidence assessment

  - `rob`: RoB2Result object or list of RoB2Result objects for risk of
    bias (optional, can be derived from grade)

  - `label`: Character string for endpoint display name (optional)

- title:

  Table title (default: "Evidence Summary")

- columns:

  Character vector of column names to include. Default columns:
  "Endpoint", "N Studies", "Effect (95% CI)", "p-value", "I2", "RoB",
  "Grade".

- conf_level:

  Numeric confidence level for CIs (default: 0.95)

- language:

  Output language: "en" for English, "de" for German. Default: "en".

- footnotes:

  Character vector of footnotes to add to the table.

- ...:

  Additional arguments passed to
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md),
  such as `col_widths`, `autofit`, `theme`, etc.

## Value

A ClinicalTable object containing the formatted evidence summary table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with meta-analysis results
endpoints <- list(
  "Overall Survival" = list(
    result = MetaResult(
      estimate = 0.75,
      ci = c(0.60, 0.94),
      p_value = 0.012,
      n = 3L,
      effect_measure = "hr",
      heterogeneity = list(I2 = 25.0, tau2 = 0.01)
    ),
    grade = EvidenceGrade(
      grade = "indication",
      grade_de = "Hinweis",
      direction = "benefit",
      n_studies = 3L
    ),
    rob = list(
      # Additional RoB2Result objects with required arguments would go here
      # e.g., RoB2Result(study_id = "Study1", outcome = "...", overall = "...", domains = list(...))
      # e.g., RoB2Result(study_id = "Study2", outcome = "...", overall = "...", domains = list(...))
    )
  ),
  "Progression-Free Survival" = list(
    result = MetaResult(
      estimate = 0.68,
      ci = c(0.52, 0.89),
      p_value = 0.005,
      n = 3L,
      effect_measure = "hr",
      heterogeneity = list(I2 = 45.0, tau2 = 0.03)
    ),
    grade = EvidenceGrade(
      grade = "proof",
      grade_de = "Beleg",
      direction = "benefit",
      n_studies = 3L
    )
  )
)

table <- create_evidence_summary_table(endpoints)
} # }
```
