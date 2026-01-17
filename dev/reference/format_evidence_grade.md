# Format Evidence Grade for Display

Formats an EvidenceGrade object for display in reports, converting
between German and English terminology as requested.

## Usage

``` r
format_evidence_grade(
  grade,
  language = c("en", "de"),
  include_details = FALSE,
  format = c("text", "html", "latex")
)
```

## Arguments

- grade:

  An EvidenceGrade object.

- language:

  Character. Output language: "en" for English, "de" for German.
  Default: "en".

- include_details:

  Logical. Include detailed justification. Default: FALSE.

- format:

  Character. Output format: "text", "html", or "latex". Default: "text".

## Value

A character string with the formatted grade.

## Examples

``` r
if (FALSE) { # \dontrun{
grade <- EvidenceGrade(
  grade = "indication",
  grade_de = "Hinweis",
  direction = "benefit",
  n_studies = 5L,
  justification = "Consistent results from 5 studies"
)

# English output
format_evidence_grade(grade, language = "en")

# German output with details
format_evidence_grade(grade, language = "de", include_details = TRUE)

# HTML format
format_evidence_grade(grade, format = "html")
} # }
```
