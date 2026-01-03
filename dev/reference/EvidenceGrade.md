# EvidenceGrade Class

An S7 class for representing IQWiG evidence grading results
(Beleg/Hinweis/Anhaltspunkt).

## Usage

``` r
EvidenceGrade(
  grade = character(0),
  grade_de = "",
  direction = "none",
  certainty = NA_real_,
  n_studies = NA_integer_,
  domains = list(),
  justification = "",
  metadata = list()
)
```

## Arguments

- grade:

  Character string: "proof" (Beleg), "indication" (Hinweis), "hint"
  (Anhaltspunkt), or "none" (kein Beleg)

- grade_de:

  Character string: German grade name

- direction:

  Character string: "benefit", "harm", or "none"

- certainty:

  Numeric certainty score (0-1)

- n_studies:

  Integer number of studies

- domains:

  List of domain assessments

- justification:

  Character string explaining the grade

- metadata:

  List of additional metadata

## Value

An EvidenceGrade object

## Examples

``` r
if (FALSE) { # \dontrun{
grade <- EvidenceGrade(
  grade = "indication",
  grade_de = "Hinweis",
  direction = "benefit",
  n_studies = 3L,
  justification = "Consistent results from 3 RCTs with moderate risk of bias"
)
} # }
```
