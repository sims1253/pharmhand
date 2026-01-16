# Generate Full Evidence Report

Creates a comprehensive Word document with evidence narratives for
multiple endpoints, including overall results, individual endpoint
sections, and conclusions.

## Usage

``` r
generate_full_evidence_report(
  endpoints,
  title = "Evidence Report",
  subtitle = NULL,
  author = NULL,
  date = format(Sys.Date(), "%d %B %Y"),
  template = "iqwig",
  language = c("en", "de"),
  ci_level = 0.95
)
```

## Arguments

- endpoints:

  List or data frame containing endpoint data. Each element should have:
  endpoint (name), result (MetaResult/ComparisonResult), evidence_grade
  (EvidenceGrade, optional), rob_results (RoB2Result, optional),
  n_patients (integer).

- title:

  Character. Report title.

- subtitle:

  Character. Report subtitle (optional).

- author:

  Character. Author name (optional).

- date:

  Character. Report date (optional, defaults to current date).

- template:

  Character. Narrative template. Default: "iqwig".

- language:

  Character. Output language. Default: "en".

- ci_level:

  Numeric. Confidence level. Default: 0.95.

## Value

An officer rdocx object ready for writing to Word.

## Examples

``` r
if (FALSE) { # \dontrun{
# Prepare endpoint data
endpoints <- list(
  list(
    endpoint = "Overall Survival",
    result = meta_os,
    evidence_grade = grade_os,
    n_patients = 2125
  ),
  list(
    endpoint = "Progression-Free Survival",
    result = meta_pfs,
    evidence_grade = grade_pfs,
    n_patients = 2100
  )
)

# Generate report
doc <- generate_full_evidence_report(
  endpoints = endpoints,
  title = "Clinical Study Report - Efficacy Analysis",
  subtitle = "Study ABC-123",
  author = "Clinical Development",
  language = "en"
)

# Write to Word file
print(doc, target = "efficacy_report.docx")

# German IQWiG report
doc_de <- generate_full_evidence_report(
  endpoints = endpoints_de,
  title = "Nutzenbewertung - Wirksamkeitsanalyse",
  author = "Medizinische Abteilung",
  language = "de"
)
print(doc_de, target = "nutzenbewertung.docx")
} # }
```
