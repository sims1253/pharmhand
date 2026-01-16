# Create Evidence Summary Table

Generates a summary table of evidence grades for multiple outcomes or
endpoints.

## Usage

``` r
evidence_summary_table(grades, outcomes = NULL, language = c("en", "de"))
```

## Arguments

- grades:

  List of EvidenceGrade objects, or a data frame with grade information.

- outcomes:

  Character vector of outcome names (required if grades is a list
  without names).

- language:

  Output language for grade names: "en" or "de". Default: "en".

## Value

A data frame with outcome, grade, certainty, and study count.

## Examples

``` r
if (FALSE) { # \dontrun{
grades <- list(
  grade_evidence(meta1, rob1, outcome = "OS"),
  grade_evidence(meta2, rob2, outcome = "PFS")
)
summary_table <- evidence_summary_table(grades)
} # }
```
