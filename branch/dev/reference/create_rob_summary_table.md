# Create Risk of Bias Summary Table

Creates a summary table of risk of bias assessments across studies and
domains, suitable for G-BA Module 4 requirements.

## Usage

``` r
create_rob_summary_table(
  data,
  title = "Risk of Bias Assessment",
  include_justification = FALSE,
  footnotes = character(),
  ...
)
```

## Arguments

- data:

  List of RoB2Result objects (one per study).

- title:

  Table title (default: "Risk of Bias Assessment").

- include_justification:

  Logical, include justification column (default: FALSE).

- footnotes:

  Character vector of footnotes.

- ...:

  Additional arguments passed to
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md),
  such as `col_widths`, `autofit`, `theme`, etc.

## Value

A ClinicalTable object.

## Examples

``` r
if (FALSE) { # \dontrun{
rob_results <- list(
  # RoB2Result objects with required arguments would go here
  # e.g., RoB2Result(study_id = "Study 1", outcome = "...", overall = "...", domains = list(...))
  # e.g., RoB2Result(study_id = "Study 2", outcome = "...", overall = "...", domains = list(...))
)
rob_table <- create_rob_summary_table(rob_results)
} # }
```
