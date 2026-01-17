# Create Risk of Bias Summary Table

Creates a summary table of risk of bias assessments across studies and
domains, suitable for G-BA Module 4 requirements.

## Usage

``` r
create_rob_summary_table(
  rob_results,
  title = "Risk of Bias Assessment",
  include_justification = FALSE,
  footnotes = character(),
  autofit = TRUE
)
```

## Arguments

- rob_results:

  List of RoB2Result objects (one per study).

- title:

  Table title (default: "Risk of Bias Assessment").

- include_justification:

  Logical, include justification column (default: FALSE).

- footnotes:

  Character vector of footnotes.

- autofit:

  Logical, autofit column widths (default: TRUE).

## Value

A ClinicalTable object.

## Examples

``` r
if (FALSE) { # \dontrun{
rob_results <- list(
  RoB2Result(study_id = "Study 1", ...),
  RoB2Result(study_id = "Study 2", ...)
)
rob_table <- create_rob_summary_table(rob_results)
} # }
```
