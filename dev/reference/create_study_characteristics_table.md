# Create Study Characteristics Table

Creates a summary table of study characteristics for G-BA Module 4
requirements, displaying design, sample size, treatment, and comparator
information for each included study.

## Usage

``` r
create_study_characteristics_table(
  studies,
  title = "Study Characteristics",
  columns = c("Study", "Design", "N", "Treatment", "Comparator", "Population"),
  include_metadata = TRUE,
  footnotes = character(),
  col_widths = NULL,
  autofit = TRUE
)
```

## Arguments

- studies:

  List of Study objects, TwoArmStudy objects, or data frame with
  study-level characteristics.

- title:

  Table title (default: "Study Characteristics")

- columns:

  Character vector of column names. Default columns: "Study", "Design",
  "N", "Treatment", "Comparator", "Population".

- include_metadata:

  Logical, include study metadata columns (default: TRUE).

- footnotes:

  Character vector of footnotes to add.

- col_widths:

  Named numeric vector of column widths (in inches).

- autofit:

  Logical, whether to autofit column widths (default: TRUE).

## Value

A ClinicalTable object containing the study characteristics table.

## Examples

``` r
if (FALSE) { # \dontrun{
studies <- list(
  TwoArmStudy(
    study_id = "STUDY001",
    study_title = "Phase III Trial of Drug X vs Placebo",
    design = "rct",
    population = "ITT"
  ),
  TwoArmStudy(
    study_id = "STUDY002",
    study_title = "Phase III Trial of Drug X vs Standard of Care",
    design = "rct",
    population = "FAS"
  )
)

table <- create_study_characteristics_table(studies)
} # }
```
