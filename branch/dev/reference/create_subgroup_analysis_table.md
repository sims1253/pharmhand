# Create Subgroup Analysis Table

Create Subgroup Analysis Table

## Usage

``` r
create_subgroup_analysis_table(
  adsl,
  advs,
  paramcd = "SYSBP",
  visit = "End of Treatment",
  subgroups = list(AGEGR1 = "Age Group", SEX = "Sex"),
  title = "Subgroup Analysis",
  autofit = TRUE
)
```

## Arguments

- adsl:

  ADSL data frame

- advs:

  ADVS data frame

- paramcd:

  Parameter code to analyze

- visit:

  Visit to analyze

- subgroups:

  List of subgroup variables (e.g. list(AGEGR1="Age Group"))

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
