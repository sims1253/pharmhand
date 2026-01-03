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
  min_subgroup_size = 20,
  trt_var = "TRT01P",
  title = "Subgroup Analysis",
  autofit = TRUE
)
```

## Arguments

- adsl:

  ADSL data frame. Subgroup variables can be sourced from adsl if not
  present in advs. Must contain USUBJID column for joining.

- advs:

  ADVS data frame

- paramcd:

  Parameter code to analyze

- visit:

  Visit to analyze

- subgroups:

  Named list of subgroup variables with display labels (e.g.
  list(AGEGR1="Age Group", SEX="Sex")). Variables can come from advs or
  adsl (joined by USUBJID).

- min_subgroup_size:

  Numeric. Minimum subgroup size required before warnings are issued.
  Use NULL to disable warnings. Default is 20.

- trt_var:

  Treatment variable name (default: "TRT01P")

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
