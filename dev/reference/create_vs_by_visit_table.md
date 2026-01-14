# Create Vital Signs by Visit Table

Create Vital Signs by Visit Table

## Usage

``` r
create_vs_by_visit_table(
  advs,
  paramcd = "SYSBP",
  visits = c("Baseline", "Week 2", "Week 4", "Week 8", "End of Treatment"),
  trt_var = "TRT01P",
  title = "Vital Signs by Visit",
  autofit = TRUE
)
```

## Arguments

- advs:

  ADVS data frame

- paramcd:

  Parameter code to analyze

- visits:

  Vector of visits to include

- trt_var:

  Treatment variable name (default: "TRT01P")

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
