# Create Vital Signs by Visit Table

Create Vital Signs by Visit Table

## Usage

``` r
create_vs_by_visit_table(
  advs,
  trt_n,
  paramcd = "SYSBP",
  visits = c("Baseline", "Week 2", "Week 4", "Week 8", "End of Treatment"),
  title = "Vital Signs by Visit",
  autofit = TRUE
)
```

## Arguments

- advs:

  ADVS data frame

- trt_n:

  Treatment group counts

- paramcd:

  Parameter code to analyze

- visits:

  Vector of visits to include

- title:

  Table title

## Value

ClinicalTable object
