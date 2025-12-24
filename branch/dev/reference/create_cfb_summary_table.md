# Create Change from Baseline Summary Table

Create Change from Baseline Summary Table

## Usage

``` r
create_cfb_summary_table(
  advs,
  trt_n,
  params = c("SYSBP", "DIABP", "PULSE"),
  visit = "End of Treatment",
  title = "Change from Baseline Summary",
  autofit = TRUE
)
```

## Arguments

- advs:

  ADVS data frame

- trt_n:

  Treatment group counts

- params:

  Vector of parameter codes to include

- visit:

  Visit to analyze

- title:

  Table title

## Value

ClinicalTable object
