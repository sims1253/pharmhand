# Create Laboratory Summary Table

Create Laboratory Summary Table

## Usage

``` r
create_lab_summary_table(
  adlb,
  trt_n,
  params = c("HGB", "WBC", "PLAT", "ALT", "AST", "BILI", "CREAT"),
  visit = "Week 24",
  trt_var = "TRT01P",
  title = "Laboratory Parameters Summary",
  autofit = TRUE
)
```

## Arguments

- adlb:

  ADLB data frame

- trt_n:

  Treatment group counts

- params:

  Vector of parameter codes to analyze

- visit:

  Visit to analyze

- trt_var:

  Treatment variable name (default: "TRT01P")

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
