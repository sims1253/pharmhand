# Create Laboratory Shift Table

Create Laboratory Shift Table

## Usage

``` r
create_lab_shift_table(
  adlb,
  paramcd = "ALT",
  visit = "Week 24",
  trt_var = "TRT01P",
  title = "Laboratory Shift Table",
  autofit = TRUE
)
```

## Arguments

- adlb:

  ADLB data frame

- paramcd:

  Parameter code to analyze

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
