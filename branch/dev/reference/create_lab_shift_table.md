# Create Laboratory Shift Table

Create Laboratory Shift Table

## Usage

``` r
create_lab_shift_table(
  adlb,
  trt_n,
  paramcd = "ALT",
  visit = "Week 24",
  title = "Laboratory Shift Table",
  autofit = TRUE
)
```

## Arguments

- adlb:

  ADLB data frame

- trt_n:

  Treatment group counts

- paramcd:

  Parameter code to analyze

- visit:

  Visit to analyze

- title:

  Table title

## Value

ClinicalTable object
