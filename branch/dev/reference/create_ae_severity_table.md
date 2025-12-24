# Create AE by Maximum Severity Table

Create AE by Maximum Severity Table

## Usage

``` r
create_ae_severity_table(
  adae,
  trt_n,
  title = "Subjects by Maximum Adverse Event Severity",
  trt_var = "TRT01A",
  autofit = TRUE
)
```

## Arguments

- adae:

  ADAE data frame

- trt_n:

  Treatment group counts

- title:

  Table title

- trt_var:

  Treatment variable name

## Value

ClinicalTable object
