# Create AE by System Organ Class Table

Create AE by System Organ Class Table

## Usage

``` r
create_ae_soc_table(
  adae,
  trt_n,
  title = "Adverse Events by System Organ Class",
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

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
