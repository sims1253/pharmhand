# Create AE Leading to Discontinuation Table

Create AE Leading to Discontinuation Table

## Usage

``` r
create_ae_discontinuation_table(
  adae,
  trt_n,
  title = "Adverse Events Leading to Study Drug Discontinuation",
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
