# Create AE by Relationship Table

Create AE by Relationship Table

## Usage

``` r
create_ae_relationship_table(
  adae,
  trt_n,
  title = "Adverse Events by Relationship to Study Drug",
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
