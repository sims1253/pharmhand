# Create Concomitant Medications Table

Create Concomitant Medications Table

## Usage

``` r
create_conmeds_table(
  adsl,
  adcm,
  title = "Prior and Concomitant Medications by Class",
  trt_var = "TRT01P",
  class_var = "CMCLAS",
  autofit = TRUE
)
```

## Arguments

- adsl:

  ADSL data frame

- adcm:

  ADCM data frame

- title:

  Table title

- trt_var:

  Treatment variable name

- class_var:

  ATC class/category variable name

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
