# Create Concomitant Medications Table

Create Concomitant Medications Table

## Usage

``` r
create_conmeds_table(
  data,
  adsl,
  title = "Prior and Concomitant Medications by Class",
  trt_var = "TRT01P",
  class_var = "CMCLAS",
  autofit = TRUE
)
```

## Arguments

- data:

  ADCM data frame or ADaMData object

- adsl:

  ADSL data frame or ADaMData object for denominators

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
