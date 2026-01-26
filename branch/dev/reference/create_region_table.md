# Create Enrollment by Region Table

Create Enrollment by Region Table

## Usage

``` r
create_region_table(
  data,
  title = "Enrollment by Region",
  trt_var = "TRT01P",
  region_var = "REGION1",
  autofit = TRUE
)
```

## Arguments

- data:

  ADSL data frame or ADaMData object

- title:

  Table title

- trt_var:

  Treatment variable name

- region_var:

  Region variable name

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
