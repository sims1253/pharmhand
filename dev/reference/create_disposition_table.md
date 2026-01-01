# Create Subject Disposition Table

Create Subject Disposition Table

## Usage

``` r
create_disposition_table(
  adsl,
  title = "Subject Disposition",
  trt_var = "TRT01P",
  status_var = "EOSSTT",
  reason_var = "DCSREAS",
  autofit = TRUE
)
```

## Arguments

- adsl:

  ADSL data frame

- title:

  Table title

- trt_var:

  Treatment variable name

- status_var:

  End of study status variable (e.g., EOSSTT)

- reason_var:

  Discontinuation reason variable (e.g., DCSREAS)

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
