# Create Primary Endpoint Summary Table

Create Primary Endpoint Summary Table

## Usage

``` r
create_primary_endpoint_table(
  advs,
  trt_n,
  paramcd = "SYSBP",
  visit = "End of Treatment",
  trt_var = "TRT01P",
  title = "Primary Endpoint Summary",
  autofit = TRUE
)
```

## Arguments

- advs:

  ADVS data frame

- trt_n:

  Treatment group counts

- paramcd:

  Parameter code to analyze (default: "SYSBP")

- visit:

  Visit to analyze (default: "End of Treatment")

- trt_var:

  Treatment variable name (default: "TRT01P")

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
