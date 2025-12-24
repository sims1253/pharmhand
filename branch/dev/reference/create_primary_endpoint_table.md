# Create Primary Endpoint Summary Table

Create Primary Endpoint Summary Table

## Usage

``` r
create_primary_endpoint_table(
  advs,
  trt_n,
  paramcd = "SYSBP",
  visit = "End of Treatment",
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

- title:

  Table title

## Value

ClinicalTable object
