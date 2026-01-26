# Create Primary Endpoint Summary Table

Create Primary Endpoint Summary Table

## Usage

``` r
create_primary_endpoint_table(
  data,
  paramcd = "SYSBP",
  visit = "End of Treatment",
  trt_var = ph_default("trt_var"),
  title = "Primary Endpoint Summary",
  autofit = ph_default("autofit")
)
```

## Arguments

- data:

  ADVS data frame or ADaMData object (data frames are coerced via
  .ensure_adam_data())

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

## Examples

``` r
# Create primary endpoint summary
advs <- data.frame(
  USUBJID = c("01", "02", "03", "04"),
  TRT01P = c("Placebo", "Placebo", "Active", "Active"),
  PARAMCD = rep("SYSBP", 4),
  AVISIT = rep("End of Treatment", 4),
  AVAL = c(120, 125, 118, 122)
)
table <- create_primary_endpoint_table(advs, paramcd = "SYSBP")
#> Automatically wrapping data.frame in ADaMData object
table@type
#> [1] "primary_endpoint"
```
