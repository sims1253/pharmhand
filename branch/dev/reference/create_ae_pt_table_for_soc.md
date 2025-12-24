# Create AE PT Table for a specific SOC

Create AE PT Table for a specific SOC

## Usage

``` r
create_ae_pt_table_for_soc(
  adae,
  trt_n,
  soc,
  trt_var = "TRT01A",
  autofit = TRUE
)
```

## Arguments

- adae:

  ADAE data frame

- trt_n:

  Treatment group counts

- soc:

  SOC value to filter by

- trt_var:

  Treatment variable name

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
