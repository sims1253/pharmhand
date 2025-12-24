# Create AE PT Tables for all SOCs

Create AE PT Tables for all SOCs

## Usage

``` r
create_ae_pt_tables_by_soc(adae, trt_n, trt_var = "TRT01A", autofit = TRUE)
```

## Arguments

- adae:

  ADAE data frame

- trt_n:

  Treatment group counts

- trt_var:

  Treatment variable name

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

List of ClinicalTable objects
