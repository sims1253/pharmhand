# Create Empty AE Exposure Table

Helper function to construct an empty ClinicalTable for AE exposure
analysis when no events meet the criteria.

## Usage

``` r
create_empty_ae_exposure_table(
  trt_levels,
  by,
  conf_level,
  per,
  exposure_var,
  trt_var,
  time_unit,
  threshold,
  title,
  autofit,
  empty_reason
)
```
