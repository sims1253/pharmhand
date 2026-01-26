# Summarize AE Exposure Data

Internal function to summarize AE data for exposure-adjusted analysis.

## Usage

``` r
.summarize_ae_exposure(
  adae,
  adsl,
  exposure_var,
  trt_var,
  by,
  time_unit,
  per,
  conf_level,
  threshold,
  na_string
)
```

## Arguments

- adae:

  ADaMData object with ADAE data

- adsl:

  ADaMData object with ADSL data

- exposure_var:

  Exposure duration variable name

- trt_var:

  Treatment variable name

- by:

  Grouping level ("pt", "soc", or "overall")

- time_unit:

  Unit of exposure time

- per:

  Rate per X patient-years

- conf_level:

  Confidence level

- threshold:

  Minimum incidence threshold

- na_string:

  NA string for formatting

## Value

Data frame with summarized AE exposure data
