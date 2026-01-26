# Create Empty AE Exposure Data Frame

Helper function to construct an empty data frame for AE exposure
analysis.

## Usage

``` r
.empty_ae_exposure_df(trt_levels, by, conf_level, per, na_string)
```

## Arguments

- trt_levels:

  Character vector of treatment levels

- by:

  Grouping level

- conf_level:

  Confidence level

- per:

  Rate per X patient-years

- na_string:

  NA string

## Value

Empty data frame with correct column structure
