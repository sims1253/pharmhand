# Ensure ADaMData

Helper function that coerces raw data frames to ADaMData if they aren't
already ADaMData objects.

## Usage

``` r
.ensure_adam_data(data, domain = "ADSL", trt_var = NULL, subject_var = NULL)
```

## Arguments

- data:

  A data frame or ADaMData object

- domain:

  Character string for the ADaM domain (used when wrapping data frames)

- trt_var:

  Treatment variable name (passed to ADaMData constructor when wrapping
  data frames)

- subject_var:

  Subject ID variable name (passed to ADaMData constructor when wrapping
  data frames)

## Value

An ADaMData object
