# Calculate AE TTE Data for a specific SOC

Prepares time-to-event data for Kaplan-Meier analysis of adverse events.

## Usage

``` r
calculate_ae_tte_data(adsl, adae, soc, trt_var = "TRT01A")
```

## Arguments

- adsl:

  ADSL data frame

- adae:

  ADAE data frame

- soc:

  SOC value to filter by

- trt_var:

  Treatment variable name

## Value

Data frame formatted for KM plotting
