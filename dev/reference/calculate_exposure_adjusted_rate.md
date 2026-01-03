# Calculate Exposure-Adjusted Incidence Rate

Calculates incidence rate per X patient-years with Poisson CI.

## Usage

``` r
calculate_exposure_adjusted_rate(
  n_events,
  patient_years,
  conf_level = 0.95,
  per = 100
)
```

## Arguments

- n_events:

  Integer. Number of events

- patient_years:

  Numeric. Total patient-years of exposure

- conf_level:

  Numeric. Confidence level (default: 0.95)

- per:

  Numeric. Rate per X patient-years (default: 100)

## Value

A list with:

- rate: Incidence rate per X patient-years

- ci_lower: Lower CI bound

- ci_upper: Upper CI bound

- n_events: Number of events

- patient_years: Total exposure

## Examples

``` r
calculate_exposure_adjusted_rate(n_events = 4, patient_years = 2)
#> $rate
#> [1] 200
#> 
#> $ci_lower
#> [1] 54.49327
#> 
#> $ci_upper
#> [1] 512.0794
#> 
#> $n_events
#> [1] 4
#> 
#> $patient_years
#> [1] 2
#> 
```
