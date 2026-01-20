# RMST Analysis

Calculates restricted mean survival time with treatment group
comparisons.

## Usage

``` r
rmst_analysis(
  data,
  time_var,
  event_var,
  trt_var,
  tau,
  conf_level = 0.95,
  reference_group = NULL
)
```

## Arguments

- data:

  A data frame containing survival data

- time_var:

  Character. Name of the time variable

- event_var:

  Character. Name of the event indicator variable (1=event, 0=censored)

- trt_var:

  Character. Name of the treatment variable

- tau:

  Numeric. Time restriction for RMST calculation

- conf_level:

  Numeric. Confidence level for intervals (default: 0.95)

- reference_group:

  Character. Reference group for treatment comparison. If NULL, uses
  first level of treatment variable.

## Value

An RMSTResult object

## Details

The restricted mean survival time (RMST) is defined as:
`RMST(tau) = E[min(T, tau)] = integral from 0 to tau of S(t) dt`

where T is the survival time and S(t) is the survival function.

RMST is estimated using the Kaplan-Meier estimator:
`RMST(tau) = tau - integral from 0 to tau of F(t) dt = sum_i (t_i - t_i-1) * S(t_i-1)`

where the integral is restricted to `[0, tau]`.

## References

Uno, H. et al. (2014). Moving beyond the hazard ratio in quantifying the
between-group difference in survival analysis. Journal of Clinical
Oncology, 32(22), 2380-2385.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic RMST analysis
result <- rmst_analysis(
  data = survival_data,
  time_var = "time",
  event_var = "status",
  trt_var = "TRT01P",
  tau = 12
)

# With custom confidence level
result <- rmst_analysis(
  data = survival_data,
  time_var = "time",
  event_var = "status",
  trt_var = "TRT01P",
  tau = 24,
  conf_level = 0.90
)
} # }
```
