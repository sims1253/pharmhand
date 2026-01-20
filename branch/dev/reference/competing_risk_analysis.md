# Competing Risk Analysis

Performs competing risk analysis using the Fine-Gray model to estimate
cumulative incidence functions and subhazard ratios.

## Usage

``` r
competing_risk_analysis(
  data,
  time_var,
  event_var,
  trt_var,
  main_event,
  competing_events,
  covariates = NULL,
  conf_level = 0.95,
  time_points = NULL,
  reference_group = NULL
)
```

## Arguments

- data:

  A data frame containing the survival data

- time_var:

  Character. Name of the time variable

- event_var:

  Character. Name of the event indicator variable

- trt_var:

  Character. Name of the treatment variable

- main_event:

  Integer. Code for the main event of interest

- competing_events:

  Integer vector. Codes for competing events

- covariates:

  Character vector of covariate names

- conf_level:

  Numeric. Confidence level for intervals (default: 0.95)

- time_points:

  Numeric vector of specific time points to estimate CIF. If NULL, uses
  automatic grid based on data.

- reference_group:

  Character. Reference group for treatment comparison. If NULL, uses
  first level of treatment variable.

## Value

A CompetingRiskResult object

## Details

The Fine-Gray model estimates the cumulative incidence function (CIF):
F_k(t) = P(T \<= t, epsilon = k)

where T is the event time and epsilon is the event type.

The model uses a proportional subhazards approach: lambda_k(t\|X) =
lambda\_{0k}(t) \* exp(beta_k'X)

where lambda_k is the subhazard for event type k.

## References

Fine, J.P. and Gray, R.J. (1999). A proportional hazards model for the
subdistribution of a competing risk. Journal of the American Statistical
Association, 94(446), 496-509.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic competing risk analysis
result <- competing_risk_analysis(
  data = survival_data,
  time_var = "time",
  event_var = "event",
  trt_var = "TRT01P",
  main_event = 1,
  competing_events = c(2, 3)
)

# With covariates and custom time points
result <- competing_risk_analysis(
  data = survival_data,
  time_var = "time",
  event_var = "event",
  trt_var = "TRT01P",
  main_event = 1,
  competing_events = c(2),
  covariates = c("age", "sex"),
  time_points = c(6, 12, 24)
)
} # }
```
