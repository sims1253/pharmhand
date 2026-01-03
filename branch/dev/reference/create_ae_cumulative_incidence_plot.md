# Create AE Cumulative Incidence Plot

Creates cumulative incidence plot for adverse events using 1 - KM
estimator. For simple analyses without competing risks.

## Usage

``` r
create_ae_cumulative_incidence_plot(
  data,
  time_var,
  event_var,
  trt_var,
  title = "Cumulative Incidence of Adverse Events",
  xlab = "Time",
  ylab = "Cumulative Incidence",
  show_ci = TRUE,
  colors = NULL
)
```

## Arguments

- data:

  Data frame with time-to-event data

- time_var:

  Character. Time variable

- event_var:

  Character. Event indicator (1=AE occurred, 0=censored)

- trt_var:

  Character. Treatment variable

- title:

  Character. Plot title

- xlab:

  Character. X-axis label

- ylab:

  Character. Y-axis label

- show_ci:

  Logical. Show confidence bands (default: TRUE)

- colors:

  Named character vector of colors

## Value

ClinicalPlot with cumulative incidence curves

## Details

Uses 1 - Kaplan-Meier to estimate cumulative incidence. This is
appropriate when there are no competing risks or competing risks are
minimal.

If `event_var` contains values other than 0/1, they are treated as
competing events and coded as censored for this estimator.

For analyses with significant competing risks (e.g., mortality),
consider using dedicated competing risk packages like cmprsk.
