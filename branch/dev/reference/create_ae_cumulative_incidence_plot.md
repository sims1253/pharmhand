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
  conf_level = 0.95,
  colors = NULL,
  base_size = 11
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

- conf_level:

  Numeric. Confidence level for intervals (default: 0.95)

- colors:

  Named character vector of colors

- base_size:

  Base font size for plot text elements (default: 11)

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

## Examples

``` r
if (FALSE) { # \dontrun{
# Cumulative incidence plot for AEs
plot <- create_ae_cumulative_incidence_plot(
  data = adaette,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01P",
  title = "Cumulative Incidence of Serious AEs"
)
print(plot)
} # }
```
