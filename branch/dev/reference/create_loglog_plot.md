# Create Log-Log Survival Plot

Creates a log(-log(survival)) vs log(time) plot for visual assessment of
the proportional hazards assumption. Parallel curves indicate the PH
assumption is likely satisfied.

## Usage

``` r
create_loglog_plot(
  data,
  time_var,
  event_var,
  trt_var,
  title = "Log-Log Survival Plot",
  xlab = "Log(Time)",
  ylab = "Log(-Log(Survival))",
  show_censor = TRUE,
  colors = NULL,
  base_size = 11
)
```

## Arguments

- data:

  ADaMData object or data frame with time-to-event data

- time_var:

  Character. Name of the time variable

- event_var:

  Character. Name of the event variable (1=event, 0=censor). If "CNSR"
  (ADaM censoring flag), it will be inverted (0=event becomes 1).

- trt_var:

  Character. Name of the treatment variable

- title:

  Character. Plot title (default: "Log-Log Survival Plot")

- xlab:

  Character. X-axis label (default: "Log(Time)")

- ylab:

  Character. Y-axis label (default: "Log(-Log(Survival))")

- show_censor:

  Logical, show censoring marks as crosses (default: TRUE).

- colors:

  Named character vector of colors for each treatment group. If NULL,
  uses `getOption("pharmhand.palette")` or the default palette.

- base_size:

  Base font size for plot text elements (default: 11).

## Value

A ClinicalPlot object containing a ggplot2 log-log survival plot

## Details

Under the Cox proportional hazards assumption, the log-log transformed
survival curves should be approximately parallel. Crossing or diverging
curves suggest the PH assumption may be violated.

This is a complementary visual diagnostic to the statistical test
provided by test_ph_assumption(). Censor marks are supported; median
lines, CI bands, risk tables, and landmarks are intentionally omitted to
keep the diagnostic scale uncluttered.

## References

IQWiG Methods v8.0, Section 10.3.12, p. 235-237.
