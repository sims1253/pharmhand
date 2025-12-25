# Create Kaplan-Meier Plot

Generates a standard Kaplan-Meier plot using ggplot2 and survival.

## Usage

``` r
create_km_plot(
  data,
  time_var,
  event_var,
  trt_var,
  title = "Kaplan-Meier Plot",
  xlab = "Time",
  ylab = "Survival Probability",
  risk_table = FALSE
)
```

## Arguments

- data:

  Data frame containing survival data

- time_var:

  Time variable name

- event_var:

  Event variable name (1=event, 0=censor)

- trt_var:

  Treatment variable name

- title:

  Plot title

- xlab:

  X-axis label

- ylab:

  Y-axis label

- risk_table:

  Logical, whether to include risk table below the plot. Requires the
  patchwork package.

## Value

A ClinicalPlot object
