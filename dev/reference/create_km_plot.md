# Create Kaplan-Meier Plot

Kaplan-Meier plot using ggplot2 and survival.

## Usage

``` r
create_km_plot(
  data,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01P",
  title = "Kaplan-Meier Plot",
  xlab = "Time",
  ylab = "Survival Probability",
  risk_table = FALSE,
  show_median = FALSE,
  show_ci = FALSE,
  show_censor = TRUE,
  landmarks = NULL,
  xlim = NULL,
  palette = NULL,
  conf_level = 0.95,
  base_size = 11
)
```

## Arguments

- data:

  ADaMData object or data frame containing survival data

- time_var:

  Time variable name (default: "AVAL")

- event_var:

  Event variable name. If "CNSR" (ADaM censoring flag), it will be
  automatically inverted (0=event becomes 1=event). Otherwise expects
  1=event, 0=censor. Default: "CNSR"

- trt_var:

  Treatment variable name (default: "TRT01P")

- title:

  Plot title

- xlab:

  X-axis label

- ylab:

  Y-axis label

- risk_table:

  Logical, include risk table below. Requires patchwork.

- show_median:

  Logical, add horizontal/vertical lines at median survival time
  (default: FALSE)

- show_ci:

  Logical, show confidence bands around survival curves (default: FALSE)

- show_censor:

  Logical, show censoring marks as crosses (default: TRUE)

- landmarks:

  Numeric vector of timepoints to highlight with vertical lines (e.g.,
  c(12, 24) for 12 and 24 months). NULL for none.

- xlim:

  Optional x-axis limits as c(min, max)

- palette:

  Color palette for treatment groups. Precedence: (1) explicit palette
  argument (character vector of colors or named palette string), (2)
  `getOption("pharmhand.palette")`, (3) default Okabe-Ito reordered
  (orange, sky blue for first two arms). Named palette strings from
  [`grDevices::palette.pals()`](https://rdrr.io/r/grDevices/palette.html)
  can be used (e.g., "Okabe-Ito", "R4", "Tableau 10", "Alphabet").

- conf_level:

  Confidence level for CI bands (default: 0.95)

- base_size:

  Base font size for plot text elements (default: 11). Also used for
  risk table text.

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic KM plot
km <- create_km_plot(
  data = adtte,
  time_var = "AVAL",
  event_var = "CNSR",
  title = "Overall Survival"
)

# With median lines and CI bands
km <- create_km_plot(
  data = adtte,
  show_median = TRUE,
  show_ci = TRUE,
  risk_table = TRUE
)
} # }
```
