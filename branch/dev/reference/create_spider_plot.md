# Create Spider Plot for Individual Trajectories

Creates a spider plot showing individual patient trajectories over time,
commonly used for PRO data, tumor response, or other longitudinal
measures.

## Usage

``` r
create_spider_plot(
  data,
  x_var,
  y_var,
  subject_var = "USUBJID",
  group_var = NULL,
  reference_line = NULL,
  threshold_lines = NULL,
  highlight_subjects = NULL,
  title = NULL,
  x_label = NULL,
  y_label = NULL,
  palette = NULL,
  alpha = 0.6,
  base_size = 11,
  line_size = 0.5
)
```

## Arguments

- data:

  Data frame containing longitudinal data

- x_var:

  Character. Variable for x-axis (visit/time)

- y_var:

  Character. Variable for y-axis (value or percent change)

- subject_var:

  Character. Subject ID variable. Default: "USUBJID"

- group_var:

  Character. Grouping variable for coloring. Default: NULL

- reference_line:

  Numeric. Y-value for reference line. Default: NULL

- threshold_lines:

  Numeric vector. Additional threshold lines. Default: NULL

- highlight_subjects:

  Character vector. Subject IDs to highlight. Default: NULL

- title:

  Character. Plot title. Default: NULL

- x_label:

  Character. X-axis label. Default: NULL

- y_label:

  Character. Y-axis label. Default: NULL

- palette:

  Character vector. Colors for groups. Default: NULL

- alpha:

  Numeric. Line transparency (0-1). Default: 0.6

- base_size:

  Numeric. Base font size. Default: 11

- line_size:

  Numeric. Line width. Default: 0.5

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
# Tumor response spider plot
data <- data.frame(
  USUBJID = rep(paste0("SUBJ", 1:20), each = 6),
  AVISITN = rep(0:5, 20),
  PCHG = c(replicate(20, cumsum(c(0, rnorm(5, mean = -5, sd = 15))))),
  TRT01P = rep(c("Treatment", "Placebo"), each = 60)
)

plot <- create_spider_plot(
  data = data,
  x_var = "AVISITN",
  y_var = "PCHG",
  group_var = "TRT01P",
  reference_line = 0,
  threshold_lines = c(-30, 20),
  title = "Individual Tumor Response Over Time",
  y_label = "% Change from Baseline"
)
} # }
```
