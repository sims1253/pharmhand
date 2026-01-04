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
