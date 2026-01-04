# Create Mean Plot with Confidence Intervals Over Time

Creates a line plot showing mean values with confidence intervals across
visits or time points, typically used for PRO or lab parameter
visualization.

## Usage

``` r
create_mean_plot(
  data,
  x_var,
  y_var,
  group_var = NULL,
  subject_var = "USUBJID",
  ci_level = 0.95,
  show_points = FALSE,
  show_n = TRUE,
  title = NULL,
  x_label = NULL,
  y_label = NULL,
  palette = NULL,
  base_size = 11,
  line_size = 1,
  point_size = 3,
  dodge_width = 0.2,
  error_bar_width = 0.1
)
```

## Arguments

- data:

  Data frame containing the data

- x_var:

  Character. Variable for x-axis (visit or time)

- y_var:

  Character. Variable for y-axis (value)

- group_var:

  Character. Grouping variable (e.g., treatment). Default: NULL

- subject_var:

  Character. Subject ID variable. Default: "USUBJID"

- ci_level:

  Numeric. Confidence level for intervals. Default: 0.95

- show_points:

  Logical. Show individual data points. Default: FALSE

- show_n:

  Logical. Show sample size at each point. Default: TRUE

- title:

  Character. Plot title. Default: NULL

- x_label:

  Character. X-axis label. Default: NULL (uses x_var)

- y_label:

  Character. Y-axis label. Default: NULL (uses y_var)

- palette:

  Character vector. Colors for groups. Default: NULL (uses default)

- base_size:

  Numeric. Base font size. Default: 11

- line_size:

  Numeric. Line width. Default: 1

- point_size:

  Numeric. Point size for means. Default: 3

- dodge_width:

  Numeric. Horizontal dodge for overlapping points. Default: 0.2

- error_bar_width:

  Numeric. Width of error bar caps. Default: 0.1

## Value

A ClinicalPlot object containing the ggplot

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample data
data <- data.frame(
  USUBJID = rep(1:50, each = 5),
  AVISITN = rep(0:4, 50),
  AVISIT = rep(c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12"), 50),
  AVAL = rnorm(250, mean = rep(c(50, 48, 45, 44, 43), 50), sd = 10),
  TRT01P = rep(c("Treatment", "Placebo"), each = 125)
)

plot <- create_mean_plot(
  data = data,
  x_var = "AVISIT",
  y_var = "AVAL",
  group_var = "TRT01P",
  title = "Mean Score Over Time"
)
} # }
```
