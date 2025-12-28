# ClinicalPlot Class

An S7 class for representing clinical study plots (Kaplan-Meier, forest
plots, waterfall plots, etc.) with methods for formatting and conversion
to Word elements.

## Usage

``` r
ClinicalPlot(
  type = NULL,
  title = NULL,
  metadata = list(),
  plot = NULL,
  data = NULL,
  width = 6,
  height = 4,
  dpi = 300
)
```

## Arguments

- type:

  Character string for plot type (e.g., "km", "forest")

- title:

  Character string for plot title

- metadata:

  List of additional metadata

- plot:

  A ggplot or ggsurvplot object

- data:

  A data frame containing the plot data (optional)

- width:

  Numeric value for plot width in inches

- height:

  Numeric value for plot height in inches

- dpi:

  Numeric value for plot DPI

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()
plot <- ClinicalPlot(plot = p, type = "scatter", title = "MPG vs HP")
} # }
```
