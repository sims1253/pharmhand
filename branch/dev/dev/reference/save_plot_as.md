# Save ClinicalPlot to file

Saves a ClinicalPlot to a file in the specified format.

## Usage

``` r
save_plot_as(x, format = "png", path = NULL)
```

## Arguments

- x:

  A ClinicalPlot object

- format:

  Character string for output format ("png", "pdf", "svg")

- path:

  Optional file path. If NULL, creates a temp file.

## Value

The file path where the plot was saved
