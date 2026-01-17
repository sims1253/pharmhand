# Save Risk of Bias Plot

Saves a risk of bias plot to file. The file format is determined by the
filename extension (.png, .pdf, .svg, .tiff, .jpeg, .bmp, or .wmf).

## Usage

``` r
save_rob_plot(plot, filename, width = 10, height = 6, dpi = 300, ...)
```

## Arguments

- plot:

  ClinicalPlot object or ggplot

- filename:

  Output filename (extension determines format)

- width:

  Width in inches. Default: 10

- height:

  Height in inches. Default: 6

- dpi:

  Resolution for raster formats (png, tiff, jpeg, bmp). Default: 300

- ...:

  Additional arguments passed to ggplot2::ggsave

## Value

Invisible filename

## Examples

``` r
if (FALSE) { # \dontrun{
# Create and save traffic light plot
results <- list(
  assess_rob2(
    study_id = "STUDY001",
    d1_randomization = "Low",
    d2_deviations = "Low",
    d3_missing_data = "Low",
    d4_measurement = "Some concerns",
    d5_selection = "Low"
  )
)

plot <- create_rob_traffic_light_plot(results)
save_rob_plot(plot, "rob_traffic_light.png", width = 8, height = 5)

# Save as PDF (vector format)
save_rob_plot(plot, "rob_summary.pdf", width = 10, height = 6)
} # }
```
