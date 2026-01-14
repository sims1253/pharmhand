# Create AE KM Plot for a specific SOC

Create AE KM Plot for a specific SOC

## Usage

``` r
create_ae_km_plot_for_soc(adsl, adae, soc, trt_var = "TRT01P")
```

## Arguments

- adsl:

  ADSL data frame

- adae:

  ADAE data frame

- soc:

  SOC value to filter by

- trt_var:

  Treatment variable name

## Value

ClinicalPlot object, or NULL if no data available for the specified SOC

## Examples

``` r
if (FALSE) { # \dontrun{
# Create KM plot for AE in a specific SOC
plot <- create_ae_km_plot_for_soc(
  adsl = adsl,
  adae = adae,
  soc = "Gastrointestinal disorders"
)
plot@plot
} # }
```
