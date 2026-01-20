# Plot MCMC Density

Creates density plots for posterior distributions.

## Usage

``` r
plot_mcmc_density(
  fit,
  parameters = NULL,
  title = "MCMC Posterior Densities",
  alpha = 0.7,
  fill_colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
)
```

## Arguments

- fit:

  A brmsfit object from brms package

- parameters:

  Character vector of parameter names to plot. If NULL, plots all
  parameters.

- title:

  Plot title

- alpha:

  Numeric. Transparency for density areas

- fill_colors:

  Character vector of colors for different chains

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot density for all parameters
plot <- plot_mcmc_density(fit)

# Plot density for specific parameter
plot <- plot_mcmc_density(fit, parameters = "b_Intercept")
} # }
```
