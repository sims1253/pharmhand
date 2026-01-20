# Plot MCMC Trace

Creates trace plots for MCMC chain convergence assessment.

## Usage

``` r
plot_mcmc_trace(
  fit,
  parameters = NULL,
  chains = NULL,
  title = "MCMC Trace Plots",
  alpha = 0.8
)
```

## Arguments

- fit:

  A brmsfit object from brms package

- parameters:

  Character vector of parameter names to plot. If NULL, plots all
  parameters.

- chains:

  Integer vector of chain numbers to plot. If NULL, plots all chains.

- title:

  Plot title

- alpha:

  Numeric. Transparency for chain lines

## Value

A ClinicalPlot object

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot trace for all parameters
plot <- plot_mcmc_trace(fit)

# Plot trace for specific parameter
plot <- plot_mcmc_trace(fit, parameters = "b_Intercept")

# Plot specific chains
plot <- plot_mcmc_trace(fit, chains = 1:2)
} # }
```
