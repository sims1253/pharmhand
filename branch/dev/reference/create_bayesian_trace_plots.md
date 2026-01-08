# Create Trace Plots for Bayesian Meta-Analysis

Generate trace plots to visualize MCMC chain convergence

## Usage

``` r
create_bayesian_trace_plots(
  bayesian_result,
  parameters = c("b_Intercept", "sd_study__Intercept"),
  chains = NULL,
  combine_plots = TRUE,
  ...
)
```

## Arguments

- bayesian_result:

  A bayesian_meta_result object from bayesian_meta_analysis()

- parameters:

  Character vector of parameters to plot. Default: c("b_Intercept",
  "sd_study\_\_Intercept")

- chains:

  Integer. Number of chains to plot. Default: NULL (all chains)

- combine_plots:

  Logical. If TRUE, combine all parameters into one plot. Default: TRUE

- ...:

  Additional arguments passed to brms::plot()

## Value

A ggplot object or list of ggplot objects

## Examples

``` r
if (FALSE) { # \dontrun{
result <- bayesian_meta_analysis(yi = yi, sei = sei)
trace_plot <- create_bayesian_trace_plots(result)
print(trace_plot)
} # }
```
