# Create MCMC Diagnostics Report

Generates a comprehensive MCMC diagnostics report including trace plots,
density plots, convergence assessment, and summary statistics.

## Usage

``` r
create_mcmc_diagnostics_report(
  fit,
  parameters = NULL,
  rhat_threshold = 1.01,
  ess_threshold = 100,
  title_prefix = "MCMC Diagnostics"
)
```

## Arguments

- fit:

  A brmsfit object from brms package

- parameters:

  Character vector of specific parameters to analyze. If NULL, analyzes
  all parameters.

- rhat_threshold:

  Numeric. R-hat threshold for convergence

- ess_threshold:

  Numeric. Minimum ESS threshold

- title_prefix:

  Character string for report section titles

## Value

A list containing:

- trace_plots:

  List of trace plot ClinicalPlot objects

- density_plots:

  List of density plot ClinicalPlot objects

- convergence_assessment:

  Convergence assessment results

- diagnostic_summary:

  Data frame with key diagnostics

## Examples

``` r
if (FALSE) { # \dontrun{
report <- create_mcmc_diagnostics_report(fit)
report$trace_plots[[1]]  # Access first trace plot
report$convergence_assessment$recommendations
} # }
```
