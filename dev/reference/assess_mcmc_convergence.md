# Assess MCMC Convergence

Provides a comprehensive assessment of MCMC convergence including R-hat
values, effective sample sizes, and recommendations.

## Usage

``` r
assess_mcmc_convergence(fit, rhat_threshold = 1.01, ess_threshold = 100)
```

## Arguments

- fit:

  A brmsfit object from brms package

- rhat_threshold:

  Numeric. R-hat threshold for convergence (default: 1.01)

- ess_threshold:

  Numeric. Minimum ESS threshold (default: 100)

## Value

A list containing:

- convergence_summary:

  Data frame with convergence assessment

- rhat_values:

  Named vector of R-hat values

- ess_values:

  Named vector of ESS values

- recommendations:

  Character vector of recommendations

## Examples

``` r
if (FALSE) { # \dontrun{
assessment <- assess_mcmc_convergence(fit)
print(assessment$convergence_summary)
print(assessment$recommendations)
} # }
```
