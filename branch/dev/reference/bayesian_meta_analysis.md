# Bayesian Meta-Analysis

Interface for Bayesian meta-analysis using brms with a Stan backend.
Provides guidance when dependencies are not installed.

## Usage

``` r
bayesian_meta_analysis(
  yi,
  sei,
  study_labels = NULL,
  effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
  prior_mu = list(mean = 0, sd = 10),
  prior_tau = list(type = "half_cauchy", scale = 0.5),
  prior_predictive = FALSE,
  posterior_predictive = TRUE,
  pp_check_type = "dens_overlay",
  pp_ndraws = 100,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = NULL,
  cores = 1,
  adapt_delta = 0.95,
  max_treedepth = 12,
  backend = c("auto", "cmdstanr", "rstan"),
  warn_convergence = c("auto", "always", "never"),
  ...
)
```

## Arguments

- yi:

  Numeric vector of effect estimates

- sei:

  Numeric vector of standard errors

- study_labels:

  Character vector of study names (optional, defaults to "Study 1",
  "Study 2", etc.)

- effect_measure:

  Character. Effect type: "hr" (hazard ratio), "or" (odds ratio), "rr"
  (risk ratio), "rd" (risk difference), "md" (mean difference), "smd"
  (standardized mean difference)

- prior_mu:

  Prior for overall effect: list(mean, sd). Controls the normal prior on
  the overall pooled effect. Default: list(mean = 0, sd = 10)

- prior_tau:

  Prior for heterogeneity: list(type, scale). Valid types:
  "half_cauchy", "half_normal", "exponential". Scale controls expected
  heterogeneity magnitude. Default: list(type = "half_cauchy", scale =
  0.5)

- prior_predictive:

  Logical. Whether to perform prior predictive check. When TRUE, samples
  from the prior predictive distribution are generated and returned.
  Default: FALSE

- posterior_predictive:

  Logical. Whether to perform posterior predictive check. Generates
  posterior predictive checks using pp_check(). Default: TRUE

- pp_check_type:

  Character. Type of posterior predictive plot/check. See
  ?brms::pp_check for options. Common values: "dens_overlay", "hist",
  "scatter", "stat". Default: "dens_overlay"

- pp_ndraws:

  Integer. Number of draws to use for posterior predictive checks.
  Default: 100

- chains:

  Integer. Number of MCMC chains. Default: 4

- iter:

  Integer. Total iterations per chain. Default: 4000

- warmup:

  Integer. Warmup iterations. Default: 2000

- seed:

  Integer. Random seed

- cores:

  Integer. Number of CPU cores to use for parallel chains. Default: 1.
  Setting to \>1 requires proper seed handling.

- adapt_delta:

  Numeric. MCMC sampler tuning parameter (0-1). Default: 0.95. Higher
  values reduce divergent transitions but slow sampling.

- max_treedepth:

  Integer. Maximum tree depth for NUTS sampler. Default: 12. Higher
  values allow more complex posterior geometry but may indicate issues.

- backend:

  Character. Which Stan backend to use: "auto" (prefer cmdstanr when
  available), "cmdstanr", or "rstan".

- warn_convergence:

  Character. Whether to emit convergence warnings: "auto" (quiet under
  testthat), "always", or "never".

- ...:

  Additional arguments passed to brms::brm

## Value

A list with class "bayesian_meta_result" containing posterior_mean,
ci_95, tau_mean, tau_ci_95, n_studies, effect_measure, model_type, fit,
convergence_diagnostics, prior_predictive, posterior_predictive, and
pp_check_plot. See Details for full descriptions. Returns installation
guidance if brms is not installed.

## Details

This function requires the brms package and a Stan backend (cmdstanr or
rstan) for full Bayesian inference. If these are not installed, the
function returns guidance on installation and falls back to frequentist
meta-analysis via
[`meta_analysis`](https://sims1253.github.io/pharmhand/branch/dev/reference/meta_analysis.md).

Install dependencies with:
`install.packages(c("brms", "cmdstanr", "rstan"))`

Note: rstan may require additional setup. See
<https://mc-stan.org/users/interfaces/rstan> for details.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic Bayesian random-effects meta-analysis of hazard ratios
# Effect estimates must be on log scale for ratio measures
yi <- log(c(0.75, 0.82, 0.68, 0.91))  # log(HR) from 4 studies
sei <- c(0.12, 0.15, 0.18, 0.14)       # standard errors

result <- bayesian_meta_analysis(
  yi = yi,
  sei = sei,
  effect_measure = "hr",  # Hazard ratio (requires log-transformed yi)
  chains = 2,
  iter = 2000
)

# View posterior summary
result$posterior_mean
result$ci_95
result$interpretation

# With predictive checking enabled
result_with_pp <- bayesian_meta_analysis(
  yi = yi,
  sei = sei,
  effect_measure = "hr",
  prior_predictive = TRUE,
  posterior_predictive = TRUE,
  pp_check_type = "dens_overlay",
  pp_ndraws = 100
)

# Access prior predictive results
result_with_pp$prior_predictive$summary

# Access posterior predictive results
result_with_pp$posterior_predictive$bayes_p_value

# Get the pp_check plot
print(result_with_pp$pp_check_plot)
} # }
```
