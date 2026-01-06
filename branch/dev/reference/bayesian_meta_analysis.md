# Interface for Bayesian meta-analysis using brms/rstan when available. Provides guidance when dependencies are not installed.

Interface for Bayesian meta-analysis using brms/rstan when available.
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
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = NULL,
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

- chains:

  Integer. Number of MCMC chains. Default: 4

- iter:

  Integer. Total iterations per chain. Default: 4000

- warmup:

  Integer. Warmup iterations. Default: 2000

- seed:

  Integer. Random seed

- ...:

  Additional arguments passed to brms::brm

## Value

A list containing:

- estimate:

  Posterior mean of overall effect

- ci:

  Credible interval (2.5%, 97.5%)

- tau:

  Posterior mean of heterogeneity SD

- tau_ci:

  Credible interval for tau

- k:

  Number of studies

- effect_measure:

  Effect measure used

- model:

  Model type ("bayesian")

- fit:

  Full brms fit object (when brms available)

If brms is not installed, returns a list with installation guidance.

## Details

This function requires the brms and rstan packages for full Bayesian
inference. If these are not installed, the function returns guidance on
installation and falls back to frequentist meta-analysis via
[`meta_analysis`](https://sims1253.github.io/pharmhand/branch/dev/reference/meta_analysis.md).

Install dependencies with: `install.packages(c("brms", "rstan"))`

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
} # }
```
