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

  Character vector of study names

- effect_measure:

  Character. Effect type

- prior_mu:

  Prior for overall effect: list(mean, sd)

- prior_tau:

  Prior for heterogeneity: list(type, scale)

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

List with posterior summaries or guidance for installation
