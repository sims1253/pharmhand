# Prior Sensitivity Analysis for Bayesian Meta-Analysis

Assess how sensitive results are to different prior specifications

## Usage

``` r
prior_sensitivity_analysis(
  yi,
  sei,
  study_labels = NULL,
  effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
  prior_scenarios = NULL,
  chains = 2,
  iter = 2000,
  seed = 42,
  ...
)
```

## Arguments

- yi:

  Numeric vector of effect estimates

- sei:

  Numeric vector of standard errors

- study_labels:

  Character vector of study names (optional)

- effect_measure:

  Character. Effect type

- prior_scenarios:

  List of prior specification scenarios. Each scenario should have:
  `name`: Character name for scenario `prior_mu`: List with `mean` and
  `sd` `prior_tau`: List with `type` and `scale`

- chains:

  Integer. Number of MCMC chains per scenario. Default: 2

- iter:

  Integer. Total iterations per chain. Default: 2000

- seed:

  Integer. Random seed for reproducibility

- ...:

  Additional arguments passed to bayesian_meta_analysis()

## Value

A list containing:

- scenarios:

  List of bayesian_meta_result objects for each scenario

- comparison:

  Data frame comparing estimates across scenarios

- sensitivity_summary:

  Summary of how results change across priors

## Examples

``` r
if (FALSE) { # \dontrun{
scenarios <- list(
  weak = list(
    prior_mu = list(mean = 0, sd = 10),
    prior_tau = list(type = "half_cauchy", scale = 0.5)
  ),
  informative = list(
    prior_mu = list(mean = 0, sd = 1),
    prior_tau = list(type = "half_cauchy", scale = 0.25)
  )
)

sensitivity <- prior_sensitivity_analysis(
  yi = yi,
  sei = sei,
  effect_measure = "hr",
  prior_scenarios = scenarios
)
print(sensitivity$comparison)
} # }
```
