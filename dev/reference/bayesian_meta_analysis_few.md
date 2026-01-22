# Bayesian Meta-Analysis for Few Studies

Specialized Bayesian meta-analysis for situations with few studies
(typically \< 5-10 studies). Uses conservative priors and includes
sensitivity analysis.

## Usage

``` r
bayesian_meta_analysis_few(
  yi,
  sei,
  study_labels = NULL,
  effect_measure = c("hr", "or", "rr", "rd", "md", "smd"),
  prior_mu = list(mean = 0, sd = 2),
  prior_tau = list(type = "half_cauchy", scale = 0.25),
  prior_sensitivity = TRUE,
  chains = 4,
  iter = 6000,
  warmup = 3000,
  seed = NULL,
  adapt_delta = 0.99,
  max_treedepth = 15,
  backend = c("auto", "cmdstanr", "rstan"),
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

  Character. Effect type: "hr" (hazard ratio), "or" (odds ratio), "rr"
  (risk ratio), "rd" (risk difference), "md" (mean difference), "smd"
  (standardized mean difference)

- prior_mu:

  Prior for overall effect mean (default: 0, very wide)

- prior_tau:

  Prior for heterogeneity. For few studies, uses more informative priors
  that pull tau towards smaller values

- prior_sensitivity:

  Logical. Whether to perform prior sensitivity analysis

- chains:

  Integer. Number of MCMC chains (default: 4)

- iter:

  Integer. Total iterations per chain (default: 6000)

- warmup:

  Integer. Warmup iterations (default: 3000)

- seed:

  Integer. Random seed for reproducibility

- adapt_delta:

  Numeric. MCMC sampler tuning parameter (0-1)

- max_treedepth:

  Integer. Maximum tree depth for NUTS sampler

- backend:

  Character. Stan backend: "auto", "cmdstanr", "rstan"

- ...:

  Additional arguments passed to brms::brm

## Value

A BayesianMetaFewResult object

## Details

This function is specifically designed for meta-analyses with few
studies and implements several adjustments:

1.  **Conservative priors**: Uses more informative priors that are
    appropriate when data is sparse

2.  **Regularization**: Stronger regularization of between-study
    variance (tau^2)

3.  **Sensitivity analysis**: Includes prior sensitivity analysis by
    default

4.  **Few studies warnings**: Provides explicit warnings about
    limitations

Recommended for meta-analyses with 2-10 studies. For larger
meta-analyses, use the standard bayesian_meta_analysis() function.

## References

Spiegelhalter, D.J. et al. (2004). Bayesian approaches to clinical
trials and health-care evaluation. Wiley.

## Examples

``` r
if (FALSE) { # \dontrun{
# Meta-analysis with only 3 studies
yi <- log(c(0.75, 0.82, 0.68))  # log(HR) from 3 studies
sei <- c(0.12, 0.15, 0.18)       # standard errors

result <- bayesian_meta_analysis_few(
  yi = yi,
  sei = sei,
  effect_measure = "hr",
  chains = 2,
  iter = 4000
)

# With prior sensitivity analysis
result <- bayesian_meta_analysis_few(
  yi = yi,
  sei = sei,
  effect_measure = "hr",
  prior_sensitivity = TRUE,
  chains = 2,
  iter = 4000
)
} # }
```
