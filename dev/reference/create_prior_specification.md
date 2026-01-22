# Create Prior Specification

Creates a prior distribution specification for use in Bayesian analyses.

## Usage

``` r
create_prior_specification(
  distribution,
  parameters,
  description = "",
  domain = "",
  reference = "",
  metadata = list()
)
```

## Arguments

- distribution:

  Character string specifying the distribution: "normal", "beta",
  "gamma", "half_cauchy", "half_normal", "exponential", "uniform",
  "cauchy", "t", "log_normal", "inverse_gamma", "laplace", "student_t"

- parameters:

  List of distribution parameters (see details)

- description:

  Character string describing the prior

- domain:

  Character string indicating the parameter domain

- reference:

  Character string with reference/citation

- metadata:

  List of additional metadata

## Value

A PriorSpecification object

## Details

Distribution parameters:

- normal:

  mean, sd

- beta:

  shape1, shape2

- gamma:

  shape, rate (or scale)

- half_cauchy:

  location, scale

- half_normal:

  sd

- exponential:

  rate

- uniform:

  min, max

- cauchy:

  location, scale

- t:

  df, location, scale

- log_normal:

  meanlog, sdlog

- inverse_gamma:

  shape, scale

- laplace:

  location, scale

- student_t:

  df, location, scale

## Examples

``` r
if (FALSE) { # \dontrun{
# Vague normal prior
prior1 <- create_prior_specification(
  distribution = "normal",
  parameters = list(mean = 0, sd = 10),
  description = "Vague prior for overall effect",
  domain = "overall_effect"
)

# Informative beta prior
prior2 <- create_prior_specification(
  distribution = "beta",
  parameters = list(shape1 = 2, shape2 = 2),
  description = "Moderately informative prior for probability",
  domain = "probability"
)

# Half-Cauchy prior for variance component
prior3 <- create_prior_specification(
  distribution = "half_cauchy",
  parameters = list(location = 0, scale = 0.5),
  description = "Default prior for heterogeneity",
  domain = "heterogeneity"
)
} # }
```
