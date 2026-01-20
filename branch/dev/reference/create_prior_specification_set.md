# Create Prior Specification Set

Creates a named list of prior specifications for use in Bayesian models.

## Usage

``` r
create_prior_specification_set(priors)
```

## Arguments

- priors:

  Named list of PriorSpecification objects

## Value

Named list of PriorSpecification objects

## Examples

``` r
if (FALSE) { # \dontrun{
priors <- create_prior_specification_set(list(
  overall = get_default_prior("overall_effect"),
  heterogeneity = get_default_prior("heterogeneity")
))
} # }
```
