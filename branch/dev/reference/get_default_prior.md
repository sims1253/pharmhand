# Get Default Prior

Returns a default prior specification for common Bayesian analysis
domains.

## Usage

``` r
get_default_prior(domain)
```

## Arguments

- domain:

  Character string specifying the domain: "overall_effect",
  "heterogeneity", "treatment_effect", "baseline_risk", "probability",
  "variance", "correlation", "few_studies"

## Value

A PriorSpecification object, or NULL if domain not recognized

## Examples

``` r
if (FALSE) { # \dontrun{
# Default prior for overall effect
prior <- get_default_prior("overall_effect")

# Default prior for heterogeneity
prior <- get_default_prior("heterogeneity")
} # }
```
