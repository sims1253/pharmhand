# Calculate Effective Sample Size

Calculates the effective sample size (ESS) for MCMC parameters.

## Usage

``` r
calculate_effective_sample_size(fit)
```

## Arguments

- fit:

  A brmsfit object from brms package

## Value

Named numeric vector of ESS values for each parameter

## Details

ESS represents the number of independent samples that would give the
same precision as the correlated MCMC samples. Higher values indicate
better mixing of the chains.

## Examples

``` r
if (FALSE) { # \dontrun{
ess_values <- calculate_effective_sample_size(fit)
print(ess_values)
} # }
```
