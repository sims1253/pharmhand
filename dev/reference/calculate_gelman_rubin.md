# Calculate Gelman-Rubin Diagnostic

Calculates the Gelman-Rubin (R-hat) convergence diagnostic for MCMC
chains.

## Usage

``` r
calculate_gelman_rubin(fit, parameters = NULL)
```

## Arguments

- fit:

  A brmsfit object from brms package

- parameters:

  Character vector or NULL (default: NULL). Subset of parameter names to
  compute R-hat for. If NULL, all parameters are included.

## Value

Named numeric vector of R-hat values for each parameter

## Details

R-hat values close to 1 indicate good convergence. Values \> 1.1 suggest
potential convergence problems.

## Examples

``` r
if (FALSE) { # \dontrun{
rhat_values <- calculate_gelman_rubin(fit)
print(rhat_values)
} # }
```
