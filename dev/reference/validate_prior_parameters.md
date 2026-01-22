# Validate Prior Parameters

Validates that parameters are appropriate for the specified
distribution.

## Usage

``` r
validate_prior_parameters(distribution, parameters)
```

## Arguments

- distribution:

  Character string specifying the distribution

- parameters:

  List of distribution parameters

## Value

NULL if valid, character string with error message if invalid

## Examples

``` r
if (FALSE) { # \dontrun{
# Valid normal distribution
validate_prior_parameters("normal", list(mean = 0, sd = 1))

# Invalid normal distribution (missing sd)
validate_prior_parameters("normal", list(mean = 0))
} # }
```
