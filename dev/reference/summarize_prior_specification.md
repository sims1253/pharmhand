# Summarize Prior Specification

Creates a summary data frame from a prior specification.

## Usage

``` r
summarize_prior_specification(prior)
```

## Arguments

- prior:

  A PriorSpecification object

## Value

A data frame with prior summary information

## Examples

``` r
if (FALSE) { # \dontrun{
prior <- create_prior_specification("normal", list(mean = 0, sd = 1))
summary <- summarize_prior_specification(prior)
print(summary)
} # }
```
