# Create Default Prior Set for Meta-Analysis

Creates a standard set of priors for Bayesian meta-analysis.

## Usage

``` r
create_meta_analysis_priors(few_studies = FALSE, informative = FALSE)
```

## Arguments

- few_studies:

  Logical. Whether to use priors appropriate for few studies

- informative:

  Logical. Whether to use more informative priors

## Value

Named list of PriorSpecification objects

## Examples

``` r
if (FALSE) { # \dontrun{
# Default priors for standard meta-analysis
priors <- create_meta_analysis_priors()

# Priors for few studies
priors <- create_meta_analysis_priors(few_studies = TRUE)

# More informative priors
priors <- create_meta_analysis_priors(informative = TRUE)
} # }
```
