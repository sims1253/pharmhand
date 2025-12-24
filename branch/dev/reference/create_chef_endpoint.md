# Create Chef Endpoint Specification

Helper to create endpoint specifications compatible with chef pipeline.

## Usage

``` r
create_chef_endpoint(
  name,
  variable,
  type = "binary",
  population = NULL,
  strata = character(),
  stats = list(),
  criteria = list()
)
```

## Arguments

- name:

  Endpoint name

- variable:

  Variable name in ADaM dataset

- type:

  Endpoint type (e.g., "binary", "continuous", "tte")

- population:

  Population filter expression

- strata:

  Character vector of stratification variables

- stats:

  List of statistical functions to apply

- criteria:

  List of inclusion criteria

## Value

A list suitable for chef endpoint definition

## Examples

``` r
if (FALSE) { # \dontrun{
ep_spec <- create_chef_endpoint(
  name = "Response Rate",
  variable = "AVALC",
  type = "binary",
  strata = c("SEX", "AGEGR1"),
  stats = list(chefStats::n_subj, chefStats::prop_est)
)
} # }
```
