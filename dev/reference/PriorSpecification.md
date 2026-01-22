# PriorSpecification Class

An S7 class for representing prior distribution specifications.

## Usage

``` r
PriorSpecification(
  distribution = character(0),
  parameters = list(),
  description = "",
  domain = "",
  reference = "",
  metadata = list()
)
```

## Arguments

- distribution:

  Character string specifying the distribution type

- parameters:

  List of distribution parameters

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
