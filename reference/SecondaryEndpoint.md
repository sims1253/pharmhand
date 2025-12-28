# SecondaryEndpoint Class

An S7 class for representing secondary endpoints in clinical studies.

## Usage

``` r
SecondaryEndpoint(
  name = character(0),
  variable = character(0),
  type = character(0),
  description = NULL,
  priority = 1,
  exploratory = FALSE,
  metadata = list()
)
```

## Arguments

- name:

  Character string for endpoint name

- variable:

  Character string for variable name in the dataset

- type:

  Character string for endpoint type

- description:

  Character string for endpoint description

- priority:

  Numeric value for priority

- exploratory:

  Logical, whether this is an exploratory endpoint

- metadata:

  List of additional metadata

## Value

A SecondaryEndpoint object

## Examples

``` r
if (FALSE) { # \dontrun{
endpoint <- SecondaryEndpoint(
  name = "Secondary",
  variable = "VAL2",
  type = "continuous"
)
} # }
```
