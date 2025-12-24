# PrimaryEndpoint Class

An S7 class for representing primary endpoints in clinical studies.

## Usage

``` r
PrimaryEndpoint(
  name = character(0),
  variable = character(0),
  type = character(0),
  description = NULL,
  hypothesis = "superiority",
  margin = NULL,
  alpha = 0.05,
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

- hypothesis:

  Character string for hypothesis type

- margin:

  Numeric value for non-inferiority margin

- alpha:

  Numeric value for significance level

- metadata:

  List of additional metadata

## Value

A PrimaryEndpoint object

## Examples

``` r
if (FALSE) { # \dontrun{
endpoint <- PrimaryEndpoint(
  name = "Primary",
  variable = "VAL",
  type = "continuous"
)
} # }
```
