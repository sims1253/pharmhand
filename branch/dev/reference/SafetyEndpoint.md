# SafetyEndpoint Class

An S7 class for representing safety endpoints in clinical studies.

## Usage

``` r
SafetyEndpoint(
  name = character(0),
  variable = character(0),
  type = character(0),
  description = NULL,
  severity = NULL,
  relatedness = NULL,
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

- severity:

  Character string for severity level

- relatedness:

  Character string for relatedness to treatment

- metadata:

  List of additional metadata

## Value

A SafetyEndpoint object

## Examples

``` r
if (FALSE) { # \dontrun{
endpoint <- SafetyEndpoint(
  name = "Safety",
  variable = "AE",
  type = "adverse_event"
)
} # }
```
