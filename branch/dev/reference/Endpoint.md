# Endpoint Class

An S7 class for representing clinical study endpoints. Replaces the
previous PrimaryEndpoint, SecondaryEndpoint, and SafetyEndpoint classes
with a unified class using the `category` property.

## Usage

``` r
Endpoint(
  name = character(0),
  variable = character(0),
  type = "continuous",
  category = "primary",
  description = NULL,
  hypothesis = "superiority",
  margin = NULL,
  alpha = 0.05,
  priority = 1,
  metadata = list()
)
```

## Arguments

- name:

  Character string for endpoint name

- variable:

  Character string for variable name in the dataset

- type:

  Character string for endpoint type: "continuous", "binary", "tte",
  "count", "pro"

- category:

  Character string for endpoint category: "primary", "secondary",
  "safety", "exploratory"

- description:

  Character string for endpoint description

- hypothesis:

  Character string for hypothesis type: "superiority",
  "non-inferiority", "equivalence"

- margin:

  Numeric value for non-inferiority/equivalence margin (if applicable)

- alpha:

  Numeric value for significance level (default: 0.05)

- priority:

  Numeric value for analysis priority (default: 1)

- metadata:

  List of additional metadata

## Value

An Endpoint object

## Examples

``` r
if (FALSE) { # \dontrun{
# Primary efficacy endpoint
endpoint <- Endpoint(
  name = "Overall Survival",
  variable = "AVAL",
  type = "tte",
  category = "primary",
  hypothesis = "superiority"
)

# Safety endpoint
ae_endpoint <- Endpoint(
  name = "Treatment-Emergent AEs",
  variable = "AETERM",
  type = "count",
  category = "safety"
)
} # }
```
