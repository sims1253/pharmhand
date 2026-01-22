# HTAEndpoint Class

An S7 class for representing Health Technology Assessment endpoints.

## Usage

``` r
HTAEndpoint(
  name = character(0),
  variable = character(0),
  type = "continuous",
  category = "primary",
  description = NULL,
  hypothesis = "superiority",
  margin = NULL,
  alpha = 0.05,
  priority = 1,
  metadata = list(),
  strata = character(0),
  criteria = list()
)
```

## Arguments

- name:

  Character string for endpoint name

- variable:

  Character string for variable name in the dataset

- type:

  Character string for endpoint type

- category:

  Character string for endpoint category (inherited from Endpoint)

- description:

  Character string for endpoint description

- hypothesis:

  Character string for hypothesis type

- margin:

  Numeric value for non-inferiority margin

- alpha:

  Numeric value for significance level

- priority:

  Numeric value for analysis priority (inherited from Endpoint)

- metadata:

  List of additional metadata

- strata:

  Character vector of stratification variables

- criteria:

  List of criteria definitions for HTA assessment (e.g., list(clinical =
  list(...), economic = list(...)))

## Value

An HTAEndpoint object

## Examples

``` r
if (FALSE) { # \dontrun{
endpoint <- HTAEndpoint(
  name = "Response Rate",
  variable = "AVALC",
  type = "binary",
  strata = c("SEX", "AGEGR1")
)
} # }
```
