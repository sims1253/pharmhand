# Count Layer Class

Layer for categorical variable summaries including counts and
percentages. Supports distinct subject counts and nested hierarchies.

## Usage

``` r
CountLayer(
  target_var = character(0),
  by_vars = character(0),
  where = NULL,
  format_spec = NULL,
  label = NULL,
  metadata = list(),
  distinct_by = "USUBJID",
  include_pct = TRUE,
  denoms_by = character(0)
)
```

## Arguments

- target_var:

  Character vector of variable names to summarize

- by_vars:

  Character vector of grouping variables

- where:

  Filter expression (quosure or NULL)

- format_spec:

  Format specification for output

- label:

  Optional label for the layer

- metadata:

  Additional metadata

- distinct_by:

  Variable for distinct counting (default: USUBJID)

- include_pct:

  Logical, include percentages

- denoms_by:

  Variables to calculate denominators by

## Value

A CountLayer object

## Examples

``` r
layer <- CountLayer(
  target_var = "SEX",
  by_vars = "TRT01P",
  include_pct = TRUE
)
```
