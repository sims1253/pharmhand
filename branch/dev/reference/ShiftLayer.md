# Shift Layer Class

Layer for shift table analysis showing transitions from baseline to
post-baseline states (e.g., lab grade shifts).

## Usage

``` r
ShiftLayer(
  target_var = character(0),
  by_vars = character(0),
  where = NULL,
  format_spec = NULL,
  label = NULL,
  metadata = list(),
  baseline_var = character(0),
  post_var = character(0),
  include_n = TRUE,
  include_pct = TRUE
)
```

## Arguments

- target_var:

  Character vector of variable names (baseline and post)

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

- baseline_var:

  Variable name for baseline value

- post_var:

  Variable name for post-baseline value

- include_n:

  Logical, include subject counts

- include_pct:

  Logical, include percentages

## Value

A ShiftLayer object

## Examples

``` r
if (FALSE) { # \dontrun{
layer <- ShiftLayer(
  target_var = c("BTOXGR", "ATOXGR"),
  baseline_var = "BTOXGR",
  post_var = "ATOXGR",
  by_vars = "TRT01P"
)
} # }
```
