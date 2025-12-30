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
  distinct_by = "USUBJID",
  include_n = TRUE,
  include_pct = TRUE
)
```

## Arguments

- target_var:

  Character vector of exactly 2 variable names: c(baseline_var,
  post_var). First element is the baseline variable, second is the
  post-baseline variable.

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
  by_vars = "TRT01P"
)
} # }
```
