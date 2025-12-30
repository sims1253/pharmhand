# Descriptive Layer Class

Layer for continuous variable summaries including mean, SD, median, min,
max, and other descriptive statistics.

## Usage

``` r
DescriptiveLayer(
  target_var = character(0),
  by_vars = character(0),
  where = NULL,
  format_spec = NULL,
  label = NULL,
  metadata = list(),
  stats = c("n", "mean", "sd", "median", "min", "max"),
  precision = "auto"
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

- stats:

  Character vector of statistics to compute

- precision:

  Integer or "auto" for decimal precision

## Value

A DescriptiveLayer object

## Examples

``` r
if (FALSE) { # \dontrun{
layer <- DescriptiveLayer(
  target_var = "AGE",
  by_vars = "TRT01P",
  stats = c("n", "mean", "sd", "median", "min", "max")
)
} # }
```
