# Layered Table Class

Container for multiple analysis layers that compose into a single table.
Manages the stacking and rendering of layers.

## Usage

``` r
LayeredTable(
  data = data.frame(),
  trt_var = "TRT01P",
  pop_filter = NULL,
  layers = list(),
  title = NULL,
  metadata = list(),
  big_n = NULL
)
```

## Arguments

- data:

  Source data frame

- trt_var:

  Treatment variable name

- pop_filter:

  Population filter expression

- layers:

  List of AnalysisLayer objects

- title:

  Table title

- metadata:

  Additional metadata

- big_n:

  Pre-computed treatment group counts (optional)

## Value

A LayeredTable object

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- LayeredTable(
  data = adsl,
  trt_var = "TRT01P",
  layers = list(
    CountLayer(target_var = "SEX"),
    DescriptiveLayer(target_var = "AGE")
  ),
  title = "Demographics"
)
} # }
```
