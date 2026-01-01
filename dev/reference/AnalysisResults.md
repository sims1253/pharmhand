# AnalysisResults Class

Container for pre-calculated statistics.

## Usage

``` r
AnalysisResults(
  stats = structure(list(), names = character(0),
    row.names = integer(0), class = "data.frame"),
  type = "",
  groupings = list(),
  metadata = list()
)
```

## Arguments

- stats:

  A data frame containing the statistical results

- type:

  Character string for result type

- groupings:

  List of grouping variables used

- metadata:

  List of additional metadata

## Value

An AnalysisResults object
