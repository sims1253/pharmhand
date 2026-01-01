# PerformanceReport Class

An S7 class for representing performance benchmarking reports.

## Usage

``` r
PerformanceReport(
  title = character(0),
  author = character(0),
  description = NULL,
  benchmarks = list(),
  metadata = list()
)
```

## Arguments

- title:

  Character string for report title

- author:

  Character string for report author

- description:

  Character string for report description

- benchmarks:

  List of benchmark results

- metadata:

  List of additional metadata

## Value

A PerformanceReport object

## Examples

``` r
if (FALSE) { # \dontrun{
report <- PerformanceReport(
  title = "Performance Report",
  author = "R User"
)
} # }
```
