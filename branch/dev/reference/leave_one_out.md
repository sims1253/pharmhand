# Perform Leave-One-Out Sensitivity Analysis

Recalculates meta-analysis results leaving out each study one at a time
to assess the influence of individual studies.

## Usage

``` r
leave_one_out(meta_result = NULL, yi = NULL, sei = NULL)
```

## Arguments

- meta_result:

  A MetaResult object from meta_analysis()

- yi:

  Numeric vector of effect estimates (optional if in meta_result)

- sei:

  Numeric vector of standard errors (optional if in meta_result)

## Value

List with leave-one-out results and influence statistics
