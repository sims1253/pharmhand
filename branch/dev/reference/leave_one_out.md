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

A list with components:

- results:

  Data frame with estimates when each study is excluded

- influential_studies:

  Character vector of influential studies

- n_influential:

  Number of influential studies

- effect_measure:

  Effect measure used

- model:

  Model type (fixed/random)
