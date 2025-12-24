# OneArmStudy Class

An S7 class for representing and analyzing single-arm clinical studies.
Provides methods for calculating statistics, creating tables and plots,
and performing hypothesis tests.

## Usage

``` r
OneArmStudy(
  data = structure(list(), names = character(0), row.names = integer(0), class =
    "data.frame"),
  study_id = NULL,
  study_title = NULL,
  results = list(),
  metadata = list()
)
```

## Arguments

- data:

  A data frame containing the study data

- study_id:

  Character string for study identifier

- study_title:

  Character string for study title

- results:

  List of analysis results

- metadata:

  List of additional metadata

## Value

A OneArmStudy object
