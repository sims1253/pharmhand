# TwoArmStudy Class

An S7 class for representing and analyzing two-arm clinical studies.
Provides methods for comparing treatment groups, creating tables and
plots, and performing hypothesis tests.

## Usage

``` r
TwoArmStudy(
  data = structure(list(), names = character(0),
    row.names = integer(0), class = "data.frame"),
  group_var = "",
  study_id = character(0),
  study_title = character(0),
  results = list(),
  metadata = list()
)
```

## Arguments

- data:

  A data frame containing the study data

- group_var:

  Character string for treatment group variable

- study_id:

  Character string for study identifier

- study_title:

  Character string for study title

- results:

  List of analysis results

- metadata:

  List of additional metadata

## Value

A TwoArmStudy object
