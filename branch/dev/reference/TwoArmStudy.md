# TwoArmStudy Class

An S7 class for representing and analyzing two-arm clinical studies.
Provides methods for comparing treatment groups, creating tables and
plots, and performing hypothesis tests.

## Usage

``` r
TwoArmStudy(
  data = structure(list(), names = character(0), row.names = integer(0), class =
    "data.frame"),
  group_var = "",
  study_id = NULL,
  study_title = NULL,
  results = list(),
  metadata = list()
)
```
