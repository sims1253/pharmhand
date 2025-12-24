# ADaMData Class

Base class for ADaM datasets with population filters.

## Usage

``` r
ADaMData(
  data = structure(list(), names = character(0), row.names = integer(0), class =
    "data.frame"),
  domain = "",
  population = "FAS",
  subject_var = "USUBJID",
  trt_var = "TRT01P",
  metadata = list()
)
```

## Arguments

- data:

  A data frame containing the ADaM dataset

- domain:

  Character string for the ADaM domain (e.g., "ADSL", "ADAE")

- population:

  Character string for population filter (default: "FAS")

- subject_var:

  Character string for subject ID variable (default: "USUBJID")

- trt_var:

  Character string for treatment variable (default: "TRT01P")

- metadata:

  List of additional metadata

## Value

An ADaMData object
