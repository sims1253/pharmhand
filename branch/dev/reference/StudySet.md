# StudySet Class

An S7 class for representing a collection of studies for evidence
synthesis (meta-analysis, indirect comparison, network meta-analysis).

## Usage

``` r
StudySet(
  studies = list(),
  endpoint = NULL,
  comparison_type = "direct",
  common_comparator = NULL,
  characteristics = NULL,
  metadata = list()
)
```

## Arguments

- studies:

  List of Study objects

- endpoint:

  Endpoint object being synthesized

- comparison_type:

  Type of comparison: "direct", "indirect", "network"

- common_comparator:

  Common comparator for indirect/network comparisons

- characteristics:

  Data frame of study-level characteristics (optional, NULL by default)

- metadata:

  List of additional metadata

## Value

A StudySet object

## Examples

``` r
if (FALSE) { # \dontrun{
study_set <- StudySet(
  studies = list(study1, study2, study3),
  endpoint = os_endpoint,
  comparison_type = "direct"
)
} # }
```
