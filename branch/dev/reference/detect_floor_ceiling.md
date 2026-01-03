# Detect Floor and Ceiling Effects

Identifies floor/ceiling effects in PRO data when \>15% of responses are
at the minimum or maximum possible scale values.

## Usage

``` r
detect_floor_ceiling(
  data,
  score_var,
  min_score,
  max_score,
  by_var = NULL,
  threshold = 0.15,
  subject_var = "USUBJID"
)
```

## Arguments

- data:

  Data frame containing PRO scores

- score_var:

  Character. Name of the score variable

- min_score:

  Numeric. Minimum possible score on the scale

- max_score:

  Numeric. Maximum possible score on the scale

- by_var:

  Character vector. Variables to group by (e.g., c("VISIT", "TRT01P"))

- threshold:

  Numeric. Threshold for flagging (default: 0.15 = 15%)

- subject_var:

  Character. Subject identifier (default: "USUBJID")

## Value

A data frame with columns:

- Grouping variables

- n: number of subjects with non-missing scores

- n_floor: count of subjects at minimum

- pct_floor: percentage at minimum

- floor_flag: TRUE if pct_floor \> threshold

- n_ceiling: count of subjects at maximum

- pct_ceiling: percentage at maximum

- ceiling_flag: TRUE if pct_ceiling \> threshold

## Examples

``` r
if (FALSE) { # \dontrun{
# Detect floor/ceiling effects in PRO scores
result <- detect_floor_ceiling(
  data = adqs,
  score_var = "AVAL",
  min_score = 0,
  max_score = 100
)
print(result)
} # }
```
