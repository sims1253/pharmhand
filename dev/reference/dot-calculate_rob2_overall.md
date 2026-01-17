# Internal Helper: Calculate Overall RoB 2 Judgment

Calculates the overall risk of bias judgment based on domain judgments
using RoB 2 algorithm.

## Usage

``` r
.calculate_rob2_overall(judgments)
```

## Arguments

- judgments:

  Character vector of 5 domain judgments.

## Value

Character string: "Low", "Some concerns", or "High".
