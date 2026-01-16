# Internal Helper: Calculate Overall ROBINS-I Judgment

Calculates the overall risk of bias judgment based on domain judgments
using ROBINS-I algorithm.

## Usage

``` r
.calculate_robins_i_overall(judgments)
```

## Arguments

- judgments:

  Character vector of 7 domain judgments.

## Value

Character string: "Low", "Moderate", "Serious", "Critical", or "No
information".
