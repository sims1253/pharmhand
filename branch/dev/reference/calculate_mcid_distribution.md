# Calculate MCID using Distribution-Based Methods

Calculates MCID estimates using distribution-based approaches.

## Usage

``` r
calculate_mcid_distribution(
  data,
  score_var,
  reliability = NULL,
  methods = c("half_sd", "one_sem", "third_sd", "fifth_sd")
)
```

## Arguments

- data:

  Data frame with baseline PRO scores

- score_var:

  Character. Name of the baseline score variable

- reliability:

  Numeric. Test-retest reliability for SEM method (default: NULL, SEM
  not calculated)

- methods:

  Character vector. Methods to use: "half_sd", "one_sem", "third_sd",
  "fifth_sd" (default: all)

## Value

List with MCID estimates for each method
