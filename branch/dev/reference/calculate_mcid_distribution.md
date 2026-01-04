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

A list with MCID estimates:

- half_sd:

  0.5 \* SD of baseline scores

- third_sd:

  0.33 \* SD of baseline scores

- fifth_sd:

  0.2 \* SD of baseline scores

- one_sem:

  1 \* SEM (requires reliability)

- sd:

  Standard deviation of baseline scores

- n:

  Number of observations

- reliability:

  Test-retest reliability if provided

## Examples

``` r
if (FALSE) { # \dontrun{
# Distribution-based MCID for a PRO measure
result <- calculate_mcid_distribution(
  data = pro_data,
  score_var = "AVAL",
  reliability = 0.85
)

# Access 0.5 SD MCID
result$half_sd
} # }
```
