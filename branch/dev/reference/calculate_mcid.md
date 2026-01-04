# Calculate MCID (Combined Approach)

Wrapper function to calculate MCID using anchor-based,
distribution-based, or both approaches.

## Usage

``` r
calculate_mcid(
  data,
  score_var,
  anchor_var = NULL,
  baseline_var = NULL,
  method = c("both", "anchor", "distribution"),
  reliability = NULL,
  anchor_minimal = "Minimally Improved",
  conf_level = 0.95
)
```

## Arguments

- data:

  Data frame with PRO data

- score_var:

  Character. Name of the score/change variable

- anchor_var:

  Character. Name of anchor variable (required for anchor method)

- baseline_var:

  Character. Name of baseline score variable (for distribution method)

- method:

  Character. "anchor", "distribution", or "both" (default: "both")

- reliability:

  Numeric. Test-retest reliability for SEM calculation

- anchor_minimal:

  Character vector. Anchor values for minimal improvement

- conf_level:

  Numeric. Confidence level (default: 0.95)

## Value

List with MCID results from requested method(s)
