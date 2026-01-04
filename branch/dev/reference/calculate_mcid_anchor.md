# Calculate MCID using Anchor-Based Method

Calculates the Minimal Clinically Important Difference using an anchor
variable (e.g., Patient Global Impression of Change).

## Usage

``` r
calculate_mcid_anchor(
  data,
  score_var,
  anchor_var,
  anchor_positive = c("Minimally Improved", "Much Improved"),
  anchor_minimal = "Minimally Improved",
  conf_level = 0.95
)
```

## Arguments

- data:

  Data frame with PRO scores and anchor variable

- score_var:

  Character. Name of the PRO score change variable

- anchor_var:

  Character. Name of the anchor variable

- anchor_positive:

  Character vector. Values indicating positive/improved response on
  anchor (default: c("Minimally Improved", "Much Improved"))

- anchor_minimal:

  Character vector. Values indicating minimal improvement used for MCID
  (default: "Minimally Improved")

- conf_level:

  Numeric. Confidence level for CI (default: 0.95)

## Value

List with mcid estimate, ci, n, and method
