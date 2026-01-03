# Non-Inferiority Test

Tests non-inferiority of treatment vs comparator.

## Usage

``` r
test_non_inferiority(
  data,
  outcome_var,
  trt_var,
  ref_group,
  ni_margin,
  type = c("continuous", "binary"),
  higher_better = TRUE,
  conf_level = 0.975,
  method = c("wilson", "wald", "exact")
)
```

## Arguments

- data:

  Data frame

- outcome_var:

  Character. Outcome variable name

- trt_var:

  Character. Treatment variable name

- ref_group:

  Character. Reference group name

- ni_margin:

  Numeric. Non-inferiority margin (positive value)

- type:

  Character. "continuous" or "binary"

- higher_better:

  Logical. TRUE if higher values are better (default: TRUE)

- conf_level:

  Numeric. One-sided confidence level (default: 0.975 for 95% CI)

- method:

  Character. Method for binary endpoints only: "wald", "wilson", "exact"
  (default: "wilson"). Ignored for continuous endpoints.

## Value

List with:

- estimate: Point estimate of difference (trt - ref)

- ci_lower: Lower bound of one-sided CI

- ci_upper: Upper bound (may be Inf for one-sided)

- ni_margin: The margin used

- non_inferior: Logical. TRUE if lower CI \> -ni_margin (or upper \<
  margin if lower is worse)

- conclusion: Character summary

- method: Method used

## Details

For continuous endpoints: Tests if (mean_trt - mean_ref) + ni_margin \>
0 For binary endpoints: Tests if (prop_trt - prop_ref) + ni_margin \> 0

Non-inferiority is concluded if the lower bound of the one-sided CI for
the treatment difference exceeds -ni_margin.

For binary endpoints, the Wilson method uses the Newcombe-Wilson hybrid
CI approach. The Wald method provides a simpler alternative. For
regulatory submissions, verify the CI method aligns with agency
preferences.

## References

IQWiG Methods v8.0, Section 10.3.5, p. 217-218.

## Examples

``` r
if (FALSE) { # \dontrun{
# Non-inferiority test for continuous endpoint
result <- test_non_inferiority(
  data = adeff,
  outcome_var = "CHG",
  trt_var = "TRT01P",
  ref_group = "Active Control",
  ni_margin = 0.5,
  type = "continuous"
)

# Non-inferiority test for binary endpoint
result <- test_non_inferiority(
  data = adrs,
  outcome_var = "AVALC",
  trt_var = "TRT01P",
  ref_group = "Active Control",
  ni_margin = 0.10,
  type = "binary"
)
} # }
```
