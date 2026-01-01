# Create Responder Summary Table

Generates a response rate table with confidence intervals and treatment
comparisons (odds ratio, risk ratio, or risk difference) for binary
endpoints like ORR, CR, PR.

## Usage

``` r
create_responder_table(
  data,
  response_var = "AVALC",
  response_values = c("CR", "PR"),
  trt_var = "TRT01P",
  ref_group = NULL,
  ci_method = c("wilson", "exact", "wald"),
  comparison_type = c("OR", "RR", "RD"),
  conf_level = 0.95,
  title = "Response Rate Summary",
  autofit = TRUE
)
```

## Arguments

- data:

  ADaMData object or data frame

- response_var:

  Variable containing response values

- response_values:

  Character vector of values indicating response (default: c("CR", "PR")
  for tumor response)

- trt_var:

  Treatment variable name (default: "TRT01P")

- ref_group:

  Reference group for comparison. If NULL, uses first level of treatment
  variable.

- ci_method:

  Method for CI calculation: "wilson" (recommended), "exact"
  (Clopper-Pearson), or "wald"

- comparison_type:

  "OR" for odds ratio, "RR" for risk ratio, "RD" for risk difference

- conf_level:

  Confidence level (default: 0.95)

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable object with response summary

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic tumor response table
resp_table <- create_responder_table(
  data = adrs,
  response_var = "AVALC",
  response_values = c("CR", "PR"),
  title = "Best Overall Response"
)

# With risk ratio instead of odds ratio
resp_table <- create_responder_table(
  data = adrs,
  comparison_type = "RR",
  title = "Response Rate Summary"
)
} # }
```
