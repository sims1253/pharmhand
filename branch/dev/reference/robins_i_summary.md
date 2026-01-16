# Create Summary Table of ROBINS-I Assessments

Combines results from multiple ROBINS-I assessments into a summary
table.

## Usage

``` r
robins_i_summary(results, include_justification = FALSE)
```

## Arguments

- results:

  List of ROBINSIResult objects.

- include_justification:

  Logical. Include justification text. Default: FALSE.

## Value

A data frame with study-level summary.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- list(
  assess_robins_i("S1", "Serious", "Low", "Low", "Low", "Low",
                 "Moderate", "Low",
                 d6_support = "Unblinded", outcome = "OS"),
  assess_robins_i("S2", "Low", "Low", "Low", "Low", "Low", "Low", "Low",
                 outcome = "OS"),
  assess_robins_i("S3", "Moderate", "Low", "Low", "Low", "Low", "Low", "Low",
                 outcome = "PFS")
)
summary_table <- robins_i_summary(results)
print(summary_table)
} # }
```
