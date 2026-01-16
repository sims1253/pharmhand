# Create Summary Table of RoB 2 Assessments

Combines results from multiple RoB 2 assessments into a summary table.

## Usage

``` r
rob2_summary(results, include_justification = FALSE)
```

## Arguments

- results:

  List of RoB2Result objects.

- include_justification:

  Logical. Include justification text. Default: FALSE.

## Value

A data frame with study-level summary.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- list(
  assess_rob2("S1", "Low", "Low", "Low", "Some concerns", "Low",
              d4_support = "Unblinded", outcome = "OS"),
  assess_rob2("S2", "Low", "Low", "Low", "Low", "Low", outcome = "OS")
)
summary_table <- rob2_summary(results)
} # }
```
