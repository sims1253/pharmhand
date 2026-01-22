# Create Bayesian Meta-Analysis Table for Few Studies

Creates a clinical table from Bayesian meta-analysis results for few
studies.

## Usage

``` r
create_bayesian_few_table(
  result,
  title = "Bayesian Meta-Analysis for Few Studies",
  subtitle = NULL,
  footnotes = NULL,
  autofit = TRUE
)
```

## Arguments

- result:

  A BayesianMetaFewResult object

- title:

  Table title

- subtitle:

  Optional subtitle

- footnotes:

  Optional footnotes

- autofit:

  Logical. Whether to autofit column widths

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
table <- create_bayesian_few_table(
  result, title = "Bayesian Meta-Analysis (Few Studies)"
)
} # }
```
