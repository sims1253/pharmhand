# Create Competing Risk Table

Creates a clinical table from competing risk analysis results.

## Usage

``` r
create_competing_risk_table(
  result,
  title = "Competing Risk Analysis",
  subtitle = NULL,
  footnotes = NULL,
  autofit = TRUE
)
```

## Arguments

- result:

  A CompetingRiskResult object

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
table <- create_competing_risk_table(
  result, title = "Competing Risk Analysis"
)
} # }
```
