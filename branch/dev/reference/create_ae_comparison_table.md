# Create AE Comparison Table with Risk Differences

Generate AE table with statistical comparisons (risk difference, risk
ratio) between treatment groups. Essential for GBA/AMNOG safety
assessments.

## Usage

``` r
create_ae_comparison_table(
  data,
  adsl,
  ref_group,
  trt_var = "TRT01P",
  by = c("pt", "soc", "overall"),
  threshold = 0,
  sort_by = c("incidence", "rd", "rr"),
  conf_level = ph_default("conf_level"),
  include_nnh = TRUE,
  title = NULL,
  footnotes = character(),
  theme = "hta",
  ...
)
```

## Arguments

- data:

  ADAE data frame or ADaMData object containing ADaM Adverse Events
  dataset

- adsl:

  ADSL data frame or ADaMData object for denominators

- ref_group:

  Character. Reference (control) group for comparison

- trt_var:

  Treatment variable name (default: "TRT01P")

- by:

  Character. Grouping level: "soc", "pt", or "overall" (default: "pt")

- threshold:

  Numeric. Minimum incidence pct in any group (default: 0)

- sort_by:

  Character. Sort by "rd", "rr", or "incidence" (default: "incidence")

- conf_level:

  Confidence level for intervals (default: 0.95)

- include_nnh:

  Logical. Include NNH column (default: TRUE)

- title:

  Table title (auto-generated if NULL)

- footnotes:

  Character vector of footnotes (optional)

- theme:

  Theme for table styling (default: "hta")

- ...:

  Additional arguments passed to
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)

## Value

ClinicalTable with columns for each group's n(%), RD, 95% CI, NNH, RR,
p-value

## Examples

``` r
if (FALSE) { # \dontrun{
# AE comparison by PT
ae_comp <- create_ae_comparison_table(
  data = adae,
  adsl = adsl,
  ref_group = "Placebo",
  by = "pt"
)

# AE comparison by SOC with 5% threshold
ae_comp_soc <- create_ae_comparison_table(
  data = adae,
  adsl = adsl,
  ref_group = "Placebo",
  by = "soc",
  threshold = 5
)
} # }
```
