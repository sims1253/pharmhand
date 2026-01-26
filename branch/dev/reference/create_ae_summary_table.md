# Create Adverse Event Summary Table

Generate AE summary tables for clinical study reports.

## Usage

``` r
create_ae_summary_table(
  data,
  adsl = NULL,
  type = c("overview", "soc", "soc_pt", "pt", "common", "severity", "relationship",
    "sae", "discontinuation", "deaths", "comparison"),
  trt_var = ph_default("trt_var"),
  n_top = ph_default("n_top"),
  soc = NULL,
  title = NULL,
  footnotes = character(),
  theme = "hta",
  ref_group = NULL,
  by = "pt",
  threshold = 0,
  sort_by = "incidence",
  conf_level = ph_default("conf_level"),
  include_nnh = TRUE,
  soc_order = NULL,
  severity_levels = c("MILD", "MODERATE", "SEVERE"),
  ...
)
```

## Arguments

- data:

  ADAE data frame or ADaMData object containing ADaM Adverse Events
  dataset

- adsl:

  ADSL data frame or ADaMData object (optional, required for some table
  types like "deaths", "comparison", or for denominator calculation)

- type:

  Character string specifying table type:

  - "overview" - Summary of TEAEs, related AEs, SAEs, discontinuations

  - "soc" - AEs by System Organ Class

  - "soc_pt" - AEs by SOC and Preferred Term (hierarchical)

  - "pt" - AEs by Preferred Term only

  - "common" - Most frequently reported AEs

  - "severity" - AEs by maximum severity

  - "relationship" - AEs by relationship to study drug

  - "sae" - Serious Adverse Events

  - "discontinuation" - AEs leading to discontinuation

  - "deaths" - Deaths summary (requires adsl)

  - "comparison" - AE comparison with RD/RR (requires adsl)

- trt_var:

  Treatment variable name (default: "TRT01P")

- n_top:

  For type="common", number of top PTs to show (default: 15)

- soc:

  For type="pt", filter to specific SOC (optional)

- title:

  Table title (auto-generated if NULL)

- footnotes:

  Character vector of footnotes (optional)

- theme:

  Theme for table styling (default: "hta")

- ref_group:

  For type="comparison", reference group for comparisons

- by:

  For type="comparison", grouping level: "soc", "pt", or "overall"

- threshold:

  For type="comparison", minimum incidence pct (default: 0)

- sort_by:

  For type="comparison", sort by "rd", "rr", or "incidence"

- conf_level:

  For type="comparison", confidence level (default: 0.95)

- include_nnh:

  For type="comparison", include NNH column (default: TRUE)

- soc_order:

  For type="soc" or type="soc_pt", custom ordering of SOCs (character
  vector). If NULL, SOCs are sorted alphabetically (default: NULL)

- severity_levels:

  For type="severity", severity levels ordering (character vector).
  Default: c("MILD", "MODERATE", "SEVERE")

- ...:

  Additional arguments passed to
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
# AE Overview
overview <- create_ae_summary_table(data, adsl, type = "overview")

# SOC table
soc_table <- create_ae_summary_table(data, adsl, type = "soc")

# SOC/PT hierarchical table
soc_pt <- create_ae_summary_table(data, adsl, type = "soc_pt")

# Most common AEs (top 20)
common <- create_ae_summary_table(data, adsl, type = "common", n_top = 20)

# SAE table
sae <- create_ae_summary_table(data, adsl, type = "sae")

# AE comparison with risk differences
comparison <- create_ae_summary_table(
  data, adsl,
  type = "comparison",
  ref_group = "Placebo",
  by = "pt",
  threshold = 5
)

# SOC table with custom ordering
soc_ordered <- create_ae_summary_table(
  data, adsl,
  type = "soc",
  soc_order = c(
    "Infections",
    "Nervous system disorders",
    "Gastrointestinal disorders"
  )
)
} # }
```
