# Quick Safety Report

High-level convenience function to generate a safety report from
ADAE/ADSL data in a single call. Creates multiple safety tables and
writes to Word.

## Usage

``` r
quick_safety_report(
  data,
  adsl = NULL,
  output,
  title = "Safety Analysis",
  trt_var = ph_default("trt_var"),
  include_overview = TRUE,
  include_soc = TRUE,
  include_soc_pt = FALSE,
  include_sae = TRUE,
  include_title = TRUE,
  ...
)
```

## Arguments

- data:

  Data frame containing ADAE data

- adsl:

  Data frame containing ADSL data (optional, for denominators)

- output:

  Character string output file path (e.g., "safety.docx")

- title:

  Character string report title

- trt_var:

  Character string treatment variable name

- include_overview:

  Logical, include AE overview table

- include_soc:

  Logical, include SOC table

- include_soc_pt:

  Logical, include SOC/PT hierarchical table

- include_sae:

  Logical, include SAE table

- include_title:

  Logical, include title page

- ...:

  Additional arguments passed to table functions

## Value

Invisibly returns the ClinicalReport object

## Examples

``` r
if (FALSE) { # \dontrun{
# Quick safety report with default tables
quick_safety_report(adae_df, adsl_df, "safety.docx")

# Custom configuration
quick_safety_report(
  adae_df, adsl_df,
  "safety.docx",
  title = "Study XYZ Safety Analysis",
  include_soc_pt = FALSE
)
} # }
```
