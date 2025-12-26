# Create Adverse Event Table

Unified function for generating standard adverse event tables for HTA
dossiers and clinical study reports. Supports multiple table types
through a single interface.

## Usage

``` r
create_ae_table(
  adae,
  adsl = NULL,
  type = c("overview", "soc", "soc_pt", "pt", "common", "severity", "relationship",
    "sae", "discontinuation", "deaths"),
  trt_var = "TRT01A",
  n_top = 15,
  soc = NULL,
  title = NULL,
  autofit = TRUE
)
```

## Arguments

- adae:

  ADAE data frame (ADaM Adverse Events dataset)

- adsl:

  ADSL data frame (optional, required for some table types like "deaths"
  or for denominator calculation)

- type:

  Character string specifying the table type:

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

- trt_var:

  Treatment variable name (default: "TRT01A")

- n_top:

  For type="common", number of top PTs to show (default: 15)

- soc:

  For type="pt", filter to specific SOC (optional)

- title:

  Table title (auto-generated if NULL)

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
# AE Overview
overview <- create_ae_table(adae, adsl, type = "overview")

# SOC table
soc_table <- create_ae_table(adae, adsl, type = "soc")

# SOC/PT hierarchical table
soc_pt <- create_ae_table(adae, adsl, type = "soc_pt")

# Most common AEs (top 20)
common <- create_ae_table(adae, adsl, type = "common", n_top = 20)

# SAE table
sae <- create_ae_table(adae, adsl, type = "sae")
} # }
```
