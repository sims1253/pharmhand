# Create AE Table with Full MedDRA Hierarchy

Creates an adverse event summary table using the full MedDRA hierarchy:
System Organ Class (SOC) → High Level Group Term (HLGT) → High Level
Term (HLT) → Preferred Term (PT).

## Usage

``` r
create_ae_hierarchy_table(
  data,
  adsl,
  trt_var = "TRT01P",
  soc_var = "AEBODSYS",
  hlgt_var = "AEHLTGT",
  hlt_var = "AEHLT",
  pt_var = "AEDECOD",
  levels = c("soc", "pt"),
  min_pct = 0,
  sort_by = c("frequency", "alphabetical"),
  title = NULL,
  ...
)
```

## Arguments

- data:

  ADAE dataset (data frame or ADaMData object)

- adsl:

  ADSL dataset for denominators (data frame or ADaMData object)

- trt_var:

  Treatment variable. Default: "TRT01P"

- soc_var:

  SOC variable. Default: "AEBODSYS"

- hlgt_var:

  HLGT variable. Default: "AEHLTGT"

- hlt_var:

  HLT variable. Default: "AEHLT"

- pt_var:

  PT variable. Default: "AEDECOD"

- levels:

  Character vector of hierarchy levels to include. Options: "soc",
  "hlgt", "hlt", "pt". Default: c("soc", "pt")

- min_pct:

  Numeric. Minimum percentage to display. Default: 0

- sort_by:

  Character. Sort by "alphabetical" or "frequency". Default: "frequency"

- title:

  Character. Table title. Default: NULL (auto-generated)

- ...:

  Additional arguments passed to
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)

## Value

ClinicalTable with hierarchical AE summary

## Examples

``` r
if (FALSE) { # \dontrun{
# Create AE hierarchy table
table <- create_ae_hierarchy_table(
  data = adae,
  adsl = adsl,
  levels = c("soc", "pt"),
  min_pct = 5
)
table@flextable
} # }
```
