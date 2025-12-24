# SOCPTSection Class

An S7 class for representing SOC-PT (System Organ Class - Preferred
Term) sections in adverse events reports.

## Usage

``` r
SOCPTSection(
  title = NULL,
  section_type = character(0),
  content = list(),
  metadata = list(),
  soc_var = character(0),
  pt_var = character(0),
  group_var = character(0)
)
```

## Arguments

- title:

  Character string for section title

- section_type:

  Character string for section type

- content:

  List of ClinicalContent objects

- metadata:

  List of additional metadata

- soc_var:

  Character string for SOC variable name

- pt_var:

  Character string for PT variable name

- group_var:

  Character string for treatment group variable

## Value

A SOCPTSection object

## Examples

``` r
if (FALSE) { # \dontrun{
section <- SOCPTSection(
  title = "Adverse Events",
  soc_var = "AEBODSYS",
  pt_var = "AEDECOD",
  group_var = "TRT"
)
} # }
```
