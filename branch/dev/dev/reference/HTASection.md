# HTASection Class

An S7 class for representing HTA-specific report sections with support
for AMNOG/G-BA dossier requirements.

## Usage

``` r
HTASection(
  title = NULL,
  section_type = character(0),
  content = list(),
  metadata = list(),
  endpoint = NULL,
  comparator = "",
  population = ""
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

- endpoint:

  HTAEndpoint object for this section

- comparator:

  Character string for comparator description

- population:

  Character string for population description

## Value

An HTASection object

## Examples

``` r
if (FALSE) { # \dontrun{
section <- HTASection(
  title = "Primary Efficacy Analysis",
  section_type = "efficacy",
  comparator = "Placebo",
  population = "ITT"
)
} # }
```
