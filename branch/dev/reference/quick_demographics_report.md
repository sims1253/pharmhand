# Quick Demographics Report

High-level convenience function to generate a demographics report from
ADSL data in a single call. Creates demographics table and writes to
Word.

## Usage

``` r
quick_demographics_report(
  data,
  output,
  title = "Demographics and Baseline Characteristics",
  trt_var = ph_default("trt_var"),
  include_title = TRUE,
  ...
)
```

## Arguments

- data:

  Data frame or ADaMData object containing ADSL data

- output:

  Character string output file path (e.g., "demographics.docx")

- title:

  Character string report title

- trt_var:

  Character string treatment variable name

- include_title:

  Logical, include title page

- ...:

  Additional arguments passed to create_demographics_table()

## Value

Invisibly returns the ClinicalReport object

## Examples

``` r
if (FALSE) { # \dontrun{
# Quick demographics report in one line
quick_demographics_report(adsl_df, "demo.docx")

# With custom settings
quick_demographics_report(
  adsl_df,
  "demo.docx",
  title = "Study XYZ Demographics",
  trt_var = "ARM"
)
} # }
```
