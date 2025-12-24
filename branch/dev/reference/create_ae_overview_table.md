# Create AE Overview Table

Generates a standard adverse event overview table including TEAEs,
related AEs, SAEs, and discontinuations.

## Usage

``` r
create_ae_overview_table(
  adae,
  trt_n,
  title = "Overview of Adverse Events",
  trt_var = "TRT01A",
  subjid_var = "USUBJID",
  autofit = TRUE
)
```

## Arguments

- adae:

  ADAE data frame

- trt_n:

  Treatment group counts data frame (must contain TRT01A and N columns)

- title:

  Table title (default: "Overview of Adverse Events")

- trt_var:

  Treatment variable name (default: "TRT01A")

- subjid_var:

  Subject ID variable name (default: "USUBJID")

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable object
