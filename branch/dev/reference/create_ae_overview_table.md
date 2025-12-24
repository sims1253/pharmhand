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
  relationship_values = c("PROBABLE", "POSSIBLE", "RELATED"),
  discontinuation_action = "DRUG WITHDRAWN",
  fatal_outcome = "FATAL",
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

- relationship_values:

  Character vector of AEREL values considered as "related" to treatment.
  Default: c("PROBABLE", "POSSIBLE", "RELATED").

- discontinuation_action:

  Character value in AEACN indicating drug discontinuation. Default:
  "DRUG WITHDRAWN".

- fatal_outcome:

  Character value in AEOUT indicating death. Default: "FATAL".

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

A ClinicalTable object
