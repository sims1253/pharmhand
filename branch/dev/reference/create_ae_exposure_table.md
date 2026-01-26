# Create Exposure-Adjusted AE Table

Generate AE table with incidence rates per X patient-years.

## Usage

``` r
create_ae_exposure_table(
  data,
  adsl,
  exposure_var = "TRTDURD",
  trt_var = "TRT01P",
  by = c("pt", "soc", "overall"),
  time_unit = c("days", "weeks", "months"),
  per = 100,
  conf_level = 0.95,
  threshold = 0,
  title = NULL,
  ...
)
```

## Arguments

- data:

  ADAE data frame or ADaMData object

- adsl:

  ADSL data frame or ADaMData object (must contain exposure duration)

- exposure_var:

  Character. Exposure duration variable (default: "TRTDURD")

- trt_var:

  Character. Treatment variable (default: "TRT01P")

- by:

  Character. "soc", "pt", or "overall" (default: "pt")

- time_unit:

  Character. Unit of the exposure_var values: "days" (default for
  TRTDURD), "weeks", or "months". Used to convert exposure to
  patient-years.

- per:

  Numeric. Rate per X patient-years (default: 100)

- conf_level:

  Numeric. Confidence level (default: 0.95)

- threshold:

  Numeric. Minimum incidence to include (default: 0)

- title:

  Character. Table title

- ...:

  Additional arguments passed to
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)

## Value

ClinicalTable with IDR columns

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- create_ae_exposure_table(data, adsl, by = "pt")
} # }
```
