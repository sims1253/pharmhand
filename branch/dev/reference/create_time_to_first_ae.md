# Create Time-to-First AE Analysis

Time-to-event analysis for first occurrence of adverse events.

## Usage

``` r
create_time_to_first_ae(
  adae,
  adsl,
  ae_filter = NULL,
  trt_var = "TRT01P",
  ref_group = NULL,
  time_var = "ASTDY",
  censor_var = "TRTDURD",
  conf_level = 0.95,
  title = NULL,
  autofit = TRUE
)
```

## Arguments

- adae:

  ADAE data frame with time variable (e.g., ASTDY)

- adsl:

  ADSL data frame for denominators and censoring

- ae_filter:

  Expression to filter specific AEs (e.g., AEBODSYS == "Infections")

- trt_var:

  Treatment variable (default: "TRT01P")

- ref_group:

  Reference group for HR calculation

- time_var:

  Time variable in ADAE (default: "ASTDY")

- censor_var:

  Censoring variable in ADSL (default: "TRTDURD")

- conf_level:

  Confidence level (default: 0.95)

- title:

  Table title

- autofit:

  Logical (default: TRUE)

## Value

List with:

- table: ClinicalTable with KM estimates

- plot: ClinicalPlot with KM curves

- hr: Hazard ratio from Cox model

## Examples

``` r
if (FALSE) { # \dontrun{
result <- create_time_to_first_ae(
  adae = adae,
  adsl = adsl,
  ae_filter = AEBODSYS == "Infections",
  ref_group = "Placebo"
)
} # }
```
