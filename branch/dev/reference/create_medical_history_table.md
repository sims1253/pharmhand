# Create Medical History Table

Create Medical History Table

## Usage

``` r
create_medical_history_table(
  adsl,
  admh,
  title = "Medical History by Body System",
  trt_var = "TRT01P",
  soc_var = "MHBODSYS",
  autofit = TRUE
)
```

## Arguments

- adsl:

  ADSL data frame

- admh:

  ADMH data frame

- title:

  Table title

- trt_var:

  Treatment variable name

- soc_var:

  SOC variable name for MH

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
