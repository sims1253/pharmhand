# Create Medical History Table

Create Medical History Table

## Usage

``` r
create_medical_history_table(
  data,
  adsl,
  title = "Medical History by Body System",
  trt_var = "TRT01P",
  soc_var = "MHBODSYS",
  autofit = TRUE
)
```

## Arguments

- data:

  ADMH data frame or ADaMData object

- adsl:

  ADSL data frame or ADaMData object for denominators

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
