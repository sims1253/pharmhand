# Create Demographics Table

Standard demographics and baseline characteristics table.

## Usage

``` r
create_demographics_table(
  adsl_data,
  title = "Demographics and Baseline Characteristics",
  trt_var = "TRT01P",
  age_var = "AGE",
  age_grp_var = "AGEGR1",
  sex_var = "SEX",
  race_var = "RACE",
  ethnic_var = "ETHNIC",
  country_var = "COUNTRY",
  autofit = TRUE
)
```

## Arguments

- adsl_data:

  ADaMData object containing ADSL

- title:

  Table title (default: "Demographics and Baseline")

- trt_var:

  Treatment variable name (default: "TRT01P")

- age_var:

  Age variable name (default: "AGE")

- age_grp_var:

  Age group variable name (default: "AGEGR1")

- sex_var:

  Sex variable name (default: "SEX")

- race_var:

  Race variable name (default: "RACE")

- ethnic_var:

  Ethnicity variable name (default: "ETHNIC")

- country_var:

  Country variable name (default: "COUNTRY")

- autofit:

  Logical, perform expensive layout calculations (default: TRUE)

## Value

A ClinicalTable object
