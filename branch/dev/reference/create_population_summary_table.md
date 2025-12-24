# Create Analysis Populations Summary Table

Create Analysis Populations Summary Table

## Usage

``` r
create_population_summary_table(
  adsl,
  title = "Analysis Populations",
  trt_var = "TRT01P",
  pop_flags = c("SAFFL"),
  pop_labels = c("Safety"),
  autofit = TRUE
)
```

## Arguments

- adsl:

  ADSL data frame

- title:

  Table title

- trt_var:

  Treatment variable name

- pop_flags:

  Character vector of population flag variables

- pop_labels:

  Character vector of labels for population flags

## Value

ClinicalTable object
