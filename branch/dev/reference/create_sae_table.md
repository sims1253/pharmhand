# Create SAE Table

Create SAE Table

## Usage

``` r
create_sae_table(
  adae,
  trt_n,
  title = "Serious Adverse Events",
  trt_var = "TRT01A",
  autofit = TRUE
)
```

## Arguments

- adae:

  ADAE data frame

- trt_n:

  Treatment group counts

- title:

  Table title

- trt_var:

  Treatment variable name

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
