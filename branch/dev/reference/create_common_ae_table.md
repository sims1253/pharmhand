# Create Most Common AE Table

Create Most Common AE Table

## Usage

``` r
create_common_ae_table(
  adae,
  trt_n,
  title = "Most Common Adverse Events",
  n_top = 15,
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

- n_top:

  Number of top PTs to show

- trt_var:

  Treatment variable name

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object
