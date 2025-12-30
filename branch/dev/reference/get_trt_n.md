# Get Treatment Group Counts

Extract or compute treatment group counts from ADaMData or data frame.

## Usage

``` r
get_trt_n(data, trt_var = "TRT01P", population = NULL, subject_var = "USUBJID")
```

## Arguments

- data:

  ADaMData object or data frame

- trt_var:

  Treatment variable name (used only for data frames, ignored for
  ADaMData which uses its own trt_var)

- population:

  Population to filter by (used only for data frames, filters by
  `{population}FL == "Y"`)

- subject_var:

  Subject ID variable (used only for data frames)

## Value

Data frame with treatment variable and N column

## Examples

``` r
if (FALSE) { # \dontrun{
# From ADaMData - uses stored trt_var and population
adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
trt_n <- get_trt_n(adam)

# From data frame - must specify parameters
trt_n <- get_trt_n(adsl, trt_var = "TRT01P", population = "SAF")
} # }
```
