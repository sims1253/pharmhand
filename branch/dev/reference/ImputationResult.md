# ImputationResult Class

An S7 class for storing multiple imputation results from mice.

## Usage

``` r
ImputationResult(
  mice_object = NULL,
  m = 5L,
  method = "pmm",
  imputed_vars = character(0),
  n_missing = list(),
  original_data = data.frame(),
  metadata = list()
)
```

## Arguments

- mice_object:

  The mice mids object containing imputed datasets

- m:

  Integer number of imputations performed

- method:

  Character string or vector specifying imputation method(s)

- imputed_vars:

  Character vector of variables that were imputed

- n_missing:

  List with count of missing values per variable

- original_data:

  The original data frame before imputation

- metadata:

  List of additional metadata

## Value

An ImputationResult object

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(x = c(1, NA, 3), y = c(NA, 2, 3))
result <- perform_multiple_imputation(data, m = 5)
result@m # Number of imputations
result@imputed_vars # Variables that were imputed
} # }
```
