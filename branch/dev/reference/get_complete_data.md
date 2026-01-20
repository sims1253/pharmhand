# Get Completed Datasets from Imputation

Extracts completed (imputed) datasets from an ImputationResult object.

## Usage

``` r
get_complete_data(imputation_result, action = c("list", "long", "stacked"))
```

## Arguments

- imputation_result:

  An ImputationResult object

- action:

  Character. How to return completed data:

  - "list": Return a list of m data frames (default)

  - "long": Return a single stacked data frame with .imp column

  - "stacked": Alias for "long"

## Value

Depending on action: a list of data frames or a single data frame

## Examples

``` r
if (FALSE) { # \dontrun{
imp <- perform_multiple_imputation(data, m = 5)

# Get as list
datasets <- get_complete_data(imp, action = "list")
length(datasets) # 5

# Get as stacked data frame
stacked <- get_complete_data(imp, action = "long")
table(stacked$.imp) # Shows imputation numbers
} # }
```
