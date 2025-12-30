# Get Filtered Data

Extract filtered data from ADaMData or apply population filter to a data
frame. For ADaMData objects, returns the `filtered_data` property. For
data frames, applies the specified population filter.

## Usage

``` r
get_filtered_data(data, population = NULL)
```

## Arguments

- data:

  ADaMData object or data frame

- population:

  Population to filter by (used only for data frames, filters by
  `{population}FL == "Y"`)

## Value

Filtered data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# From ADaMData - uses stored population
adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
df <- get_filtered_data(adam)

# From data frame - must specify population
df <- get_filtered_data(adsl, population = "SAF")
} # }
```
