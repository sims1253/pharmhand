# Get Total Subject Count

Extract total subject count from ADaMData or calculate from data frame.
For ADaMData, uses the computed `subject_n` property which respects
population filters.

## Usage

``` r
get_subject_n(data, population = NULL, subject_var = "USUBJID")
```

## Arguments

- data:

  ADaMData object or data frame

- population:

  Population filter for data frames (optional)

- subject_var:

  Subject variable name for data frames (optional)

## Value

Integer count of distinct subjects

## Examples

``` r
if (FALSE) { # \dontrun{
# From ADaMData - uses computed property
adam <- ADaMData(data = adsl, domain = "ADSL", population = "FAS")
n_subj <- get_subject_n(adam)

# From data frame
n_subj <- get_subject_n(adsl, population = "FAS")
} # }
```
