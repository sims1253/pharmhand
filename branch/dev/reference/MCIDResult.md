# MCIDResult Class

An S7 class for representing Minimal Clinically Important Difference
(MCID) calculation results from anchor-based, distribution-based, or
combined methods.

## Usage

``` r
MCIDResult(
  anchor = list(),
  distribution = list(),
  method = character(0),
  metadata = list()
)
```

## Arguments

- anchor:

  List or NULL. Anchor-based MCID results containing mcid, ci, n, se,
  and method

- distribution:

  List or NULL. Distribution-based MCID results containing half_sd,
  one_sem, third_sd, fifth_sd, sd, n, and method

- method:

  Character string: "anchor", "distribution", or "both"

- metadata:

  List of additional metadata

## Value

An MCIDResult object

## Examples

``` r
if (FALSE) { # \dontrun{
# Anchor-based MCID result
result <- MCIDResult(
  anchor = list(mcid = 2.5, ci = c(1.8, 3.2), n = 45L),
  distribution = NULL,
  method = "anchor"
)

# Access components
result@anchor$mcid
result@method
} # }
```
