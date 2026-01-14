# NMAResult Class

An S7 class for representing network meta-analysis results including
comparison tables, network structure, and metadata.

## Usage

``` r
NMAResult(
  comparisons = data.frame(),
  network = list(),
  model = "random",
  effect_measure = "hr",
  conf_level = 0.95,
  method = "bucher_chain",
  n_studies = NA_integer_,
  metadata = list()
)
```

## Arguments

- comparisons:

  Data frame of treatment comparisons vs reference

- network:

  List containing network structure:

  - treatments: character vector of treatment names

  - n_treatments: integer number of treatments

  - edges: data frame of study comparisons

  - reference: character reference treatment name

- model:

  Character string: "fixed" or "random"

- effect_measure:

  Character string: "hr", "or", "rr", "rd", "md", "smd"

- conf_level:

  Numeric confidence level (default: 0.95)

- method:

  Character string for NMA method

- n_studies:

  Integer number of studies in the network

- metadata:

  List of additional metadata

## Value

An NMAResult object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- NMAResult(
  comparisons = comparison_df,
  network = list(
    treatments = c("A", "B", "C"),
    n_treatments = 3L,
    edges = data.frame(treat1 = "A", treat2 = "B", n_studies = 2L),
    reference = "A"
  ),
  model = "random",
  effect_measure = "hr"
)
} # }
```
