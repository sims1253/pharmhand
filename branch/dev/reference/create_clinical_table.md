# Create Clinical Table (Factory Function)

Primary factory function for creating ClinicalTable objects with proper
styling. Supports multiple data types and themes, with optional custom
summarization logic.

## Usage

``` r
create_clinical_table(
  data,
  type,
  title = NULL,
  footnotes = character(),
  theme = c("hta", "iqwig", "gba", "clinical"),
  summary_fn = NULL,
  ...
)
```

## Arguments

- data:

  Data to display. Can be:

  - `ADaMData` object (uses `@data` or `@filtered_data`)

  - `LayeredTable` object (built using
    [`build_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/build_table.md))

  - `AnalysisResults` object (uses `@stats`)

  - Raw `data.frame`

- type:

  Character string for table type (e.g., "demographics", "ae_soc")

- title:

  Table title

- footnotes:

  Character vector of footnotes

- theme:

  Theme preset: "hta", "iqwig", "gba", or "clinical" (default: "hta")

- summary_fn:

  Optional function for custom summarization. Receives the extracted
  data and should return a data.frame. If NULL, data is used as-is.

- ...:

  Additional arguments passed to theme functions

## Value

A ClinicalTable object

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with data.frame
df <- data.frame(Treatment = c("A", "B"), N = c(100, 95))
table <- create_clinical_table(
  data = df,
  type = "summary",
  title = "Treatment Summary",
  footnotes = "ITT Population"
)

# With custom summary function
table <- create_clinical_table(
  data = adsl_data,
  type = "demographics",
  summary_fn = function(d) {
    d |> dplyr::summarise(N = dplyr::n(), .by = TRT01P)
  },
  theme = "iqwig"
)

# With ADaMData object
table <- create_clinical_table(
  data = ADaMData(adsl_df, domain = "ADSL"),
  type = "demographics",
  title = "Baseline Characteristics",
  theme = "gba"
)

# With LayeredTable object
demo_lt <- LayeredTable(
  data = adsl_df,
  trt_var = "TRT01P",
  layers = list(
    CountLayer(target_var = "SEX", label = "Sex"),
    DescriptiveLayer(target_var = "AGE", label = "Age (years)")
  )
)
table <- create_clinical_table(
  data = demo_lt,
  type = "demographics",
  title = "Demographics"
)
} # }
```
