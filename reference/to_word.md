# Convert clinical content to Word format

Generic function that converts clinical content (tables, plots) to a
format suitable for Word documents. Dispatches on the class of `x`.

## Usage

``` r
to_word(x, ...)
```

## Arguments

- x:

  A ClinicalContent object (ClinicalTable or ClinicalPlot)

- ...:

  Additional arguments passed to methods

## Value

A Word-compatible object (flextable or external_img)

## Examples

``` r
if (FALSE) { # \dontrun{
# For a table
word_obj <- to_word(clinical_table)

# For a plot
word_obj <- to_word(clinical_plot)
} # }
```
