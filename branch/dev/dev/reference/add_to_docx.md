# Add content to a Word document

Generic function for adding clinical content to an officer Word
document. Uses multiple dispatch on both `doc` and `content`.

## Usage

``` r
add_to_docx(doc, content, ...)
```

## Arguments

- doc:

  An rdocx object from officer

- content:

  A ClinicalContent object

- ...:

  Additional arguments passed to methods

## Value

The modified rdocx object

## Examples

``` r
if (FALSE) { # \dontrun{
doc <- officer::read_docx()
doc <- add_to_docx(doc, clinical_table)
doc <- add_to_docx(doc, clinical_plot)
} # }
```
