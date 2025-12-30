# Save ClinicalTable as PDF

Saves a ClinicalTable's flextable to a PDF file. Uses webshot2 for
high-quality HTML-to-PDF conversion if available, otherwise falls back
to image-based export via flextable::save_as_image().

## Usage

``` r
save_as_pdf(x, path = NULL)
```

## Arguments

- x:

  A ClinicalTable object

- path:

  Optional file path. If NULL, creates a temp file.

## Value

The file path where the PDF was saved

## Note

The image-based fallback may result in lower quality output compared to
native PDF rendering. For best results, install the webshot2 package.
