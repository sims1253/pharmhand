# Export Evidence Table

Exports an evidence summary table to Word, HTML, or Excel format.

## Usage

``` r
export_evidence_table(table, file, title = NULL, ...)
```

## Arguments

- table:

  ClinicalTable object from
  [`create_evidence_summary_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_evidence_summary_table.md)
  or
  [`create_study_characteristics_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_study_characteristics_table.md).

- file:

  Character string for output file path. Extension determines format:

  - `.docx`: Microsoft Word document

  - `.html`: HTML file

  - `.xlsx`: Microsoft Excel file

- title:

  Character string for document title (used in Word/HTML exports).

- ...:

  Additional arguments passed to export functions:

  - For Word: `template` (officer template path), `header_footer` (list
    with header/footer text)

  - For HTML: `css` (custom CSS string), `standalone` (wrap in HTML
    boilerplate)

  - For Excel: `sheet_name` (worksheet name), `append` (logical)

## Value

Invisible NULL. Writes file to disk.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create table
table <- create_evidence_summary_table(endpoints)

# Export to Word
export_evidence_table(table, "evidence_summary.docx")

# Export to HTML
export_evidence_table(table, "evidence_summary.html")

# Export to Excel
export_evidence_table(table, "evidence_summary.xlsx")
} # }
```
