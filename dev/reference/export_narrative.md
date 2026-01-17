# Export Narrative to Text File

Writes a narrative (or list of narratives) to a plain text file.

## Usage

``` r
export_narrative(narrative, path, append = FALSE)
```

## Arguments

- narrative:

  Character string or list of narratives to write.

- path:

  File path for output.

- append:

  Logical. Append to existing file. Default: FALSE.

## Value

Invisible NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
# Export single narrative
export_narrative(narrative, "endpoint_os.txt")

# Export multiple narratives
narratives <- list(os = narrative_os, pfs = narrative_pfs)
export_narrative(narratives, "all_narratives.txt")
} # }
```
