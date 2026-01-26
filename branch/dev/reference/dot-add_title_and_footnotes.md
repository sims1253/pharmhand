# Add title and footnotes to flextable

Internal helper function to add title and footnotes to a flextable with
consistent styling.

## Usage

``` r
.add_title_and_footnotes(ft, title, footnotes)
```

## Arguments

- ft:

  A flextable object

- title:

  Character string title (NULL, NA, or empty string to skip)

- footnotes:

  Character vector of footnotes

## Value

The modified flextable object
