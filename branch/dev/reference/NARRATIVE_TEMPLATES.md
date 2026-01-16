# Narrative Template System

Predefined templates for evidence narratives. Templates use placeholder
syntax compatible with
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html):
`{endpoint}`, `{estimate}`, `{ci}`, `{p_value}`, `{n_studies}`,
`{n_patients}`, `{effect_dir}`, etc.

## Usage

``` r
NARRATIVE_TEMPLATES
```

## Format

An object of class `list` of length 3.

## Details

Available templates:

- "iqwig":

  German IQWiG style with formal terminology

- "clinical":

  Clinical study report style (English)

- "plain":

  Plain text without specialized terminology
