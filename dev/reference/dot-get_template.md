# Get Narrative Template

Retrieves a named template component for narrative generation.

## Usage

``` r
.get_template(template_name, component, language = "en")
```

## Arguments

- template_name:

  Character. Template name: "iqwig", "clinical", or "plain".

- component:

  Character. Template component: "effect_significant",
  "effect_nonsificant", "single_study", "heterogeneity\_*", "rob\_*",
  "grade\_\*".

- language:

  Character. Output language: "en" or "de". Default: "en".

## Value

Character string template.
