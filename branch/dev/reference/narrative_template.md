# Narrative Template

Manages narrative templates for evidence reporting. Allows access to
predefined templates or creation of custom templates.

## Usage

``` r
narrative_template(
  template = "iqwig",
  component = NULL,
  language = c("en", "de"),
  list_templates = FALSE
)
```

## Arguments

- template:

  Character. Template name: "iqwig", "clinical", "plain", or a custom
  template string.

- component:

  Character. Template component to retrieve (for predefined templates
  only).

- language:

  Character. Language for output: "en" or "de". Default: "en".

- list_templates:

  Logical. If TRUE, returns list of available templates. Default: FALSE.

## Value

For `list_templates = TRUE`: list of available template names. For
`list_templates = FALSE`: character template string.

## Examples

``` r
# List available templates
narrative_template(list_templates = TRUE)
#> [1] "iqwig"    "clinical" "plain"   

# Get specific template component
narrative_template("iqwig", "effect_significant")
#> [1] "Für den Endpunkt {endpoint} zeigt die Metaanalyse von {n_studies} Studien (N={n_patients}) einen statistisch signifikanten Effekt zugunsten der Prüfintervention ({effect_measure_label} {estimate} [{ci_level}%-KI: {ci_formatted}; p={p_formatted}]). "

# Get German template
narrative_template("iqwig", "effect_significant", language = "de")
#> [1] "Für den Endpunkt {endpoint} zeigt die Metaanalyse von {n_studies} Studien (N={n_patients}) einen statistisch signifikanten Effekt zugunsten der Prüfintervention ({effect_measure_label} {estimate} [{ci_level}%-KI: {ci_formatted}; p={p_formatted}]). "
```
