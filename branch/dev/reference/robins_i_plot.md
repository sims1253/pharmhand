# Create ROBINS-I Summary Plot

Generates a visualization of ROBINS-I assessments across multiple
studies using a stacked bar chart showing the distribution of domain
judgments.

## Usage

``` r
robins_i_plot(
  results,
  title = "Risk of Bias Assessment (ROBINS-I)",
  colors = c(Low = "#2ecc71", Moderate = "#f1c40f", Serious = "#e67e22", Critical =
    "#e74c3c", `No information` = "#95a5a6")
)
```

## Arguments

- results:

  List of ROBINSIResult objects.

- title:

  Character. Plot title. Default: "Risk of Bias Assessment (ROBINS-I)".

- colors:

  Character vector. Colors for judgments in order: Low, Moderate,
  Serious, Critical, No information. Default: c("#2ecc71", "#f1c40f",
  "#e67e22", "#e74c3c", "#95a5a6").

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- list(
  assess_robins_i("S1", "Serious", "Low", "Low", "Low", "Low",
                 "Moderate", "Low",
                 outcome = "OS"),
  assess_robins_i("S2", "Low", "Low", "Low", "Low", "Low", "Low", "Low",
                 outcome = "OS"),
  assess_robins_i("S3", "Moderate", "Low", "Low", "Low", "Low", "Low", "Low",
                 outcome = "PFS")
)
plot <- robins_i_plot(results)
print(plot)
} # }
```
