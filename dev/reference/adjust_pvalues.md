# Adjust P-values for Multiple Comparisons

Adjusts p-values for multiple testing using common methods. Essential
for subgroup analyses in GBA/AMNOG dossiers where multiple comparisons
require appropriate correction.

## Usage

``` r
adjust_pvalues(
  p,
  method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "fdr", "BY", "none"),
  alpha = 0.05
)
```

## Arguments

- p:

  Numeric vector of p-values

- method:

  Character. Adjustment method:

  - "holm" (default): Holm-Bonferroni step-down (controls FWER)

  - "hochberg": Hochberg step-up (controls FWER)

  - "hommel": Hommel method (controls FWER)

  - "bonferroni": Bonferroni correction (controls FWER, conservative)

  - "BH" or "fdr": Benjamini-Hochberg (controls FDR)

  - "BY": Benjamini-Yekutieli (controls FDR under dependency)

  - "none": No adjustment

- alpha:

  Numeric. Significance level (default: 0.05)

## Value

A data frame with columns:

- original_p: Original p-values

- adjusted_p: Adjusted p-values

- significant_original: Logical for original significance

- significant_adjusted: Logical for adjusted significance

- method: Adjustment method used

## Details

FWER methods control the probability of making any false rejection. FDR
methods control the expected proportion of false rejections.

For GBA submissions, Holm or Hochberg methods are commonly recommended
as they control FWER while being less conservative than Bonferroni.

## Examples

``` r
# Adjust p-values from multiple subgroup comparisons
p_values <- c(0.01, 0.04, 0.03, 0.15, 0.008)
adjust_pvalues(p_values, method = "holm")
#>   original_p adjusted_p significant_original significant_adjusted method
#> 1      0.010       0.04                 TRUE                 TRUE   holm
#> 2      0.040       0.09                 TRUE                FALSE   holm
#> 3      0.030       0.09                 TRUE                FALSE   holm
#> 4      0.150       0.15                FALSE                FALSE   holm
#> 5      0.008       0.04                 TRUE                 TRUE   holm

# Compare different adjustment methods
adjust_pvalues(p_values, method = "bonferroni")
#>   original_p adjusted_p significant_original significant_adjusted     method
#> 1      0.010       0.05                 TRUE                FALSE bonferroni
#> 2      0.040       0.20                 TRUE                FALSE bonferroni
#> 3      0.030       0.15                 TRUE                FALSE bonferroni
#> 4      0.150       0.75                FALSE                FALSE bonferroni
#> 5      0.008       0.04                 TRUE                 TRUE bonferroni
adjust_pvalues(p_values, method = "BH")
#>   original_p adjusted_p significant_original significant_adjusted method
#> 1      0.010      0.025                 TRUE                 TRUE     BH
#> 2      0.040      0.050                 TRUE                FALSE     BH
#> 3      0.030      0.050                 TRUE                FALSE     BH
#> 4      0.150      0.150                FALSE                FALSE     BH
#> 5      0.008      0.025                 TRUE                 TRUE     BH

# With custom significance level
adjust_pvalues(p_values, method = "holm", alpha = 0.10)
#>   original_p adjusted_p significant_original significant_adjusted method
#> 1      0.010       0.04                 TRUE                 TRUE   holm
#> 2      0.040       0.09                 TRUE                 TRUE   holm
#> 3      0.030       0.09                 TRUE                 TRUE   holm
#> 4      0.150       0.15                FALSE                FALSE   holm
#> 5      0.008       0.04                 TRUE                 TRUE   holm
```
