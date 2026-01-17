# Summarize Bias-Adjusted Meta-Analysis Results

Creates a formatted summary table of bias-adjusted meta-analysis results
including the original and adjusted estimates with interpretation.

## Usage

``` r
summarize_bias_adjusted(adjusted_result, original_result, digits = 3)
```

## Arguments

- adjusted_result:

  A BiasAdjustedMetaResult object from bias_adjusted_meta().

- original_result:

  A MetaResult object with original (unadjusted) results.

- digits:

  Integer. Number of decimal places for display. Default: 3.

## Value

A data frame with comparison of original and adjusted results.

## Examples

``` r
if (FALSE) { # \dontrun{
# (Using results from bias_adjusted_meta example)
# summary_df <- summarize_bias_adjusted(adjusted, original_meta)
# print(summary_df)
} # }
```
