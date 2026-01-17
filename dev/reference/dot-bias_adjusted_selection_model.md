# Internal: Selection model method

Implements a simplified selection model approach where studies are
weighted based on the probability of being selected given their p-values
and risk of bias. This is a sensitivity analysis approach.

## Usage

``` r
.bias_adjusted_selection_model(
  yi,
  sei,
  study_labels,
  study_weights,
  include_study,
  meta_result,
  conf_level,
  selection_alpha,
  rob_judgments
)
```
