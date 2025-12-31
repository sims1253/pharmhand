# BalanceAssessment S7 Class

Container for comprehensive baseline balance assessment results
including SMD calculations, imbalance flags, and data for Love plots.

## Usage

``` r
BalanceAssessment(
  smd_results = data.frame(),
  imbalanced_vars = character(0),
  threshold = 0.1,
  n_treatment = 0L,
  n_control = 0L,
  summary_stats = list(),
  love_plot_data = data.frame(),
  metadata = list()
)
```

## Arguments

- smd_results:

  Data frame containing SMD results for each variable

- imbalanced_vars:

  Character vector of variable names exceeding threshold

- threshold:

  Numeric threshold used for imbalance flagging

- n_treatment:

  Integer count of subjects in treatment group

- n_control:

  Integer count of subjects in control group

- summary_stats:

  List of summary statistics

- love_plot_data:

  Data frame formatted for Love plot creation

- metadata:

  List of additional metadata

## Value

A BalanceAssessment object
