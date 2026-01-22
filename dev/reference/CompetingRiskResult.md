# CompetingRiskResult Class

An S7 class for storing competing risk analysis results.

## Usage

``` r
CompetingRiskResult(
  model = NULL,
  cif_main = data.frame(),
  cif_competing = data.frame(),
  cif_by_treatment = list(),
  treatment_comparison = data.frame(),
  subhazard_ratio = data.frame(),
  main_event = integer(0),
  competing_events = NULL,
  time_points = integer(0),
  n_obs = integer(0),
  n_events = integer(0),
  metadata = list()
)
```

## Arguments

- model:

  The cmprsk model object

- cif_main:

  Data frame with cumulative incidence function for main event

- cif_competing:

  Data frame with cumulative incidence function for competing events

- cif_by_treatment:

  List of CIFs by treatment group

- treatment_comparison:

  Data frame comparing treatments

- subhazard_ratio:

  Data frame with subhazard ratios

- main_event:

  Integer code for main event of interest

- competing_events:

  Integer vector of competing event codes

- time_points:

  Numeric vector of time points used

- n_obs:

  Number of observations

- n_events:

  Vector of event counts by type

- metadata:

  List of additional metadata

## Value

A CompetingRiskResult object
