# Calculate Risk Difference and Confidence Interval for AE

Calculates risk difference, risk ratio, and associated confidence
intervals and p-values for comparing adverse event incidence between two
groups. Uses Wald method for risk difference CI and log-transformation
for risk ratio CI.

## Usage

``` r
calculate_ae_risk_difference(n1, N1, n2, N2, conf_level = 0.95)
```

## Arguments

- n1:

  Number of subjects with event in treatment group

- N1:

  Total subjects in treatment group

- n2:

  Number of subjects with event in reference group

- N2:

  Total subjects in reference group

- conf_level:

  Confidence level (default: 0.95)

## Value

List with rd (risk difference), rd_lower, rd_upper, rr (risk ratio),
rr_lower, rr_upper, p_value
