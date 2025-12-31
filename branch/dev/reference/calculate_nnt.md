# Calculate Number Needed to Treat (NNT) / Number Needed to Harm (NNH)

Calculates NNT or NNH from risk difference with confidence intervals.
Positive values indicate NNT (benefit), negative values indicate NNH
(harm).

## Usage

``` r
calculate_nnt(
  rd,
  rd_lower = NULL,
  rd_upper = NULL,
  event_type = c("benefit", "harm")
)
```

## Arguments

- rd:

  Numeric. Risk difference (treatment - control)

- rd_lower:

  Numeric. Lower CI bound for risk difference

- rd_upper:

  Numeric. Upper CI bound for risk difference

- event_type:

  Character. "benefit" for favorable outcomes (default), "harm" for
  adverse events

## Value

A list with:

- nnt: Number needed to treat (positive) or harm (negative)

- nnt_lower: Lower CI bound

- nnt_upper: Upper CI bound

- interpretation: Text interpretation

## Details

NNT = 1 / absolute_risk_difference

When the CI for RD crosses zero, the NNT CI will include infinity,
indicating the treatment effect is not statistically significant.

For GBA benefit assessment, NNT provides a clinically interpretable
measure of absolute treatment effect.

## Examples

``` r
# Treatment increases response rate (beneficial for benefit endpoint)
calculate_nnt(rd = 0.10, rd_lower = 0.05, rd_upper = 0.15)
#> $nnt
#> [1] 10
#> 
#> $nnt_lower
#> [1] 6.666667
#> 
#> $nnt_upper
#> [1] 20
#> 
#> $rd
#> [1] 0.1
#> 
#> $rd_lower
#> [1] 0.05
#> 
#> $rd_upper
#> [1] 0.15
#> 
#> $ci_crosses_zero
#> [1] FALSE
#> 
#> $event_type
#> [1] "benefit"
#> 
#> $interpretation
#> [1] "NNT = 10 (95% CI: 6.7 to 20). Treat 10 patients for one additional patient to benefit."
#> 

# Treatment reduces adverse events (beneficial for harm endpoint)
calculate_nnt(
  rd = -0.08, rd_lower = -0.14, rd_upper = -0.02, event_type = "harm"
)
#> $nnt
#> [1] 12.5
#> 
#> $nnt_lower
#> [1] 7.142857
#> 
#> $nnt_upper
#> [1] 50
#> 
#> $rd
#> [1] -0.08
#> 
#> $rd_lower
#> [1] -0.14
#> 
#> $rd_upper
#> [1] -0.02
#> 
#> $ci_crosses_zero
#> [1] FALSE
#> 
#> $event_type
#> [1] "harm"
#> 
#> $interpretation
#> [1] "NNT = 12.5 (95% CI: 7.1 to 50). Treat 12 patients for one additional patient to benefit."
#> 

# Non-significant effect (CI crosses zero)
calculate_nnt(rd = -0.05, rd_lower = -0.12, rd_upper = 0.02)
#> $nnt
#> [1] -20
#> 
#> $nnt_lower
#> [1] 8.333333
#> 
#> $nnt_upper
#> [1] -50
#> 
#> $rd
#> [1] -0.05
#> 
#> $rd_lower
#> [1] -0.12
#> 
#> $rd_upper
#> [1] 0.02
#> 
#> $ci_crosses_zero
#> [1] TRUE
#> 
#> $event_type
#> [1] "benefit"
#> 
#> $interpretation
#> [1] "Effect not statistically significant (CI crosses zero). Point estimate NNT = 20 to harm"
#> 
```
