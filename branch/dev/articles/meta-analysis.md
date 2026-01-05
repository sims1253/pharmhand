# Meta-Analysis and Network Meta-Analysis

``` r
library(pharmhand)
```

## Introduction

pharmhand provides comprehensive functions for meta-analysis and network
meta-analysis (NMA), supporting German HTA requirements (G-BA/IQWiG).

## Pairwise Meta-Analysis

### Basic Meta-Analysis

``` r
# Five studies with hazard ratios
yi <- log(c(0.75, 0.82, 0.68, 0.91, 0.77))
sei <- c(0.12, 0.15, 0.18, 0.14, 0.11)

result <- meta_analysis(
  yi = yi,
  sei = sei,
  study_labels = paste("Study", 1:5),
  effect_measure = "hr",
  model = "random",
  method = "REML",
  knapp_hartung = TRUE
)

result@estimate
#> [1] 0.7823178
result@ci
#> [1] 0.6840484 0.8947045
result@heterogeneity$I2
#> [1] 0
```

### Heterogeneity Assessment

``` r
het <- calculate_heterogeneity(yi, sei, method = "REML")
het$Q
#> [1] 2.009819
het$I2
#> [1] 0
het$tau2
#> [1] 214637.8
het$interpretation
#> [1] "Low heterogeneity"
```

### Leave-One-Out Sensitivity Analysis

``` r
loo <- leave_one_out(result)
loo$results[, c("excluded_study", "estimate_display", "I2")]
#>   excluded_study estimate_display I2
#> 1        Study 1        0.7980720  0
#> 2        Study 2        0.7794674  0
#> 3        Study 3        0.8000967  0
#> 4        Study 4        0.7603503  0
#> 5        Study 5        0.7925677  0
loo$influential_studies
#> [1] "Study 4"
```

### Forest Plot

``` r
plot <- create_meta_forest_plot(result, title = "Treatment Effect (HR)")
plot@plot
#> Warning in ggplot2::scale_x_log10(limits = xlim): log-10
#> transformation introduced infinite values.
#> `height` was translated to `width`.
```

![Meta-analysis forest
plot](meta-analysis_files/figure-html/forest-1.png)

### Funnel Plot and Publication Bias

``` r
funnel <- create_funnel_plot(result, title = "Funnel Plot")
funnel@plot
```

![Funnel plot](meta-analysis_files/figure-html/funnel-1.png)

``` r
egger <- eggers_test(yi = yi, sei = sei)
egger$p_value
#> [1] 0.8929091
egger$interpretation
#> [1] "No significant asymmetry detected (p >= 0.10)"
```

### Trim-and-Fill

``` r
tf <- trim_and_fill(result)
tf$n_imputed
#> [1] 0
tf$interpretation
#> [1] "No missing studies detected"
```

## Indirect Comparison

``` r
# Bucher method: A vs B via common comparator C
indirect <- indirect_comparison(
  effect_ab = log(0.75),  # A vs C
  se_ab = 0.12,
  effect_bc = log(0.85),  # B vs C
  se_bc = 0.10,
  effect_measure = "hr",
  label_a = "Drug A",
  label_b = "Placebo",
  label_c = "Drug B"
)
indirect@estimate
#> [1] 0.8823529
indirect@ci
#> [1] 0.6496514 1.1984068
```

## Network Meta-Analysis

### Basic NMA

``` r
nma_data <- data.frame(
  study = c("S1", "S2", "S3", "S4"),
  treat1 = c("A", "B", "A", "B"),
  treat2 = c("B", "C", "C", "D"),
  effect = log(c(0.75, 0.90, 0.80, 0.85)),
  se = c(0.12, 0.15, 0.18, 0.14)
)

nma <- network_meta(nma_data, effect_measure = "hr")
nma$comparisons
#>   treatment vs estimate  ci_lower  ci_upper        se n_studies  evidence rank
#> 1         A  A   1.0000 1.0000000 1.0000000 0.0000000        NA reference   NA
#> B         B  A   0.7500 0.5928121 0.9488672 0.1200000         1    direct    2
#> C         C  A   0.8000 0.5621778 1.1384298 0.1800000         1    direct    3
#> D         D  A   0.6375 0.4441466 0.9150272 0.1843909         2  indirect    1
```

### Network Geometry Plot

``` r
net_plot <- create_network_plot(nma, title = "Treatment Network")
net_plot@plot
```

![Network geometry
plot](meta-analysis_files/figure-html/network-plot-1.png)

### SUCRA Rankings

``` r
sucra <- calculate_sucra(nma)
sucra$ranking
#>   treatment mean_rank     sucra prob_best prob_worst final_rank
#> D         D     1.591 80.300000     0.599      0.026          1
#> B         B     2.135 62.166667     0.218      0.028          2
#> C         C     2.482 50.600000     0.182      0.136          3
#> A         A     3.792  6.933333     0.001      0.810          4
sucra$interpretation
#> [1] "Treatment ranking by lower is better (SUCRA, %). Best: D (80.3%), Worst: A (6.9%)"
```

### League Table

``` r
league <- create_league_table(nma)
league@data
#>   Treatment                  A                  B                 C
#> 1         A                  A 1.33 (1.05, 1.69)* 1.25 (0.88, 1.78)
#> 2         B 0.75 (0.59, 0.95)*                  B 0.94 (0.61, 1.43)
#> 3         C  0.80 (0.56, 1.14)  1.07 (0.70, 1.63)                 C
#> 4         D 0.64 (0.44, 0.92)*  0.85 (0.55, 1.31) 0.80 (0.48, 1.32)
#>                    D
#> 1 1.57 (1.09, 2.25)*
#> 2  1.18 (0.76, 1.81)
#> 3  1.25 (0.76, 2.08)
#> 4                  D
```

### Transitivity Assessment

``` r
chars <- data.frame(
  study_id = c("S1", "S1", "S2", "S2", "S3", "S3", "S4", "S4"),
  treatment = c("A", "B", "B", "C", "A", "C", "B", "D"),
  mean_age = c(55, 55, 58, 58, 52, 52, 60, 60),
  pct_male = c(60, 60, 65, 65, 55, 55, 70, 70)
)

transit <- assess_transitivity(
  study_characteristics = chars,
  char_vars = c("mean_age", "pct_male"),
  continuous_vars = c("mean_age", "pct_male")
)
transit$overall_assessment
#> [1] "Transitivity assumption appears reasonable"
```

## Consistency Assessment

A key assumption in network meta-analysis is consistency between direct
and indirect evidence. pharmhand provides tools to assess this
assumption.

### Comparing Direct and Indirect Evidence

When both direct (head-to-head) and indirect evidence exist for a
comparison, we can test whether they agree:

``` r
# Suppose we have direct evidence for A vs B from a head-to-head trial
direct <- list(
  estimate = log(0.78),
  se = 0.14
)

# And indirect evidence via common comparator C
indirect <- indirect_comparison(
  effect_ab = log(0.75),  # A vs C
  se_ab = 0.12,
  effect_bc = log(0.96),  # B vs C
  se_bc = 0.11,
  effect_measure = "hr",
  label_a = "A",
  label_b = "C",
  label_c = "B"
)

# Compare direct and indirect evidence
consistency <- compare_direct_indirect(
  direct_result = direct,
  indirect_result = indirect,
  effect_measure = "hr"
)

consistency$direct_estimate
#> [1] 0.78
consistency$indirect_estimate
#> [1] 0.78125
consistency$inconsistency_p
#> NULL
consistency$is_consistent
#> NULL
```

A non-significant p-value (p \> 0.05) suggests the direct and indirect
evidence are consistent, supporting the validity of the indirect
comparison.

### Node-Splitting Analysis

For network meta-analyses, node-splitting separates direct and indirect
evidence for each comparison to identify potential inconsistencies:

``` r
# Node-splitting analysis on our NMA
ns <- node_splitting(nma)
ns$results
#>   comparison direct_estimate direct_se n_direct indirect_available
#> 1     A vs B          0.7500 0.1200000        1               TRUE
#> 2     B vs C          0.8000 0.1800000        1               TRUE
#> 3     A vs C          0.8000 0.1800000        1               TRUE
#> 4     B vs D          0.6375 0.1843909        1               TRUE
#>   inconsistency_p
#> 1              NA
#> 2              NA
#> 3              NA
#> 4              NA
ns$note
#> [1] "Full node-splitting requires re-analysis excluding direct evidence. Results shown are simplified."
```

Node-splitting helps identify specific comparisons where direct and
indirect evidence may disagree, which could indicate violations of the
transitivity assumption.

## Bayesian Meta-Analysis

For researchers preferring Bayesian inference, pharmhand provides an
interface to Bayesian meta-analysis using the brms package. This allows
specification of informative priors and provides full posterior
distributions.

``` r
# Bayesian random-effects meta-analysis
# Requires: install.packages("brms")
bayes_result <- bayesian_meta_analysis(
  yi = yi,
  sei = sei,
  study_labels = paste("Study", 1:5),
  effect_measure = "hr",
  prior_mu = list(mean = 0, sd = 10),
  prior_tau = list(type = "half_cauchy", scale = 0.5),
  chains = 4,
  iter = 4000,
  seed = 12345
)

# Posterior summary
bayes_result$posterior_summary

# Credible intervals
bayes_result$credible_interval

# Probability of benefit
bayes_result$prob_benefit
```

**Note:** Bayesian meta-analysis requires the `brms` package and a
working Stan installation. If brms is not available, the function
returns a frequentist random-effects estimate as a fallback with
installation guidance.

Advantages of the Bayesian approach include:

- Natural interpretation of credible intervals as probability statements
- Ability to incorporate prior knowledge
- Full posterior distributions for all parameters
- Better handling of small sample sizes

## Summary

pharmhand provides a complete toolkit for evidence synthesis:

| Function                                                                                                            | Purpose                             |
|---------------------------------------------------------------------------------------------------------------------|-------------------------------------|
| [`meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/meta_analysis.md)                     | Fixed/random-effects meta-analysis  |
| [`calculate_heterogeneity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_heterogeneity.md) | Q, I², τ² statistics                |
| [`leave_one_out()`](https://sims1253.github.io/pharmhand/branch/dev/reference/leave_one_out.md)                     | Sensitivity analysis                |
| [`create_meta_forest_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_meta_forest_plot.md) | Forest plot visualization           |
| [`create_funnel_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_funnel_plot.md)           | Publication bias assessment         |
| [`eggers_test()`](https://sims1253.github.io/pharmhand/branch/dev/reference/eggers_test.md)                         | Funnel asymmetry test               |
| [`trim_and_fill()`](https://sims1253.github.io/pharmhand/branch/dev/reference/trim_and_fill.md)                     | Bias adjustment                     |
| [`indirect_comparison()`](https://sims1253.github.io/pharmhand/branch/dev/reference/indirect_comparison.md)         | Bucher indirect comparison          |
| [`compare_direct_indirect()`](https://sims1253.github.io/pharmhand/branch/dev/reference/compare_direct_indirect.md) | Test direct vs indirect consistency |
| [`network_meta()`](https://sims1253.github.io/pharmhand/branch/dev/reference/network_meta.md)                       | Network meta-analysis               |
| [`create_network_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_network_plot.md)         | Network geometry                    |
| [`calculate_sucra()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_sucra.md)                 | Treatment rankings                  |
| [`create_league_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_league_table.md)         | Pairwise comparisons                |
| [`assess_transitivity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_transitivity.md)         | Transitivity check                  |
| [`node_splitting()`](https://sims1253.github.io/pharmhand/branch/dev/reference/node_splitting.md)                   | NMA inconsistency testing           |
| [`bayesian_meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/bayesian_meta_analysis.md)   | Bayesian meta-analysis (via brms)   |
