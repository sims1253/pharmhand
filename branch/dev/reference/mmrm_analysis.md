# MMRM Analysis

Fits a Mixed Model Repeated Measures (MMRM) for longitudinal data
analysis.

## Usage

``` r
mmrm_analysis(
  data,
  response_var,
  subject_var,
  trt_var,
  time_var,
  covariates = NULL,
  interaction = FALSE,
  cov_covariance = c("us", "cs", "ar1", "ad", "toep", "sp_exp"),
  df_adjustment = c("Kenward-Roger", "Satterthwaite", "Residual"),
  method = c("REML", "ML"),
  control = NULL
)
```

## Arguments

- data:

  A data frame containing the longitudinal data

- response_var:

  Character. Name of the response variable

- subject_var:

  Character. Name of the subject identifier variable

- trt_var:

  Character. Name of the treatment variable

- time_var:

  Character. Name of the time/visit variable

- covariates:

  Character vector of covariate variable names

- interaction:

  Logical. Whether to include treatment by time interaction

- cov_covariance:

  Character. Covariance structure: "us" (unstructured), "cs" (compound
  symmetry), "ar1" (autoregressive), "ad" (ante-dependence), "toep"
  (Toeplitz), "sp_exp" (spatial exponential)

- df_adjustment:

  Character. Degrees of freedom adjustment: "Kenward-Roger",
  "Satterthwaite", "Residual"

- method:

  Character. Estimation method: "REML" (default) or "ML"

- control:

  List of control parameters for optimization, passed to
  [`mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).
  Typically created with
  [`mmrm_control`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm_control.html).

## Value

An MMRMResult object

## Details

The MMRM model fits: Y_ij = X_ij \* beta + epsilon_ij

Where Y_ij is the response for subject i at time j, X_ij contains fixed
effects (treatment, time, covariates, interactions), and epsilon_ij ~
N(0, Sigma) where Sigma is the covariance matrix.

Common covariance structures:

- "us": Unstructured - most flexible, all covariances differ

- "cs": Compound symmetry - equal variances and covariances

- "ar1": Autoregressive - covariance decreases with time distance

- "ad": Ante-dependence - flexible for unequal time intervals

## References

Mallinckrodt, C.H. et al. (2013). Choosing the optimal mixed-effects
model for repeated measures data. Statistical Methods in Medical
Research, 22(2), 113-138.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic MMRM with treatment and time effects
result <- mmrm_analysis(
  data = pro_data,
  response_var = "AVAL",
  subject_var = "USUBJID",
  trt_var = "TRT01P",
  time_var = "AVISITN",
  covariates = "BASE"
)

# MMRM with interaction and custom covariance
result <- mmrm_analysis(
  data = pro_data,
  response_var = "AVAL",
  subject_var = "USUBJID",
  trt_var = "TRT01P",
  time_var = "AVISITN",
  covariates = c("BASE", "AGE"),
  interaction = TRUE,
  cov_covariance = "cs"
)
} # }
```
