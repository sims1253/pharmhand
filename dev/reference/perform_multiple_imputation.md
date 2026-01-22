# Perform Multiple Imputation

Performs multiple imputation using the mice package.

## Usage

``` r
perform_multiple_imputation(
  data,
  m = 5L,
  maxit = 5L,
  method = "pmm",
  predictorMatrix = NULL,
  seed = NULL,
  print = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame with missing values

- m:

  Integer. Number of imputations to perform (default: 5)

- maxit:

  Integer. Number of iterations for mice algorithm (default: 5)

- method:

  Character. Imputation method(s). Can be a single method applied to all
  variables or a named vector specifying method per variable. Common
  methods include:

  - "pmm": Predictive mean matching (default, for numeric)

  - "logreg": Logistic regression (for binary)

  - "polyreg": Polytomous regression (for unordered factors)

  - "polr": Proportional odds model (for ordered factors)

  - "norm": Bayesian linear regression

  - "rf": Random forest

- predictorMatrix:

  Matrix specifying which variables predict which. If NULL (default),
  mice determines automatically.

- seed:

  Integer. Random seed for reproducibility

- print:

  Logical. Whether to print mice progress (default: FALSE)

- ...:

  Additional arguments passed to mice::mice()

## Value

An ImputationResult object

## Details

This function wraps mice::mice() to perform multiple imputation under
the Missing at Random (MAR) assumption. The mice algorithm uses chained
equations (MICE/FCS) to impute missing values.

For clinical trial data, predictive mean matching ("pmm") is often
recommended as it preserves the distribution of observed values and
works well with continuous variables.

## References

van Buuren, S. and Groothuis-Oudshoorn, K. (2011). mice: Multivariate
Imputation by Chained Equations in R. Journal of Statistical Software,
45(3), 1-67.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic multiple imputation
data <- data.frame(
  age = c(45, NA, 52, 38, NA),
  bmi = c(25, 28, NA, 31, 27),
  treatment = c("A", "B", "A", NA, "B")
)

result <- perform_multiple_imputation(data, m = 5, seed = 123)

# Access completed datasets
completed <- get_complete_data(result)

# View imputation summary
result@imputed_vars
result@n_missing
} # }
```
