# Format Bayesian Meta-Analysis Result for IQWiG Submission

Formats a bayesian_meta_result object according to IQWiG Methods v8.0
requirements. Includes effect estimate, credible interval,
heterogeneity, and probability statements with proper formatting
(semicolons in CIs, specific decimal places, etc.)

## Usage

``` r
format_bayesian_result_iqwig(
  bayesian_result,
  digits_estimate = 3L,
  digits_ci = 3L,
  digits_tau = 3L,
  locale = get_locale(),
  include_prob = TRUE,
  include_interpretation = TRUE,
  ci_brackets = c("[", "]")
)
```

## Arguments

- bayesian_result:

  A bayesian_meta_result object from bayesian_meta_analysis()

- digits_estimate:

  Integer. Decimal places for estimate (default: 3)

- digits_ci:

  Integer. Decimal places for CI bounds (default: 3)

- digits_tau:

  Integer. Decimal places for heterogeneity (default: 3)

- locale:

  Character. Locale for decimal separator: "en" or "de" (default:
  current pharmhand locale)

- include_prob:

  Logical. Include probability statements (default: TRUE)

- include_interpretation:

  Logical. Include text interpretation (default: TRUE)

- ci_brackets:

  Character vector of length 2 for CI brackets (default: `c("[", "]")`
  per IQWiG)

## Value

A list with formatted strings for:

- estimate:

  Formatted point estimate

- ci_95:

  Formatted 95% credible interval as "lower; upper" with brackets

- tau:

  Formatted heterogeneity estimate

- tau_ci:

  Formatted heterogeneity CI

- probability_statement:

  Probability statement string

- interpretation:

  Text interpretation

- full_text:

  Complete formatted summary text

- raw:

  Raw values used to construct the formatted output, with fields:

  - point_estimate

  - ci_lower

  - ci_upper

  - tau

  - tau_ci_lower

  - tau_ci_upper

  - effect_measure

  - n_studies

  - prob_beneficial

## Examples

``` r
if (FALSE) { # \dontrun{
yi <- log(c(0.75, 0.82))
sei <- c(0.12, 0.15)
result <- bayesian_meta_analysis(
  yi = yi, sei = sei, effect_measure = "hr"
)
formatted <- format_bayesian_result_iqwig(result)
print(formatted$full_text)
} # }
```
