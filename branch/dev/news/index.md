# Changelog

## pharmhand 0.4.2.9000

- Validation helpers now reject NA consistently across assertion
  functions.
- Reporting engine refactor: title/footnotes helper, ADaMData coercion,
  empty gt styling.
- Export improvements: preserve ClinicalTable styling, HTML title
  handling.
- Safety/efficacy robustness: NNH bounds ordering, warnings on empty
  filters, Treatment column guard, case-insensitive DISCONTINUED
  matching.
- Added .format_n_over_n formatting helper for n/N display.
- Test refactors for clarity and maintainability.

## pharmhand 0.4.1.9000

### Code Quality Improvements

#### Validation Utilities

- Added comprehensive assertion functions for input validation:
  - [`assert_all_positive()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_all_positive.md),
    [`assert_positive()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_positive.md),
    [`assert_non_negative()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_non_negative.md)
  - [`assert_positive_integer()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_positive_integer.md),
    [`assert_non_negative_integer()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_non_negative_integer.md)
  - [`assert_character_scalar()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_character_scalar.md),
    [`assert_character_vector()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_character_vector.md)
  - [`assert_numeric_scalar()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_numeric_scalar.md),
    [`assert_numeric_vector()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_numeric_vector.md)
  - [`assert_integer_scalar()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_integer_scalar.md),
    [`assert_logical_scalar()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_logical_scalar.md)
  - [`assert_data_frame()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_data_frame.md),
    [`assert_column_exists()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_column_exists.md)
  - [`assert_lengths_match()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_lengths_match.md),
    [`assert_no_na()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_no_na.md),
    [`assert_in_range()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assert_in_range.md)

#### Messaging and Helpers

- Added
  [`ph_abort()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_abort.md)
  for consistent error handling with abort messaging
- Added
  [`ph_inform()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_inform.md)
  for informational messages
- Added
  [`ph_warn()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_warn.md)
  for warning messages
- Added
  [`get_subject_n()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_subject_n.md)
  for extracting subject counts from ADaM datasets
- Added
  [`get_summary_label()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_summary_label.md)
  for generating standardized summary labels
- Added `||` operator (`grapes-or-or-grapes`) for flexible default value
  handling

#### Documentation Improvements

- Added man pages for internal helper functions:
  - [`.build_evidence_summary_row()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-build_evidence_summary_row.md),
    [`.build_league_matrix()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-build_league_matrix.md),
    [`.build_rob_summary_row()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-build_rob_summary_row.md)
  - [`.build_study_characteristic_row()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-build_study_characteristic_row.md),
    [`.calculate_ae_comparisons()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-calculate_ae_comparisons.md)
  - [`.empty_ae_exposure_df()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-empty_ae_exposure_df.md),
    [`.ensure_adam_data()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-ensure_adam_data.md),
    [`.summarize_ae_exposure()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-summarize_ae_exposure.md),
    [`.summarize_ae_hierarchy()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-summarize_ae_hierarchy.md)
- Added man pages for utility functions:
  [`ph_abort()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_abort.md),
  [`ph_inform()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_inform.md),
  [`ph_warn()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_warn.md)
- Added man pages for new helpers:
  [`get_subject_n()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_subject_n.md),
  [`get_summary_label()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_summary_label.md),
  `||`

#### Cleanup and Maintenance

- Removed `.tldr/` directory and cache files (TLDR code analysis cache)
- Deprecated `safe_pct()` function (removed man page)
- Removed 11 old imputation test problem files from
  `tests/testthat/_problems/`
- Added `.tldr/` and `vignettes/safety-tables_files/` to `.gitignore`
- Updated README files and pkgdown configuration
- Updated vignettes for baseline, efficacy, and safety tables

## pharmhand 0.4.0.9000

### Phase 3: Quality Assessment

#### Risk of Bias Assessment (RoB 2)

- Added `RoB2Result` S7 class for storing RoB 2 assessment results.
- Added
  [`assess_rob2()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_rob2.md)
  for single-study risk of bias assessment using Cochrane RoB 2 tool.
- Added
  [`assess_rob2_batch()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_rob2_batch.md)
  for batch processing multiple studies from a data frame.
- Added
  [`rob2_summary()`](https://sims1253.github.io/pharmhand/branch/dev/reference/rob2_summary.md)
  for creating summary tables of RoB 2 assessments.
- Implements all 5 RoB 2 domains: randomization, deviations, missing
  data, measurement, selection.
- Automatic overall judgment calculation following RoB 2 algorithm.
- Reference: IQWiG Allgemeine Methoden Section 10.1.4.

#### Risk of Bias Assessment (ROBINS-I)

- Added `ROBINSIResult` S7 class for storing ROBINS-I assessment
  results.
- Added
  [`assess_robins_i()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_robins_i.md)
  for single-study risk of bias assessment using ROBINS-I tool.
- Added
  [`assess_robins_i_batch()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_robins_i_batch.md)
  for batch processing multiple studies from a data frame.
- Added
  [`robins_i_summary()`](https://sims1253.github.io/pharmhand/branch/dev/reference/robins_i_summary.md)
  for creating summary tables of ROBINS-I assessments.
- Implements all 7 ROBINS-I domains: confounding, participant selection,
  classification of interventions, deviations, missing data, outcome
  measurement, selection of reported result.
- Supports 5-level judgments: Low, Moderate, Serious, Critical, No
  information.
- Reference: IQWiG Allgemeine Methoden Section 10.1.4.

#### Risk of Bias Visualization

- Added
  [`create_rob_traffic_light_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_rob_traffic_light_plot.md)
  for traffic light visualization of domain-level judgments.
- Added
  [`create_rob_summary_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_rob_summary_plot.md)
  for stacked bar plot summaries of bias assessments.
- Added
  [`save_rob_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/save_rob_plot.md)
  for exporting plots to PNG, SVG, PDF and other formats.
- Added
  [`rob_data_to_tidy()`](https://sims1253.github.io/pharmhand/branch/dev/reference/rob_data_to_tidy.md)
  for converting assessment results to tidy data frames.
- Supports both RoB 2 and ROBINS-I assessment results.
- Uses standard risk of bias color scheme (green/yellow/red/gray).

#### Bias-Adjusted Meta-Analysis

- Added
  [`bias_adjusted_meta()`](https://sims1253.github.io/pharmhand/branch/dev/reference/bias_adjusted_meta.md)
  for meta-analysis with risk of bias adjustments.
- Added
  [`rob_sensitivity_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/rob_sensitivity_analysis.md)
  for sensitivity analysis across RoB scenarios.
- Added
  [`calculate_rob_weights()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_rob_weights.md)
  for computing RoB-adjusted study weights.
- Added
  [`summarize_bias_adjusted()`](https://sims1253.github.io/pharmhand/branch/dev/reference/summarize_bias_adjusted.md)
  for comparing original and adjusted estimates.
- Supports three adjustment methods: weight_downgrade, exclude_high,
  selection_model.
- Integrates with RoB 2 and ROBINS-I assessment results.

#### IQWiG Evidence Grading

- Added
  [`grade_evidence()`](https://sims1253.github.io/pharmhand/branch/dev/reference/grade_evidence.md)
  for assessing evidence certainty using IQWiG methodology.
- Added
  [`assess_evidence_domains()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_evidence_domains.md)
  for evaluating individual evidence domains.
- Added
  [`format_evidence_grade()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_evidence_grade.md)
  for German (Beleg/Hinweis/Anhaltspunkt) or English output.
- Implements IQWiG 3.1.4 evidence grading: Beleg (Proof), Hinweis
  (Indication), Anhaltspunkt (Hint), Kein Beleg (No proof).
- Evaluates: study limitations, inconsistency, imprecision,
  indirectness, publication bias.
- Integrates with RoB 2 assessments for study quality evaluation.

#### Evidence Summary Tables

- Added
  [`create_evidence_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_evidence_summary_table.md)
  for IQWiG-formatted evidence summary tables.
- Added
  [`create_study_characteristics_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_study_characteristics_table.md)
  for G-BA Module 4 study characteristics.
- Added
  [`export_evidence_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/export_evidence_table.md)
  for multi-format export (Word, HTML, Excel).
- Includes: endpoint, N studies, effect estimate, CI, p-value, I², RoB
  summary, evidence grade.
- Supports German/English bilingual output with IQWiG-compliant
  formatting.

## pharmhand 0.3.4.9000

### Fixes and maintenance

- Fixed
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)
  to accept `AnalysisResults` objects (uses `@stats`).
- Improved Bayesian meta-analysis test performance and stability.
- Cleaned up documentation/signature mismatches and `R CMD check`
  issues.

### Major Architecture Improvements

#### Package-Wide Defaults System

- **New defaults infrastructure**: Added centralized default parameter
  system via
  [`ph_default()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_default.md)
  function
  - Default values for common parameters (e.g., `trt_var = "TRT01P"`,
    `autofit = TRUE`, `conf_level = 0.95`)
  - Users can override via options: `options(pharmhand.trt_var = "ARM")`
  - Reduces boilerplate across 40+ functions
  - Improves consistency and maintainability

#### Code Reusability Improvements

- **New
  [`create_clinical_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)
  factory function**: Reduces boilerplate for table creation
  - Consolidates repeated pattern of creating flextable + wrapping in
    ClinicalTable
  - Used across 25+ table creation functions
  - ~200 lines of duplicate code eliminated
- **Extracted
  [`estimate_tau2()`](https://sims1253.github.io/pharmhand/branch/dev/reference/estimate_tau2.md)
  helper**: Eliminates duplication in meta-analysis functions
  - Single implementation of DerSimonian-Laird, Paule-Mandel, and REML
    methods
  - Used by both
    [`meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/meta_analysis.md)
    and
    [`calculate_heterogeneity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_heterogeneity.md)
  - ~100 lines of duplicate code eliminated

#### Enhanced User Experience

- **Automatic data.frame coercion**: User-facing functions now accept
  data.frames directly
  - [`create_demographics_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_demographics_table.md)
    automatically wraps data.frames in ADaMData
  - Eliminates need for users to manually create S7 objects
  - Backward compatible with existing ADaMData inputs
- **Improved error messages**: Context-rich error reporting throughout
  - Error messages now show what was received vs. what was expected
  - Available values suggested when appropriate (e.g., treatment arms)
  - Missing column errors show available columns for easier debugging
- **New workflow helpers**: High-level convenience functions for common
  tasks
  - [`quick_demographics_report()`](https://sims1253.github.io/pharmhand/branch/dev/reference/quick_demographics_report.md):
    Generate demographics report in one call
  - [`quick_safety_report()`](https://sims1253.github.io/pharmhand/branch/dev/reference/quick_safety_report.md):
    Generate multi-table safety report in one call
  - Ideal for quick analyses and prototyping

#### API Consistency

- **Standardized parameter names**: Using
  [`ph_default()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_default.md)
  across the package
  - `trt_var` (not `treatment_var`) for treatment variable
  - `conf_level` (not `ci_level` or `confidence_level`) for confidence
    level
  - `autofit` consistently for table formatting
  - Reduces cognitive load for users
- **Consistent return types**: All `create_*_table()` functions return
  ClinicalTable objects
  - Fixed inconsistencies where some returned bare flextables
  - Enables consistent downstream processing

### Breaking Changes

#### Source File Reorganization

- **Efficacy module split**: `R/efficacy_tables.R` has been split into
  focused modules:
  - `R/efficacy_primary.R` - Primary endpoint tables
    ([`create_primary_endpoint_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_primary_endpoint_table.md))
  - `R/efficacy_cfb.R` - Change from baseline tables
  - `R/efficacy_lab.R` - Laboratory tables
  - `R/efficacy_tte.R` - Time-to-event tables
  - `R/efficacy_responder.R` - Responder analysis tables
  - `R/efficacy_subgroup.R` - Subgroup analysis tables
- **Safety module split**: `R/safety_tables.R` has been split into
  focused modules:
  - `R/safety_summary.R` - AE summary tables
    ([`create_ae_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_summary_table.md))
  - `R/safety_comparison.R` - AE comparison tables
  - `R/safety_hierarchy.R` - SOC/PT hierarchy tables
  - `R/safety_tte.R` - Safety time-to-event analysis
  - `R/safety_exposure.R` - Exposure-adjusted analysis
- **Migration guidance**: Users who previously sourced these files
  directly should:
  - Use the package namespace (e.g.,
    [`pharmhand::create_primary_endpoint_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_primary_endpoint_table.md))
  - Or source the new module files (e.g.,
    `source('R/efficacy_primary.R')`)
  - Update any direct imports to reference the new module names

### Bug Fixes

#### Robustness Improvements

- Fixed
  [`create_primary_endpoint_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_primary_endpoint_table.md)
  to handle empty data or all-NA values gracefully, returning “-”
  instead of NaN/Inf
- Fixed `create_responder_analysis_table()` to compute response rate in
  separate mutate step, avoiding fragile self-reference in summarise
- Fixed
  [`create_tte_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_tte_summary_table.md)
  to correctly extract landmark survival estimates for each treatment
  stratum instead of using first stratum for all
- Fixed
  [`create_tte_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_tte_summary_table.md)
  to use dynamic CI column names based on conf_level instead of
  hardcoded “lower .95”/“upper .95”
- Fixed
  [`create_league_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_league_table.md)
  to use NMA result’s stored confidence level when available
- Fixed
  [`leave_one_out()`](https://sims1253.github.io/pharmhand/branch/dev/reference/leave_one_out.md)
  to read ci_level from correct slot (<meta_result@ci>\_level instead of
  [@metadata](https://github.com/metadata)\$conf_level)

#### Input Validation

- Require positive, non-missing standard errors in
  [`eggers_test()`](https://sims1253.github.io/pharmhand/branch/dev/reference/eggers_test.md),
  [`trim_and_fill()`](https://sims1253.github.io/pharmhand/branch/dev/reference/trim_and_fill.md),
  [`meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/meta_analysis.md),
  and
  [`calculate_heterogeneity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_heterogeneity.md)
- Added `length(yi) == length(sei)` validation in
  [`eggers_test()`](https://sims1253.github.io/pharmhand/branch/dev/reference/eggers_test.md)
  and
  [`trim_and_fill()`](https://sims1253.github.io/pharmhand/branch/dev/reference/trim_and_fill.md)
- Added `anyNA(yi)` check in
  [`eggers_test()`](https://sims1253.github.io/pharmhand/branch/dev/reference/eggers_test.md)
  returning NA-filled result with clear interpretation
- Added `anyNA(yi)` check in
  [`trim_and_fill()`](https://sims1253.github.io/pharmhand/branch/dev/reference/trim_and_fill.md)
  with error listing invalid indices

#### Code Quality

- Changed [`stop()`](https://rdrr.io/r/base/stop.html) to
  [`ph_abort()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ph_abort.md)
  in example script for consistent error handling
- Fixed vignette code fence indentation mismatch in efficacy-tables.Rmd
- Added `:=` import from rlang to fix undefined global function warning
- Fixed
  [`create_mean_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_mean_plot.md)
  n calculation to count non-missing values
- Added guards for CI computation when n \<= 1 to avoid Inf values

#### Plotting Fixes

- Fixed `create_efficacy_waterfall_plot()` to compute category counts in
  separate mutate step, avoiding fragile self-reference in summarise
- Fixed
  [`create_forest_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_forest_plot.md)
  Cox model CI column extraction to use positional indexing instead of
  fragile name construction
- Fixed
  [`create_km_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_km_plot.md)
  risk table grid background by explicitly setting
  panel.grid.major/minor to element_blank()
- Fixed `safe_pct()` division by zero in `safety_summary.R` by adding
  helper function applied to 9 locations

#### Bayesian Meta-Analysis Fixes

- Fixed
  [`bayesian_meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/bayesian_meta_analysis.md)
  to use
  [`brms::neff_ratio()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
  instead of non-exported `brms::ess_bulk()`/`brms::ess_tail()`
- Fixed
  [`bayesian_meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/bayesian_meta_analysis.md)
  to compute BFMI from nuts_params energy values
- Fixed
  [`bayesian_meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/bayesian_meta_analysis.md)
  to use
  [`brms::nuts_params()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
  for divergent transitions instead of accessing internal
  `fit$fit@sim$divergent__`
- Added `tryCatch` wrapper around
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
  calls for better error handling
- Changed all brms tests to use `cores = 1` to avoid parallel rstan
  issues in CI
- Added `bayesplot` to Suggests, removed direct `rstan` dependency
- Fixed roxygen documentation to avoid `\describe{}` block Rd parsing
  issues
- Added brms test caching helper to speed up test suite

#### Documentation

- Added [@title](https://github.com/title) tags and completed
  [@return](https://github.com/return) documentation in `meta_bias.R`
- Added within-study variation warning documentation in
  [`assess_transitivity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_transitivity.md)
- Split long [@return](https://github.com/return) lines in
  `safety_tte.R`
- Added description field to meta-analysis vignette
- Updated brms section in meta-analysis vignette with installation
  guidance

### Test Improvements

- Refactored test-plotting_forest.R, test-efficacy_tte.R, and
  test-efficacy_subgroup.R to use shared test fixtures
- Included documentation examples to exported functions in efficacy and
  safety modules
- Updated `test-meta_bayesian.R` with 8-study test data and
  `adapt_delta = 0.99` to prevent divergent transitions
- Seeded RNG in 11 tests in `test-pro-analysis.R`
- Applied [`set.seed()`](https://rdrr.io/r/base/Random.html) to
  `test-efficacy_tte.R` before sample() call
- Removed duplicate tests in `test-efficacy_responder.R` and
  `test-efficacy_subgroup.R`

## pharmhand 0.3.0.9000

### Major Changes

#### Test Suite Reorganization

- **One-test-file-per-source-file convention**: Reorganized test files
  to follow R testing best practices.
  - Split `test-efficacy_tables.R` into `test-efficacy_cfb.R`,
    `test-efficacy_lab.R`, `test-efficacy_primary.R`,
    `test-efficacy_responder.R`, `test-efficacy_subgroup.R`, and
    `test-efficacy_tte.R`
  - Split `test-meta-analysis.R` into `test-meta_core.R`,
    `test-meta_bayesian.R`, `test-meta_bias.R`, `test-meta_indirect.R`,
    `test-meta_network.R`, and `test-meta_plots.R`
  - Split `test-plotting.R` into `test-plotting_survival.R`,
    `test-plotting_efficacy.R`, and `test-plotting_forest.R`
  - Renamed `test-safety_tables.R` to `test-safety_summary.R` and
    extracted `test-safety_comparison.R` and `test-safety_hierarchy.R`

#### New Test Coverage

- Added comprehensive tests for Configuration API (`test-config_api.R`)
- Added comprehensive tests for Configuration Classes
  (`test-config_classes.R`)

### Bug Fixes

#### Test Data Improvements

- Fixed test data in safety TTE tests to use larger sample sizes (20+
  subjects per arm) to avoid Cox model convergence issues
- Fixed test data to avoid duplicate landmark timepoints in TTE summary
  tests
- Fixed test data for subgroup analysis to have adequate subjects per
  subgroup (25+)
- Fixed S7 class checks from `expect_s3_class()` to
  [`S7::S7_inherits()`](https://rconsortium.github.io/S7/reference/S7_inherits.html)
  for proper S7 class validation
- Fixed error message regex patterns to match actual function outputs

#### Source Code Fixes

- Fixed `km_summary` matrix conversion in `efficacy_tte.R` to preserve
  column names for single treatment arms
- Fixed `pct` column warning in `safety_summary.R` by using proper
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  instead of for-loop assignment
- Fixed `survfit` `n.censor` matrix handling in `plotting_survival.R` by
  converting to vector
- Fixed [`min()`](https://rdrr.io/r/base/Extremes.html) returning `-Inf`
  in `pro_analysis.R` when no events present
- Fixed S7 class checking in `config_api.R` to use
  [`S7::S7_inherits()`](https://rconsortium.github.io/S7/reference/S7_inherits.html)
  instead of [`inherits()`](https://rdrr.io/r/base/class.html)

#### Test Cleanup

- Removed tests for unimplemented `maic()` and `stc()` functions
- Fixed missing `registry` variable in config API tests
- Changed REML tests to use DerSimonian-Laird method to avoid
  convergence warnings
- Replaced `any(is.na())` with
  [`anyNA()`](https://rdrr.io/r/base/NA.html) per jarl recommendations

### Code Quality

- All tests pass (1874 tests)
- No warnings or skipped tests
- lintr clean (line length compliance)
- jarl check clean
- devtools::check() passes with 0 errors, 0 warnings, 0 notes

## pharmhand 0.2.3.9000

### Documentation

- Added runnable examples for all meta-analysis and network
  meta-analysis functions including
  [`meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/meta_analysis.md),
  [`indirect_comparison()`](https://sims1253.github.io/pharmhand/branch/dev/reference/indirect_comparison.md),
  [`network_meta()`](https://sims1253.github.io/pharmhand/branch/dev/reference/network_meta.md),
  [`create_network_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_network_plot.md),
  [`assess_transitivity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_transitivity.md),
  [`node_splitting()`](https://sims1253.github.io/pharmhand/branch/dev/reference/node_splitting.md),
  [`calculate_sucra()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_sucra.md),
  and
  [`create_league_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_league_table.md).

## pharmhand 0.2.2.9000

### Phase 2: Evidence Synthesis

#### Meta-Analysis Functions

- Added
  [`meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/meta_analysis.md)
  for fixed-effect and random-effects meta-analysis with support for
  multiple tau² estimators (DerSimonian-Laird, REML, Paule-Mandel, ML)
  and optional Knapp-Hartung adjustment.

- Added
  [`calculate_heterogeneity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_heterogeneity.md)
  for comprehensive heterogeneity assessment including Q, I², τ², and H²
  statistics with interpretation.

- Added
  [`leave_one_out()`](https://sims1253.github.io/pharmhand/branch/dev/reference/leave_one_out.md)
  for sensitivity analysis identifying influential studies.

- Added
  [`create_meta_forest_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_meta_forest_plot.md)
  for meta-analysis forest plots with study weights, heterogeneity
  statistics, and prediction intervals.

- Added
  [`create_funnel_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_funnel_plot.md)
  for publication bias visualization.

- Added
  [`eggers_test()`](https://sims1253.github.io/pharmhand/branch/dev/reference/eggers_test.md)
  for statistical assessment of funnel plot asymmetry.

- Added
  [`trim_and_fill()`](https://sims1253.github.io/pharmhand/branch/dev/reference/trim_and_fill.md)
  for Duval & Tweedie publication bias adjustment.

#### Indirect Comparison

- Added
  [`indirect_comparison()`](https://sims1253.github.io/pharmhand/branch/dev/reference/indirect_comparison.md)
  using the Bucher method for anchored indirect treatment comparisons.

- Added
  [`compare_direct_indirect()`](https://sims1253.github.io/pharmhand/branch/dev/reference/compare_direct_indirect.md)
  for consistency testing between direct and indirect evidence.

#### Network Meta-Analysis

- Added
  [`network_meta()`](https://sims1253.github.io/pharmhand/branch/dev/reference/network_meta.md)
  for network meta-analysis comparing multiple treatments.

- Added
  [`create_network_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_network_plot.md)
  for network geometry visualization.

- Added
  [`assess_transitivity()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_transitivity.md)
  for evaluating the transitivity assumption.

- Added
  [`node_splitting()`](https://sims1253.github.io/pharmhand/branch/dev/reference/node_splitting.md)
  for testing inconsistency between direct and indirect evidence.

- Added
  [`calculate_sucra()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_sucra.md)
  for treatment ranking with SUCRA/P-scores.

- Added
  [`create_league_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_league_table.md)
  for pairwise comparison tables.

#### Bayesian Analysis

- Added
  [`bayesian_meta_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/bayesian_meta_analysis.md)
  interface for Bayesian meta-analysis using brms when available, with
  automatic fallback to frequentist methods.

### Phase 1 Completion

#### PRO Analysis

- Added
  [`calculate_mcid_anchor()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_mcid_anchor.md)
  for anchor-based MCID calculation.

- Added
  [`calculate_mcid_distribution()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_mcid_distribution.md)
  for distribution-based MCID (0.5 SD, 1 SEM, etc.).

- Added
  [`calculate_mcid()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_mcid.md)
  wrapper combining both MCID approaches.

- Added
  [`create_ttd_analysis()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ttd_analysis.md)
  for time-to-deterioration analysis with Kaplan-Meier estimation.

#### Visualization

- Added
  [`create_mean_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_mean_plot.md)
  for longitudinal mean plots with confidence intervals.

- Added
  [`create_spider_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_spider_plot.md)
  for individual patient trajectory visualization.

#### Safety Analysis

- Added
  [`create_ae_hierarchy_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_hierarchy_table.md)
  for full MedDRA hierarchy analysis (SOC → HLGT → HLT → PT).

#### Subgroup Analysis

- Added
  [`assess_iceman()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_iceman.md)
  for ICEMAN criteria assessment of subgroup credibility.

### S7 Classes

- Added comprehensive tests for `MultiArmStudy` S7 class.

### Documentation

- Added extensive test coverage for meta-analysis functions.
- Added tests for PRO analysis functions.

## pharmhand 0.2.1.9000

### Improvements

- Added `conf_level` parameter to
  [`create_ae_cumulative_incidence_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_cumulative_incidence_plot.md)
  (was hard-coded at 0.95).
- Exposed `subgroup_counts` and `min_subgroup_size` in
  [`create_subgroup_analysis_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_subgroup_analysis_table.md)
  metadata for programmatic access.
- Wrapped
  [`test_ph_assumption()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_ph_assumption.md)
  call in
  [`create_tte_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_tte_summary_table.md)
  with tryCatch for robustness with sparse Cox models.

### Plotting

- Created
  [`.pharmhand_theme()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-pharmhand_theme.md)
  internal helper for consistent white backgrounds across all plots.
- Refactored all plotting functions to use
  [`.pharmhand_theme()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-pharmhand_theme.md)
  with `base_size` parameter.
- Added `conf_level` parameter to
  [`create_loglog_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_loglog_plot.md).
- Added `base_size` parameter to
  [`create_ae_cumulative_incidence_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_cumulative_incidence_plot.md)
  and
  [`create_forest_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_forest_plot.md).
- Updated `build_schoenfeld_plot()` to use
  [`.pharmhand_theme()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-pharmhand_theme.md)
  for consistent styling.

### Documentation

- Clarified that `method` parameter in
  [`test_non_inferiority()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_non_inferiority.md)
  applies only to binary endpoints.
- Added `@examples` to
  [`detect_floor_ceiling()`](https://sims1253.github.io/pharmhand/branch/dev/reference/detect_floor_ceiling.md),
  [`test_non_inferiority()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_non_inferiority.md),
  [`ancova_adjust_continuous()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ancova_adjust_continuous.md),
  [`create_loglog_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_loglog_plot.md),
  and
  [`create_ae_cumulative_incidence_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_cumulative_incidence_plot.md).
- Added Time-to-Event Safety Analysis section to safety-tables vignette.
- Added NNH (Number Needed to Harm) documentation to safety-tables
  vignette.
- Added Non-Inferiority Testing section to efficacy-tables vignette.
- Added ANCOVA Adjustment section to efficacy-tables vignette.
- Added Floor/Ceiling Detection section to efficacy-tables and
  baseline-tables vignettes.
- Improved documentation for
  [`test_non_inferiority()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_non_inferiority.md)
  with clarified conf_level, decision logic, and higher_better
  parameter.
- Improved documentation for
  [`ancova_adjust_continuous()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ancova_adjust_continuous.md)
  with edge case behavior and runnable simulated example.

### Code Quality

- Added Newcombe-Wilson reference citation for binary difference in
  proportions CI calculation.
- Removed unused `comparison_results` stub from AE comparison function.

### Tests

- Added test for NNH calculation when risk difference is negative
  (treatment beneficial).

## pharmhand 0.2.0.9000

### Code Quality

- Refactored duplicated palette resolution logic into
  [`.resolve_palette()`](https://sims1253.github.io/pharmhand/branch/dev/reference/dot-resolve_palette.md)
  helper.
- Added warning when log-log plot drops data points with non-finite
  values.
- Expanded PH assumption disable comment with rationale for
  time-to-first AE.
- Added clarifying comments in floor/ceiling detection and event
  conversion.
- Added documentation note about CI construction methods for
  non-inferiority binary endpoints.
- Removed redundant `@keywords internal` from exported
  [`ancova_adjust_continuous()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ancova_adjust_continuous.md).

### Tests

- Made color palette test assertions order-agnostic with
  `expect_setequal()`.
- Added edge case tests for
  [`calculate_exposure_adjusted_rate()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_exposure_adjusted_rate.md)
  (zero events, small exposure, large counts).

## pharmhand 0.1.21.9000

### Bug Fixes

- Fixed ComparisonResult defaults from `integer(0)` to `numeric(0)` for
  consistency with S7 class.
- Fixed line length lint errors in
  [`test_non_inferiority()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_non_inferiority.md)
  documentation.

### Documentation

- Updated pkgdown Utilities section description to include “statistical
  diagnostics”.
- Added missing topics to pkgdown reference index:
  [`calculate_exposure_adjusted_rate()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_exposure_adjusted_rate.md),
  [`create_ae_exposure_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_exposure_table.md),
  [`create_time_to_first_ae()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_time_to_first_ae.md),
  and
  [`test_non_inferiority()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_non_inferiority.md).
- Cleaned up duplicate NEWS.md heading.
- Added explanatory comment for PH assumption check disable in
  time-to-first AE function.

### Plotting

- Added `show_censor` support to
  [`create_loglog_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_loglog_plot.md)
  and passed through from `create_km_plot(type = "loglog")`.
- Updated documentation to clarify that censor marks are supported on
  log-log plots while median lines, CI bands, risk tables, and landmarks
  are intentionally omitted.
- Added test for log-log plot censor marks.

## pharmhand 0.1.20.9000

### Improvements

- Added AE cumulative incidence plots via
  [`create_ae_cumulative_incidence_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_cumulative_incidence_plot.md).

## pharmhand 0.1.19.9000

### Improvements

- Added ANCOVA adjustment for continuous endpoints via
  [`ancova_adjust_continuous()`](https://sims1253.github.io/pharmhand/branch/dev/reference/ancova_adjust_continuous.md).

## pharmhand 0.1.18.9000

### Improvements

- Added non-inferiority testing for continuous and binary endpoints via
  [`test_non_inferiority()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_non_inferiority.md).

## pharmhand 0.1.17.9000

### Improvements

- Added time-to-first adverse event analysis with KM tables, plots, and
  Cox HRs via
  [`create_time_to_first_ae()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_time_to_first_ae.md).

## pharmhand 0.1.16.9000

### Improvements

- Added exposure-adjusted incidence rate tables with Poisson confidence
  intervals via
  [`calculate_exposure_adjusted_rate()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_exposure_adjusted_rate.md)
  and
  [`create_ae_exposure_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_exposure_table.md).

## pharmhand 0.1.15.9000

### Improvements

- Added configurable warnings for small subgroups in
  [`create_subgroup_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_subgroup_table.md)
  and
  [`create_subgroup_analysis_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_subgroup_analysis_table.md)
  via `min_subgroup_size` (default: 20).

## pharmhand 0.1.14.9000

### Documentation

- Documented the complete proportional hazards assumption testing suite
  for Cox models: Schoenfeld residual tests via
  [`test_ph_assumption()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_ph_assumption.md),
  log-log survival plots via
  [`create_loglog_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_loglog_plot.md),
  and automatic warnings in
  [`create_tte_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_tte_summary_table.md).

## pharmhand 0.1.13.9000

### Improvements

- Added automatic PH violation warnings in
  [`create_tte_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_tte_summary_table.md)
  with guidance on stratified or alternative survival models.

## pharmhand 0.1.12.9000

### Improvements

- Added log-log survival plots for PH diagnostics via
  [`create_loglog_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_loglog_plot.md)
  and `create_km_plot(type = "loglog")`.

## pharmhand 0.1.11.9000

### Improvements

- Added
  [`test_ph_assumption()`](https://sims1253.github.io/pharmhand/branch/dev/reference/test_ph_assumption.md)
  to run Schoenfeld residual tests with optional diagnostic plots.

## pharmhand 0.1.10.9000

### Improvements

- Added
  [`detect_floor_ceiling()`](https://sims1253.github.io/pharmhand/branch/dev/reference/detect_floor_ceiling.md)
  to flag floor/ceiling effects in PRO scores by visit and treatment
  arm.

## pharmhand 0.1.9.9000

### Improvements

- Added NNH (Number Needed to Harm) to AE comparison tables with
  estimability handling.

## pharmhand 0.1.8.9000

### Breaking Changes

- `build_time_to_event()` in the safety report example no longer assumes
  a 100-day default duration. Supply `default_duration` when TRTDURD
  cannot be derived from TRTSDT/TRTEDT.

## pharmhand 0.1.7.9000

### Improvements

- Standardized NA display via `pharmhand.na_string` across reporting
  themes and clinical table styling.

## pharmhand 0.1.6.9000

### Documentation

- Documented fixed-column handling for the Module 4 table template.

## pharmhand 0.1.5.9000

### Documentation

- Clarified IQWiG flextable theme formatting and decimal separator
  behavior.

## pharmhand 0.1.4.9000

### Documentation

- Finalized IQWiG-compliant CI formatter documentation.

## pharmhand 0.1.3.9000

### Improvements

- Added validation for `default_duration` in the safety report example.

### Documentation

- Clarified palette resolution for
  [`create_km_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_km_plot.md)
  and CI formatting guidance.
- Added a note on comparison-table entry points in the safety tables
  vignette.
- Fixed the pkgdown badge URL in the README.

### Maintenance

- Updated compliance tests to use flextable’s public header removal API.

## pharmhand 0.1.1.9000

### Bug Fixes

- Fixed redundant null check in
  [`check_gba_compliance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/check_gba_compliance.md)
  title validation.
- Fixed
  [`to_gba_template()`](https://sims1253.github.io/pharmhand/branch/dev/reference/to_gba_template.md)
  list handling that would overwrite same file path.
- Fixed list handling in
  [`check_gba_compliance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/check_gba_compliance.md)
  for unnamed elements.

### Improvements

- [`theme_iqwig()`](https://sims1253.github.io/pharmhand/branch/dev/reference/theme_iqwig.md)
  and
  [`theme_gba()`](https://sims1253.github.io/pharmhand/branch/dev/reference/theme_gba.md)
  now apply the `decimal_separator` parameter to numeric columns (was
  declared but unused).
- `SingleArmStudy` now has a `treatment_var` property for consistency
  with `TwoArmStudy` and `MultiArmStudy`.

### Documentation

- Updated G-BA citation to 2024 reference.
- Added missing `adae` and `adrs` datasets to README examples.

### Maintenance

- Added `notes/` to `.gitignore`.
- Fixed lintr line-length violations.

## pharmhand 0.1.0.9000

### Breaking Changes

- Renamed `create_ae_table()` to
  [`create_ae_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_summary_table.md)
  for clarity.

### Improvements

- [`format_pvalue()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_pvalue.md)
  and
  [`format_ci()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_ci.md)
  now accept `trim` and `na_string` parameters for more flexible output
  formatting.

### Documentation

- Added README.Rmd with rendered demo table and KM plot figures.
- Improved pkgdown site with sandstone theme and dark mode support.
- Simplified Articles dropdown to flat list.
- Added description to baseline-tables vignette.
- Fixed dev status badges visibility with
  `development: mode: unreleased`.

### Maintenance

- Cleaned up `_pkgdown.yml` by removing redundant defaults.
- Simplified `extra.scss` to only flextable dark mode fixes.
- Updated example scripts and vignettes for function rename.

## pharmhand 0.0.18.9000

### New Features

- Add
  [`to_gba_template()`](https://sims1253.github.io/pharmhand/branch/dev/reference/to_gba_template.md)
  to export ClinicalTable/ClinicalReport objects in G-BA Module 4
  format.

### Tests

- Add coverage for G-BA template export.

## pharmhand 0.0.17.9000

### New Features

- Add
  [`create_hta_module4_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_hta_module4_table.md)
  for standardized G-BA Module 4 tables.

### Tests

- Add coverage for Module 4 table creation.

## pharmhand 0.0.16.9000

### New Features

- Add
  [`check_gba_compliance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/check_gba_compliance.md)
  for pre-export validation of G-BA Module 4 tables.
- Mark IQWiG and G-BA flextable themes with a `pharmhand_theme`
  attribute.

### Tests

- Add coverage for G-BA compliance checks.

## pharmhand 0.0.15.9000

### Breaking Changes

- Remove `fmt_pvalue()` format preset. Use
  [`format_pvalue()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_pvalue.md)
  for IQWiG-compliant p-value formatting.

### New Features

- IQWiG-compliant formatting functions:
  [`format_pvalue()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_pvalue.md)
  and
  [`format_ci()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_ci.md).
- Locale-aware helpers:
  [`format_number()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_number.md)
  and
  [`format_percentage()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_percentage.md).
- New flextable themes:
  [`theme_iqwig()`](https://sims1253.github.io/pharmhand/branch/dev/reference/theme_iqwig.md)
  and
  [`theme_gba()`](https://sims1253.github.io/pharmhand/branch/dev/reference/theme_gba.md).

### Tests

- Add coverage for IQWiG formatting and theme functions.

### Maintenance

- Remove duplicate formatting helpers to avoid shadowing.

## pharmhand 0.0.14.9000

### Breaking Changes

- **Endpoint classes unified**: `PrimaryEndpoint`, `SecondaryEndpoint`,
  and `SafetyEndpoint` have been replaced with a single `Endpoint`
  class. Use the `category` property (“primary”, “secondary”, “safety”,
  “exploratory”) instead.

- **Study class renamed**: `OneArmStudy` is now `SingleArmStudy` for
  clarity.

- **New property name**: `TwoArmStudy@group_var` is now
  `TwoArmStudy@treatment_var`.

### New Features

- **Study base class**: New abstract `Study` class provides common
  properties for all study types.

- **MultiArmStudy**: New class for studies with 3+ treatment arms.

- **StudySet**: New class for collections of studies (meta-analysis,
  NMA).

- **Statistical result classes**: New `StatResult` hierarchy with
  `ComparisonResult`, `MetaResult`, and `EvidenceGrade` for type-safe
  results.

### Documentation

- Added `notes/ARCHITECTURE_DECISIONS.md` documenting class hierarchy
  and naming conventions.

## pharmhand 0.0.13.9000

### Fixes

- Make chef integration resilient to missing stratification columns
- Add data.table guardrails and endpoints validation for chef pipeline
  usage

### Maintenance

- Add internal ph_inform helper for consistent messaging
- Scope custom translation keys to requested locale in
  list_translation_keys()

## pharmhand 0.0.12.9000

### Maintenance

- Wrap add_translation assignment to satisfy lintr line-length limits

## pharmhand 0.0.11.9000

### Fixes

- Avoid `Rplot.pdf` during tests by building plots instead of printing

### Documentation

- Simplify LayeredTable usage default to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html)

### Maintenance

- Relax R version requirement to \>= 4.1.0
- Consolidate development artifacts under `notes/` and exclude the
  directory in `.Rbuildignore`
- Use [`paste0()`](https://rdrr.io/r/base/paste.html) for
  missing-translation warnings

## pharmhand 0.0.10.9000

### Fixes

- Enforce unique ADSL USUBJID when joining subgroup variables
- Standardize data frame validation in lab summary, shift, and subgroup
  tables

### Documentation

- Clarify Love plot threshold override for BalanceAssessment inputs
- Clarify multi-level categorical SMD return semantics

### Tests

- Add coverage for duplicate ADSL USUBJID checks in subgroup analysis

### Maintenance

- Use modifyList for translation merges
- Drop explicit tidyselect import (dplyr re-exports used)

## pharmhand 0.0.9.9000

### Fixes

- NNT CI crossing zero now returns non-estimable bounds
- NA handling in assert_character_scalar
- Translations merge precedence
- ClinicalContent validation now uses assert_true for clearer errors

### Documentation

- Logit/raw SMD method documentation
- Hommel method in adjust_method

### Tests

- calculate_nnt coverage

## pharmhand 0.0.8.9000

### Breaking changes

- Removed save_as_pdf() and PDF output for ClinicalTable format_content.

### Fixes

- AE comparison multi-arm columns
- Zebra striping order
- Subgroup vars from adsl
- Required column validation for primary/CFB tables

### Improvements

- continuous_threshold parameter + multi-arm warning
- Love plot annotation placement
- Rounded KM y labels/pt constant
- Clarified errors/ph_abort consistency

## pharmhand 0.0.7.9000

### Maintenance

- Removed cli and scales from Imports and replaced with base R helpers
- Moved data.table and patchwork to Suggests (optional for chef
  integration and risk tables)
- Removed unused Suggests: yaml, checkmate, webshot2
- Updated example report scripts to use base R messaging

## pharmhand 0.0.6.9000

### New features

- Added multiplicity adjustment for p-values:
  - [`adjust_pvalues()`](https://sims1253.github.io/pharmhand/branch/dev/reference/adjust_pvalues.md):
    Apply Bonferroni, Holm, BH, or Hochberg corrections
  - [`create_subgroup_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_subgroup_table.md)
    now supports `adjust_method` parameter
- Added
  [`calculate_nnt()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_nnt.md)
  for Number Needed to Treat calculation with confidence intervals

## pharmhand 0.0.5.9000

### New features

- Added comprehensive localization support for German HTA (G-BA/AMNOG)
  dossiers:
  - [`set_locale()`](https://sims1253.github.io/pharmhand/branch/dev/reference/set_locale.md)
    /
    [`get_locale()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_locale.md):
    Set and retrieve current locale (“en” or “de”)
  - [`tr()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr.md):
    Translate keys to current locale
  - [`tr_col()`](https://sims1253.github.io/pharmhand/branch/dev/reference/tr_col.md):
    Translate data frame column names
  - [`get_translations()`](https://sims1253.github.io/pharmhand/branch/dev/reference/get_translations.md):
    Get all translations for a locale
  - [`add_translation()`](https://sims1253.github.io/pharmhand/branch/dev/reference/add_translation.md):
    Add custom translations
  - [`reset_custom_translations()`](https://sims1253.github.io/pharmhand/branch/dev/reference/reset_custom_translations.md):
    Clear custom translations
  - [`list_translation_keys()`](https://sims1253.github.io/pharmhand/branch/dev/reference/list_translation_keys.md):
    List all available translation keys
  - Built-in German translations for common clinical trial terms
    (demographics, AEs, efficacy endpoints, populations)

## pharmhand 0.0.4.9000

### New features

- Unified treatment variable API across safety and efficacy tables:
  - All table functions now consistently use `trt_var = "TRT01P"` as
    default
  - [`create_primary_endpoint_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_primary_endpoint_table.md),
    [`create_cfb_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_cfb_summary_table.md),
    [`create_vs_by_visit_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_vs_by_visit_table.md)
    now accept `trt_var` parameter
  - Safety tables (`create_ae_table()`) default changed from “TRT01A” to
    “TRT01P” for consistency
- Added
  [`create_ae_comparison_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_comparison_table.md)
  for adverse event comparisons with risk metrics:
  - Accessible via `create_ae_table(type = "comparison")`
  - Calculates risk difference (RD) and risk ratio (RR) with confidence
    intervals
  - Supports grouping by SOC, PT, or overall
  - Configurable reference group and incidence threshold filtering
  - Chi-square or Fisher’s exact test for p-values

## pharmhand 0.0.3.9000

### New features

- Added standardized mean difference (SMD) functions for GBA baseline
  balance assessment:
  - [`calculate_smd()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd.md):
    Cohen’s d and Hedges’ g for continuous variables
  - [`calculate_smd_binary()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd_binary.md):
    Arcsine/logit methods for binary variables
  - [`calculate_smd_from_data()`](https://sims1253.github.io/pharmhand/branch/dev/reference/calculate_smd_from_data.md):
    Auto-detect variable type from data
  - [`add_smd_to_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/add_smd_to_table.md):
    Add SMD column to demographics tables
  - [`assess_baseline_balance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_baseline_balance.md):
    Comprehensive multi-variable assessment
  - [`create_love_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_love_plot.md):
    Covariate balance visualization (Love plot)
  - `BalanceAssessment` S7 class for storing assessment results
- Updated baseline-tables vignette with SMD examples

## pharmhand 0.0.2.9000

### Documentation

- Vignettes now display all generated tables using flextable rendering

- Large tables (SOC/PT, lab shift, medical history, conmeds) are
  collapsible with `<details>` tags

- Added dark mode support for pkgdown site tables via CSS overrides for
  flextable compatibility

- Fixed
  [`format_spec()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_spec.md)
  roxygen documentation

### Bug fixes

- Fixed vignette data loading to use correct `pharmaverseadam` dataset
  names (`adtte_onco`, `adrs_onco`)

- Fixed treatment variable usage (`ARM` vs `TRT01P`) for oncology
  datasets

## pharmhand 0.0.1.9000

### New features

- S7 classes: `ADaMData`, `ClinicalTable`, `ClinicalPlot`,
  `ClinicalReport`, `StudyResult`

- `ADaMData` wraps ADaM datasets with automatic population filtering

- `create_ae_table()` generates AE tables with `type` argument

- [`create_tte_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_tte_summary_table.md)
  for time-to-event analysis

- [`create_responder_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_responder_table.md)
  for response rates with CI and comparisons

- [`create_subgroup_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_subgroup_table.md)
  and
  [`create_forest_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_forest_plot.md)
  for subgroups

- [`create_km_plot()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_km_plot.md)
  for Kaplan-Meier curves

- [`create_cfb_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_cfb_summary_table.md)
  for change-from-baseline

- [`create_lab_shift_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_lab_shift_table.md)
  for lab shifts

- [`create_demographics_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_demographics_table.md),
  [`create_disposition_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_disposition_table.md),
  [`create_population_summary_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_population_summary_table.md)

### Reporting

- `ClinicalReport` collects tables and plots for Word export

- [`generate_word()`](https://sims1253.github.io/pharmhand/branch/dev/reference/generate_word.md)
  and
  [`write_docx()`](https://sims1253.github.io/pharmhand/branch/dev/reference/write_docx.md)
  export to `.docx`

### Formatting

- [`format_pvalue()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_pvalue.md),
  [`format_ci()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_ci.md),
  [`format_percentage()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_percentage.md),
  [`format_number()`](https://sims1253.github.io/pharmhand/branch/dev/reference/format_number.md)

- `FormatSpec` and `CompositeFormat` classes

### chef integration

- `run_chef_pipeline()` executes chef pipelines

- `chef_to_analysis_results()` converts chef output

## pharmhand 0.0.0.9000

### New features

- Initial package release
- S7 class system for clinical content (`ADaMData`, `ClinicalTable`,
  `ClinicalPlot`, `ClinicalReport`, `StudyResult`)
- ADaM dataset handling with population filters
- Safety tables: AE overview, SOC/PT summaries, severity, relationship,
  SAEs, deaths
- Efficacy tables: primary endpoints, subgroup analyses
- Word document generation via officer/flextable
- Integration with chef package for HTA/AMNOG analyses

### Documentation

- Added package documentation and examples
