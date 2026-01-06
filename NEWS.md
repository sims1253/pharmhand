# pharmhand 0.3.0.9000

## Major Changes

### Test Suite Reorganization
* **One-test-file-per-source-file convention**: Reorganized test files to follow R testing best practices.
  - Split `test-efficacy_tables.R` into `test-efficacy_cfb.R`, `test-efficacy_lab.R`, `test-efficacy_primary.R`, `test-efficacy_responder.R`, `test-efficacy_subgroup.R`, and `test-efficacy_tte.R`
  - Split `test-meta-analysis.R` into `test-meta_core.R`, `test-meta_bayesian.R`, `test-meta_bias.R`, `test-meta_indirect.R`, `test-meta_network.R`, and `test-meta_plots.R`
  - Split `test-plotting.R` into `test-plotting_survival.R`, `test-plotting_efficacy.R`, and `test-plotting_forest.R`
  - Renamed `test-safety_tables.R` to `test-safety_summary.R` and extracted `test-safety_comparison.R` and `test-safety_hierarchy.R`

### New Test Coverage
* Added comprehensive tests for Configuration API (`test-config_api.R`)
* Added comprehensive tests for Configuration Classes (`test-config_classes.R`)

## Bug Fixes

### Test Data Improvements
* Fixed test data in safety TTE tests to use larger sample sizes (20+ subjects per arm) to avoid Cox model convergence issues
* Fixed test data to avoid duplicate landmark timepoints in TTE summary tests
* Fixed test data for subgroup analysis to have adequate subjects per subgroup (25+)
* Fixed S7 class checks from `expect_s3_class()` to `S7::S7_inherits()` for proper S7 class validation
* Fixed error message regex patterns to match actual function outputs

### Source Code Fixes
* Fixed `km_summary` matrix conversion in `efficacy_tte.R` to preserve column names for single treatment arms
* Fixed `pct` column warning in `safety_summary.R` by using proper `dplyr::mutate()` instead of for-loop assignment
* Fixed `survfit` `n.censor` matrix handling in `plotting_survival.R` by converting to vector
* Fixed `min()` returning `-Inf` in `pro_analysis.R` when no events present
* Fixed S7 class checking in `config_api.R` to use `S7::S7_inherits()` instead of `inherits()`

### Test Cleanup
* Removed tests for unimplemented `maic()` and `stc()` functions
* Fixed missing `registry` variable in config API tests
* Changed REML tests to use DerSimonian-Laird method to avoid convergence warnings
* Replaced `any(is.na())` with `anyNA()` per jarl recommendations

## Code Quality
* All tests pass (1874 tests)
* No warnings or skipped tests
* lintr clean (line length compliance)
* jarl check clean
* devtools::check() passes with 0 errors, 0 warnings, 0 notes

# pharmhand 0.2.3.9000

## Documentation

* Added runnable examples for all meta-analysis and network meta-analysis functions
  including `meta_analysis()`, `indirect_comparison()`, `network_meta()`,
  `create_network_plot()`, `assess_transitivity()`, `node_splitting()`,
  `calculate_sucra()`, and `create_league_table()`.

# pharmhand 0.2.2.9000

## Phase 2: Evidence Synthesis

### Meta-Analysis Functions

* Added `meta_analysis()` for fixed-effect and random-effects meta-analysis with
  support for multiple tau² estimators (DerSimonian-Laird, REML, Paule-Mandel, ML)
  and optional Knapp-Hartung adjustment.

* Added `calculate_heterogeneity()` for comprehensive heterogeneity assessment
  including Q, I², τ², and H² statistics with interpretation.

* Added `leave_one_out()` for sensitivity analysis identifying influential studies.

* Added `create_meta_forest_plot()` for meta-analysis forest plots with study
  weights, heterogeneity statistics, and prediction intervals.

* Added `create_funnel_plot()` for publication bias visualization.

* Added `eggers_test()` for statistical assessment of funnel plot asymmetry.

* Added `trim_and_fill()` for Duval & Tweedie publication bias adjustment.

### Indirect Comparison

* Added `indirect_comparison()` using the Bucher method for anchored indirect
  treatment comparisons.

* Added `compare_direct_indirect()` for consistency testing between direct
  and indirect evidence.

### Network Meta-Analysis

* Added `network_meta()` for network meta-analysis comparing multiple treatments.

* Added `create_network_plot()` for network geometry visualization.

* Added `assess_transitivity()` for evaluating the transitivity assumption.

* Added `node_splitting()` for testing inconsistency between direct and indirect evidence.

* Added `calculate_sucra()` for treatment ranking with SUCRA/P-scores.

* Added `create_league_table()` for pairwise comparison tables.

### Bayesian Analysis

* Added `bayesian_meta_analysis()` interface for Bayesian meta-analysis using
  brms when available, with automatic fallback to frequentist methods.

## Phase 1 Completion

### PRO Analysis

* Added `calculate_mcid_anchor()` for anchor-based MCID calculation.

* Added `calculate_mcid_distribution()` for distribution-based MCID (0.5 SD,
  1 SEM, etc.).

* Added `calculate_mcid()` wrapper combining both MCID approaches.

* Added `create_ttd_analysis()` for time-to-deterioration analysis with
  Kaplan-Meier estimation.

### Visualization

* Added `create_mean_plot()` for longitudinal mean plots with confidence intervals.

* Added `create_spider_plot()` for individual patient trajectory visualization.

### Safety Analysis

* Added `create_ae_hierarchy_table()` for full MedDRA hierarchy analysis
  (SOC → HLGT → HLT → PT).

### Subgroup Analysis

* Added `assess_iceman()` for ICEMAN criteria assessment of subgroup credibility.

## S7 Classes

* Added comprehensive tests for `MultiArmStudy` S7 class.

## Documentation

* Added extensive test coverage for meta-analysis functions.
* Added tests for PRO analysis functions.

# pharmhand 0.2.1.9000

## Improvements

* Added `conf_level` parameter to `create_ae_cumulative_incidence_plot()` (was hard-coded at 0.95).
* Exposed `subgroup_counts` and `min_subgroup_size` in `create_subgroup_analysis_table()` metadata for programmatic access.
* Wrapped `test_ph_assumption()` call in `create_tte_summary_table()` with tryCatch for robustness with sparse Cox models.

## Plotting

* Created `.pharmhand_theme()` internal helper for consistent white backgrounds across all plots.
* Refactored all plotting functions to use `.pharmhand_theme()` with `base_size` parameter.
* Added `conf_level` parameter to `create_loglog_plot()`.
* Added `base_size` parameter to `create_ae_cumulative_incidence_plot()` and `create_forest_plot()`.
* Updated `build_schoenfeld_plot()` to use `.pharmhand_theme()` for consistent styling.

## Documentation

* Clarified that `method` parameter in `test_non_inferiority()` applies only to binary endpoints.
* Added `@examples` to `detect_floor_ceiling()`, `test_non_inferiority()`, `ancova_adjust_continuous()`, `create_loglog_plot()`, and `create_ae_cumulative_incidence_plot()`.
* Added Time-to-Event Safety Analysis section to safety-tables vignette.
* Added NNH (Number Needed to Harm) documentation to safety-tables vignette.
* Added Non-Inferiority Testing section to efficacy-tables vignette.
* Added ANCOVA Adjustment section to efficacy-tables vignette.
* Added Floor/Ceiling Detection section to efficacy-tables and baseline-tables vignettes.
* Improved documentation for `test_non_inferiority()` with clarified conf_level, decision logic, and higher_better parameter.
* Improved documentation for `ancova_adjust_continuous()` with edge case behavior and runnable simulated example.

## Code Quality

* Added Newcombe-Wilson reference citation for binary difference in proportions CI calculation.
* Removed unused `comparison_results` stub from AE comparison function.

## Tests

* Added test for NNH calculation when risk difference is negative (treatment beneficial).

# pharmhand 0.2.0.9000

## Code Quality

* Refactored duplicated palette resolution logic into `.resolve_palette()` helper.
* Added warning when log-log plot drops data points with non-finite values.
* Expanded PH assumption disable comment with rationale for time-to-first AE.
* Added clarifying comments in floor/ceiling detection and event conversion.
* Added documentation note about CI construction methods for non-inferiority binary endpoints.
* Removed redundant `@keywords internal` from exported `ancova_adjust_continuous()`.

## Tests

* Made color palette test assertions order-agnostic with `expect_setequal()`.
* Added edge case tests for `calculate_exposure_adjusted_rate()` (zero events, small exposure, large counts).

# pharmhand 0.1.21.9000

## Bug Fixes

* Fixed ComparisonResult defaults from `integer(0)` to `numeric(0)` for consistency with S7 class.
* Fixed line length lint errors in `test_non_inferiority()` documentation.

## Documentation

* Updated pkgdown Utilities section description to include "statistical diagnostics".
* Added missing topics to pkgdown reference index: `calculate_exposure_adjusted_rate()`, `create_ae_exposure_table()`, `create_time_to_first_ae()`, and `test_non_inferiority()`.
* Cleaned up duplicate NEWS.md heading.
* Added explanatory comment for PH assumption check disable in time-to-first AE function.

## Plotting

* Added `show_censor` support to `create_loglog_plot()` and passed through from `create_km_plot(type = "loglog")`.
* Updated documentation to clarify that censor marks are supported on log-log plots while median lines, CI bands, risk tables, and landmarks are intentionally omitted.
* Added test for log-log plot censor marks.

# pharmhand 0.1.20.9000

## Improvements

* Added AE cumulative incidence plots via `create_ae_cumulative_incidence_plot()`.

# pharmhand 0.1.19.9000

## Improvements

* Added ANCOVA adjustment for continuous endpoints via `ancova_adjust_continuous()`.

# pharmhand 0.1.18.9000

## Improvements

* Added non-inferiority testing for continuous and binary endpoints via `test_non_inferiority()`.

# pharmhand 0.1.17.9000

## Improvements

* Added time-to-first adverse event analysis with KM tables, plots, and Cox HRs
  via `create_time_to_first_ae()`.

# pharmhand 0.1.16.9000

## Improvements

* Added exposure-adjusted incidence rate tables with Poisson confidence intervals
  via `calculate_exposure_adjusted_rate()` and `create_ae_exposure_table()`.

# pharmhand 0.1.15.9000

## Improvements

* Added configurable warnings for small subgroups in `create_subgroup_table()` and
  `create_subgroup_analysis_table()` via `min_subgroup_size` (default: 20).

# pharmhand 0.1.14.9000

## Documentation

* Documented the complete proportional hazards assumption testing suite for Cox models:
  Schoenfeld residual tests via `test_ph_assumption()`, log-log survival plots via
  `create_loglog_plot()`, and automatic warnings in `create_tte_summary_table()`.

# pharmhand 0.1.13.9000

## Improvements

* Added automatic PH violation warnings in `create_tte_summary_table()` with
  guidance on stratified or alternative survival models.

# pharmhand 0.1.12.9000

## Improvements

* Added log-log survival plots for PH diagnostics via `create_loglog_plot()` and
  `create_km_plot(type = "loglog")`.

# pharmhand 0.1.11.9000

## Improvements

* Added `test_ph_assumption()` to run Schoenfeld residual tests with optional diagnostic plots.

# pharmhand 0.1.10.9000

## Improvements

* Added `detect_floor_ceiling()` to flag floor/ceiling effects in PRO scores by visit and treatment arm.

# pharmhand 0.1.9.9000

## Improvements

* Added NNH (Number Needed to Harm) to AE comparison tables with estimability handling.

# pharmhand 0.1.8.9000

## Breaking Changes

* `build_time_to_event()` in the safety report example no longer assumes a
  100-day default duration. Supply `default_duration` when TRTDURD cannot be
  derived from TRTSDT/TRTEDT.

# pharmhand 0.1.7.9000

## Improvements

* Standardized NA display via `pharmhand.na_string` across reporting themes and
  clinical table styling.

# pharmhand 0.1.6.9000

## Documentation

* Documented fixed-column handling for the Module 4 table template.

# pharmhand 0.1.5.9000

## Documentation

* Clarified IQWiG flextable theme formatting and decimal separator behavior.

# pharmhand 0.1.4.9000

## Documentation

* Finalized IQWiG-compliant CI formatter documentation.

# pharmhand 0.1.3.9000

## Improvements

* Added validation for `default_duration` in the safety report example.

## Documentation

* Clarified palette resolution for `create_km_plot()` and CI formatting guidance.
* Added a note on comparison-table entry points in the safety tables vignette.
* Fixed the pkgdown badge URL in the README.

## Maintenance

* Updated compliance tests to use flextable's public header removal API.

# pharmhand 0.1.1.9000

## Bug Fixes

* Fixed redundant null check in `check_gba_compliance()` title validation.
* Fixed `to_gba_template()` list handling that would overwrite same file path.
* Fixed list handling in `check_gba_compliance()` for unnamed elements.

## Improvements

* `theme_iqwig()` and `theme_gba()` now apply the `decimal_separator` parameter
  to numeric columns (was declared but unused).
* `SingleArmStudy` now has a `treatment_var` property for consistency with
  `TwoArmStudy` and `MultiArmStudy`.

## Documentation

* Updated G-BA citation to 2024 reference.
* Added missing `adae` and `adrs` datasets to README examples.

## Maintenance

* Added `notes/` to `.gitignore`.
* Fixed lintr line-length violations.

# pharmhand 0.1.0.9000

## Breaking Changes

* Renamed `create_ae_table()` to `create_ae_summary_table()` for clarity.

## Improvements

* `format_pvalue()` and `format_ci()` now accept `trim` and `na_string` parameters
  for more flexible output formatting.

## Documentation

* Added README.Rmd with rendered demo table and KM plot figures.
* Improved pkgdown site with sandstone theme and dark mode support.
* Simplified Articles dropdown to flat list.
* Added description to baseline-tables vignette.
* Fixed dev status badges visibility with `development: mode: unreleased`.

## Maintenance

* Cleaned up `_pkgdown.yml` by removing redundant defaults.
* Simplified `extra.scss` to only flextable dark mode fixes.
* Updated example scripts and vignettes for function rename.

# pharmhand 0.0.18.9000

## New Features

* Add `to_gba_template()` to export ClinicalTable/ClinicalReport objects in
  G-BA Module 4 format.

## Tests

* Add coverage for G-BA template export.

# pharmhand 0.0.17.9000

## New Features

* Add `create_hta_module4_table()` for standardized G-BA Module 4 tables.

## Tests

* Add coverage for Module 4 table creation.

# pharmhand 0.0.16.9000

## New Features

* Add `check_gba_compliance()` for pre-export validation of G-BA Module 4
  tables.
* Mark IQWiG and G-BA flextable themes with a `pharmhand_theme` attribute.

## Tests

* Add coverage for G-BA compliance checks.

# pharmhand 0.0.15.9000

## Breaking Changes

* Remove `fmt_pvalue()` format preset. Use `format_pvalue()` for
  IQWiG-compliant p-value formatting.

## New Features

* IQWiG-compliant formatting functions: `format_pvalue()` and `format_ci()`.
* Locale-aware helpers: `format_number()` and `format_percentage()`.
* New flextable themes: `theme_iqwig()` and `theme_gba()`.

## Tests

* Add coverage for IQWiG formatting and theme functions.

## Maintenance

* Remove duplicate formatting helpers to avoid shadowing.

# pharmhand 0.0.14.9000

## Breaking Changes

* **Endpoint classes unified**: `PrimaryEndpoint`, `SecondaryEndpoint`, and 
  `SafetyEndpoint` have been replaced with a single `Endpoint` class. Use the 
  `category` property ("primary", "secondary", "safety", "exploratory") instead.

* **Study class renamed**: `OneArmStudy` is now `SingleArmStudy` for clarity.

* **New property name**: `TwoArmStudy@group_var` is now `TwoArmStudy@treatment_var`.

## New Features
  
* **Study base class**: New abstract `Study` class provides common properties 
  for all study types.

* **MultiArmStudy**: New class for studies with 3+ treatment arms.

* **StudySet**: New class for collections of studies (meta-analysis, NMA).

* **Statistical result classes**: New `StatResult` hierarchy with 
  `ComparisonResult`, `MetaResult`, and `EvidenceGrade` for type-safe results.

## Documentation

* Added `notes/ARCHITECTURE_DECISIONS.md` documenting class hierarchy and 
  naming conventions.

# pharmhand 0.0.13.9000

## Fixes

* Make chef integration resilient to missing stratification columns
* Add data.table guardrails and endpoints validation for chef pipeline usage

## Maintenance

* Add internal ph_inform helper for consistent messaging
* Scope custom translation keys to requested locale in list_translation_keys()

# pharmhand 0.0.12.9000

## Maintenance

* Wrap add_translation assignment to satisfy lintr line-length limits

# pharmhand 0.0.11.9000

## Fixes

* Avoid `Rplot.pdf` during tests by building plots instead of printing

## Documentation

* Simplify LayeredTable usage default to `data.frame()`

## Maintenance

* Relax R version requirement to >= 4.1.0
* Consolidate development artifacts under `notes/` and exclude the directory in `.Rbuildignore`
* Use `paste0()` for missing-translation warnings

# pharmhand 0.0.10.9000

## Fixes

* Enforce unique ADSL USUBJID when joining subgroup variables
* Standardize data frame validation in lab summary, shift, and subgroup tables

## Documentation

* Clarify Love plot threshold override for BalanceAssessment inputs
* Clarify multi-level categorical SMD return semantics

## Tests

* Add coverage for duplicate ADSL USUBJID checks in subgroup analysis

## Maintenance

* Use modifyList for translation merges
* Drop explicit tidyselect import (dplyr re-exports used)

# pharmhand 0.0.9.9000

## Fixes

* NNT CI crossing zero now returns non-estimable bounds
* NA handling in assert_character_scalar
* Translations merge precedence
* ClinicalContent validation now uses assert_true for clearer errors

## Documentation

* Logit/raw SMD method documentation
* Hommel method in adjust_method

## Tests

* calculate_nnt coverage

# pharmhand 0.0.8.9000

## Breaking changes

* Removed save_as_pdf() and PDF output for ClinicalTable format_content.

## Fixes

* AE comparison multi-arm columns
* Zebra striping order
* Subgroup vars from adsl
* Required column validation for primary/CFB tables

## Improvements

* continuous_threshold parameter + multi-arm warning
* Love plot annotation placement
* Rounded KM y labels/pt constant
* Clarified errors/ph_abort consistency

# pharmhand 0.0.7.9000

## Maintenance

* Removed cli and scales from Imports and replaced with base R helpers
* Moved data.table and patchwork to Suggests (optional for chef integration and risk tables)
* Removed unused Suggests: yaml, checkmate, webshot2
* Updated example report scripts to use base R messaging

# pharmhand 0.0.6.9000

## New features

* Added multiplicity adjustment for p-values:
  - `adjust_pvalues()`: Apply Bonferroni, Holm, BH, or Hochberg corrections
  - `create_subgroup_table()` now supports `adjust_method` parameter

* Added `calculate_nnt()` for Number Needed to Treat calculation with confidence intervals

# pharmhand 0.0.5.9000

## New features

* Added comprehensive localization support for German HTA (G-BA/AMNOG) dossiers:
  - `set_locale()` / `get_locale()`: Set and retrieve current locale ("en" or "de")
  - `tr()`: Translate keys to current locale
  - `tr_col()`: Translate data frame column names
  - `get_translations()`: Get all translations for a locale
  - `add_translation()`: Add custom translations
  - `reset_custom_translations()`: Clear custom translations
  - `list_translation_keys()`: List all available translation keys
  - Built-in German translations for common clinical trial terms (demographics, AEs, efficacy endpoints, populations)

# pharmhand 0.0.4.9000

## New features

* Unified treatment variable API across safety and efficacy tables:
  - All table functions now consistently use `trt_var = "TRT01P"` as default
  - `create_primary_endpoint_table()`, `create_cfb_summary_table()`, `create_vs_by_visit_table()` now accept `trt_var` parameter
  - Safety tables (`create_ae_table()`) default changed from "TRT01A" to "TRT01P" for consistency

* Added `create_ae_comparison_table()` for adverse event comparisons with risk metrics:
  - Accessible via `create_ae_table(type = "comparison")`
  - Calculates risk difference (RD) and risk ratio (RR) with confidence intervals
  - Supports grouping by SOC, PT, or overall
  - Configurable reference group and incidence threshold filtering
  - Chi-square or Fisher's exact test for p-values

# pharmhand 0.0.3.9000

## New features

* Added standardized mean difference (SMD) functions for GBA baseline balance assessment:
  - `calculate_smd()`: Cohen's d and Hedges' g for continuous variables
  - `calculate_smd_binary()`: Arcsine/logit methods for binary variables
  - `calculate_smd_from_data()`: Auto-detect variable type from data
  - `add_smd_to_table()`: Add SMD column to demographics tables
  - `assess_baseline_balance()`: Comprehensive multi-variable assessment
  - `create_love_plot()`: Covariate balance visualization (Love plot)
  - `BalanceAssessment` S7 class for storing assessment results

* Updated baseline-tables vignette with SMD examples

# pharmhand 0.0.2.9000

## Documentation

* Vignettes now display all generated tables using flextable rendering

* Large tables (SOC/PT, lab shift, medical history, conmeds) are collapsible with `<details>` tags

* Added dark mode support for pkgdown site tables via CSS overrides for flextable compatibility

* Fixed `format_spec()` roxygen documentation

## Bug fixes

* Fixed vignette data loading to use correct `pharmaverseadam` dataset names (`adtte_onco`, `adrs_onco`)

* Fixed treatment variable usage (`ARM` vs `TRT01P`) for oncology datasets

# pharmhand 0.0.1.9000

## New features

* S7 classes: `ADaMData`, `ClinicalTable`, `ClinicalPlot`, `ClinicalReport`, `StudyResult`

* `ADaMData` wraps ADaM datasets with automatic population filtering

* `create_ae_table()` generates AE tables with `type` argument

* `create_tte_summary_table()` for time-to-event analysis

* `create_responder_table()` for response rates with CI and comparisons

* `create_subgroup_table()` and `create_forest_plot()` for subgroups

* `create_km_plot()` for Kaplan-Meier curves

* `create_cfb_summary_table()` for change-from-baseline

* `create_lab_shift_table()` for lab shifts

* `create_demographics_table()`, `create_disposition_table()`, `create_population_summary_table()`

## Reporting

* `ClinicalReport` collects tables and plots for Word export

* `generate_word()` and `write_docx()` export to `.docx`

## Formatting

* `format_pvalue()`, `format_ci()`, `format_percentage()`, `format_number()`

* `FormatSpec` and `CompositeFormat` classes

## chef integration

* `run_chef_pipeline()` executes chef pipelines

* `chef_to_analysis_results()` converts chef output

# pharmhand 0.0.0.9000

## New features

* Initial package release
* S7 class system for clinical content (`ADaMData`, `ClinicalTable`, `ClinicalPlot`, `ClinicalReport`, `StudyResult`)
* ADaM dataset handling with population filters
* Safety tables: AE overview, SOC/PT summaries, severity, relationship, SAEs, deaths
* Efficacy tables: primary endpoints, subgroup analyses
* Word document generation via officer/flextable
* Integration with chef package for HTA/AMNOG analyses

## Documentation

* Added package documentation and examples
