# Changelog

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

- [`run_chef_pipeline()`](https://sims1253.github.io/pharmhand/branch/dev/reference/run_chef_pipeline.md)
  executes chef pipelines

- [`chef_to_analysis_results()`](https://sims1253.github.io/pharmhand/branch/dev/reference/chef_to_analysis_results.md)
  converts chef output

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
