# Changelog

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
  - Safety tables
    ([`create_ae_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_table.md))
    default changed from “TRT01A” to “TRT01P” for consistency
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

- [`create_ae_table()`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_ae_table.md)
  generates AE tables with `type` argument

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
