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
