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
