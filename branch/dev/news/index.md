# Changelog

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
