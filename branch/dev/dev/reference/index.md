# Package index

## S7 Classes

Core S7 classes for clinical data structures and reporting objects.

- [`ADaMData()`](https://sims1253.github.io/pharmhand/dev/reference/ADaMData.md)
  : ADaMData Class
- [`AnalysisMeta()`](https://sims1253.github.io/pharmhand/dev/reference/AnalysisMeta.md)
  : AnalysisMeta Class
- [`AnalysisResults()`](https://sims1253.github.io/pharmhand/dev/reference/AnalysisResults.md)
  : AnalysisResults Class
- [`ClinicalPlot()`](https://sims1253.github.io/pharmhand/dev/reference/ClinicalPlot.md)
  : ClinicalPlot Class
- [`ClinicalReport()`](https://sims1253.github.io/pharmhand/dev/reference/ClinicalReport.md)
  : ClinicalReport Class
- [`ClinicalTable()`](https://sims1253.github.io/pharmhand/dev/reference/ClinicalTable.md)
  : ClinicalTable Class
- [`CompositeFormat()`](https://sims1253.github.io/pharmhand/dev/reference/CompositeFormat.md)
  : CompositeFormat S7 Class
- [`FormatSpec()`](https://sims1253.github.io/pharmhand/dev/reference/FormatSpec.md)
  : FormatSpec S7 Class
- [`LayeredTable()`](https://sims1253.github.io/pharmhand/dev/reference/LayeredTable.md)
  : Layered Table Class
- [`PerformanceReport()`](https://sims1253.github.io/pharmhand/dev/reference/PerformanceReport.md)
  : PerformanceReport Class
- [`ReportSection()`](https://sims1253.github.io/pharmhand/dev/reference/ReportSection.md)
  : ReportSection Class
- [`StudyResult()`](https://sims1253.github.io/pharmhand/dev/reference/StudyResult.md)
  : StudyResult Class

## Study Design Classes

Classes and methods for defining and analyzing clinical study designs.
Supports one-arm and two-arm study configurations.

- [`OneArmStudy()`](https://sims1253.github.io/pharmhand/dev/reference/OneArmStudy.md)
  : OneArmStudy Class
- [`TwoArmStudy()`](https://sims1253.github.io/pharmhand/dev/reference/TwoArmStudy.md)
  : TwoArmStudy Class
- [`analyze_study()`](https://sims1253.github.io/pharmhand/dev/reference/analyze_study.md)
  : Analyze Study (S7 Method)
- [`analyze_study_OneArmStudy()`](https://sims1253.github.io/pharmhand/dev/reference/analyze_study_OneArmStudy.md)
  : Analyze OneArmStudy
- [`analyze_study_TwoArmStudy()`](https://sims1253.github.io/pharmhand/dev/reference/analyze_study_TwoArmStudy.md)
  : Analyze TwoArmStudy
- [`analyze()`](https://sims1253.github.io/pharmhand/dev/reference/analyze.md)
  : Analyze ADaM datasets
- [`analyze_ADaMData()`](https://sims1253.github.io/pharmhand/dev/reference/analyze_ADaMData.md)
  : Analyze ADaMData

## HTA & Endpoint Classes

Health Technology Assessment (HTA) endpoint definitions and section
classes for structured reporting.

- [`HTAEndpoint()`](https://sims1253.github.io/pharmhand/dev/reference/HTAEndpoint.md)
  : HTAEndpoint Class
- [`HTASection()`](https://sims1253.github.io/pharmhand/dev/reference/HTASection.md)
  : HTASection Class
- [`PrimaryEndpoint()`](https://sims1253.github.io/pharmhand/dev/reference/PrimaryEndpoint.md)
  : PrimaryEndpoint Class
- [`SecondaryEndpoint()`](https://sims1253.github.io/pharmhand/dev/reference/SecondaryEndpoint.md)
  : SecondaryEndpoint Class
- [`SafetyEndpoint()`](https://sims1253.github.io/pharmhand/dev/reference/SafetyEndpoint.md)
  : SafetyEndpoint Class
- [`PopulationSection()`](https://sims1253.github.io/pharmhand/dev/reference/PopulationSection.md)
  : PopulationSection Class
- [`SOCPTSection()`](https://sims1253.github.io/pharmhand/dev/reference/SOCPTSection.md)
  : SOCPTSection Class
- [`SubgroupSection()`](https://sims1253.github.io/pharmhand/dev/reference/SubgroupSection.md)
  : SubgroupSection Class

## Efficacy Tables

Functions for creating efficacy analysis tables including time-to-event,
responder, and subgroup analyses.

- [`create_tte_summary_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_tte_summary_table.md)
  : Create Time-to-Event Summary Table
- [`create_responder_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_responder_table.md)
  : Create Responder Summary Table
- [`create_subgroup_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_subgroup_table.md)
  : Create Subgroup Analysis Table
- [`create_subgroup_analysis_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_subgroup_analysis_table.md)
  : Create Subgroup Analysis Table
- [`create_primary_endpoint_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_primary_endpoint_table.md)
  : Create Primary Endpoint Summary Table
- [`create_cfb_summary_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_cfb_summary_table.md)
  : Create Change from Baseline Summary Table

## Safety Tables

Functions for adverse event and safety analysis tables including
laboratory shifts and AE summaries.

- [`create_ae_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_ae_table.md)
  : Create Adverse Event Table
- [`create_lab_shift_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_lab_shift_table.md)
  : Create Laboratory Shift Table
- [`create_lab_summary_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_lab_summary_table.md)
  : Create Laboratory Summary Table
- [`create_conmeds_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_conmeds_table.md)
  : Create Concomitant Medications Table
- [`create_medical_history_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_medical_history_table.md)
  : Create Medical History Table
- [`create_vs_by_visit_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_vs_by_visit_table.md)
  : Create Vital Signs by Visit Table
- [`analyze_soc_pt()`](https://sims1253.github.io/pharmhand/dev/reference/analyze_soc_pt.md)
  : Analyze Adverse Events by SOC and PT
- [`calculate_ae_tte_data()`](https://sims1253.github.io/pharmhand/dev/reference/calculate_ae_tte_data.md)
  : Calculate AE TTE Data for a specific SOC

## Standard Tables

Demographics, disposition, baseline, and other standard clinical study
tables.

- [`create_demographics_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_demographics_table.md)
  : Create Demographics Table
- [`create_disposition_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_disposition_table.md)
  : Create Subject Disposition Table
- [`create_population_summary_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_population_summary_table.md)
  : Create Analysis Populations Summary Table
- [`create_region_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_region_table.md)
  : Create Enrollment by Region Table
- [`create_clinical_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_clinical_table.md)
  : Helper to create a ClinicalTable from AnalysisResults
- [`create_hta_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_hta_table.md)
  : Create HTA-Style Table
- [`calculate_baseline()`](https://sims1253.github.io/pharmhand/dev/reference/calculate_baseline.md)
  : Calculate Baseline Characteristics

## Plotting

Clinical visualization functions for Kaplan-Meier curves, forest plots,
and other graphics.

- [`create_km_plot()`](https://sims1253.github.io/pharmhand/dev/reference/create_km_plot.md)
  : Create Kaplan-Meier Plot
- [`create_forest_plot()`](https://sims1253.github.io/pharmhand/dev/reference/create_forest_plot.md)
  : Create Subgroup Forest Plot
- [`create_ae_km_plot_for_soc()`](https://sims1253.github.io/pharmhand/dev/reference/create_ae_km_plot_for_soc.md)
  : Create AE KM Plot for a specific SOC
- [`add_table()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  [`add_plot()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  [`add_section()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  [`add_content()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  : Add a table to a StudyResult
- [`save_plot_as()`](https://sims1253.github.io/pharmhand/dev/reference/save_plot_as.md)
  : Save ClinicalPlot to file
- [`save_as_png()`](https://sims1253.github.io/pharmhand/dev/reference/save_as_png.md)
  : Save ClinicalTable as PNG
- [`save_as_pdf()`](https://sims1253.github.io/pharmhand/dev/reference/save_as_pdf.md)
  : Save ClinicalTable as PDF

## Formatting

Number and content formatting utilities for data presentation. Includes
format specifications and composite formatters.

- [`format_number()`](https://sims1253.github.io/pharmhand/dev/reference/format_number.md)
  : Format numbers with specified decimal places
- [`format_percentage()`](https://sims1253.github.io/pharmhand/dev/reference/format_percentage.md)
  : Format values as percentages
- [`format_pvalue()`](https://sims1253.github.io/pharmhand/dev/reference/format_pvalue.md)
  : Format p-values
- [`format_ci()`](https://sims1253.github.io/pharmhand/dev/reference/format_ci.md)
  : Format confidence intervals
- [`format_content()`](https://sims1253.github.io/pharmhand/dev/reference/format_content.md)
  : Format clinical content to different output formats
- [`format_spec()`](https://sims1253.github.io/pharmhand/dev/reference/format_spec.md)
  : Create Format Specification
- [`composite_format()`](https://sims1253.github.io/pharmhand/dev/reference/composite_format.md)
  : Composite Format Specification
- [`apply_format()`](https://sims1253.github.io/pharmhand/dev/reference/apply_format.md)
  : Apply a format specification to values
- [`apply_composite()`](https://sims1253.github.io/pharmhand/dev/reference/apply_composite.md)
  : Apply a composite format
- [`fmt_n_pct()`](https://sims1253.github.io/pharmhand/dev/reference/format_presets.md)
  [`fmt_mean_sd()`](https://sims1253.github.io/pharmhand/dev/reference/format_presets.md)
  [`fmt_median_range()`](https://sims1253.github.io/pharmhand/dev/reference/format_presets.md)
  [`fmt_ci()`](https://sims1253.github.io/pharmhand/dev/reference/format_presets.md)
  [`fmt_pvalue()`](https://sims1253.github.io/pharmhand/dev/reference/format_presets.md)
  : Common Clinical Format Presets

## Reporting Engine

Word document generation and report output using officer/flextable.

- [`generate_word()`](https://sims1253.github.io/pharmhand/dev/reference/generate_word.md)
  : Generate a Word document from a ClinicalReport
- [`write_docx()`](https://sims1253.github.io/pharmhand/dev/reference/write_docx.md)
  : Write clinical content to a Word document
- [`write_docx_ClinicalReport()`](https://sims1253.github.io/pharmhand/dev/reference/write_docx_ClinicalReport.md)
  : Write ClinicalReport to Word (S7 Method)
- [`to_word()`](https://sims1253.github.io/pharmhand/dev/reference/to_word.md)
  : Convert clinical content to Word format
- [`add_table()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  [`add_plot()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  [`add_section()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  [`add_content()`](https://sims1253.github.io/pharmhand/dev/reference/add_content.md)
  : Add a table to a StudyResult
- [`add_to_docx()`](https://sims1253.github.io/pharmhand/dev/reference/add_to_docx.md)
  : Add content to a Word document
- [`create_study_report()`](https://sims1253.github.io/pharmhand/dev/reference/create_study_report.md)
  : Create Report from Study
- [`create_analysis_meta()`](https://sims1253.github.io/pharmhand/dev/reference/create_analysis_meta.md)
  : Create Analysis Metadata
- [`apply_clinical_style()`](https://sims1253.github.io/pharmhand/dev/reference/apply_clinical_style.md)
  : Apply Clinical Table Styling
- [`summarize_content()`](https://sims1253.github.io/pharmhand/dev/reference/summarize_content.md)
  : Generate a summary of clinical content

## Table Conversion

Convert clinical results to flextable or gt table formats for flexible
output rendering.

- [`as_flextable()`](https://sims1253.github.io/pharmhand/dev/reference/as_flextable.md)
  : Convert analysis results to flextable
- [`as_flextable_AnalysisResults()`](https://sims1253.github.io/pharmhand/dev/reference/as_flextable_AnalysisResults.md)
  : Convert AnalysisResults to flextable (S7 Method)
- [`as_gt()`](https://sims1253.github.io/pharmhand/dev/reference/as_gt.md)
  : Convert analysis results to gt
- [`as_gt_AnalysisResults()`](https://sims1253.github.io/pharmhand/dev/reference/as_gt_AnalysisResults.md)
  : Convert AnalysisResults to gt (S7 Method)
- [`layered_to_flextable()`](https://sims1253.github.io/pharmhand/dev/reference/layered_to_flextable.md)
  : Convert LayeredTable to flextable

## Layers System

Layered table construction system for building complex tables with
multiple summary statistics.

- [`CountLayer()`](https://sims1253.github.io/pharmhand/dev/reference/CountLayer.md)
  : Count Layer Class
- [`DescriptiveLayer()`](https://sims1253.github.io/pharmhand/dev/reference/DescriptiveLayer.md)
  : Descriptive Layer Class
- [`ShiftLayer()`](https://sims1253.github.io/pharmhand/dev/reference/ShiftLayer.md)
  : Shift Layer Class
- [`add_layer()`](https://sims1253.github.io/pharmhand/dev/reference/add_layer.md)
  : Add a layer to a LayeredTable
- [`build_layer()`](https://sims1253.github.io/pharmhand/dev/reference/build_layer.md)
  : Build a single layer
- [`build_table()`](https://sims1253.github.io/pharmhand/dev/reference/build_table.md)
  : Build a LayeredTable

## Chef Integration

Integration with the chef package for Health Technology Assessment (HTA)
analyses and statistical pipelines.

- [`run_chef_pipeline()`](https://sims1253.github.io/pharmhand/dev/reference/run_chef_pipeline.md)
  : Run Chef Pipeline with pharmhand Integration
- [`create_chef_endpoint()`](https://sims1253.github.io/pharmhand/dev/reference/create_chef_endpoint.md)
  : Create Chef Endpoint Specification
- [`chef_to_analysis_results()`](https://sims1253.github.io/pharmhand/dev/reference/chef_to_analysis_results.md)
  : Convert Chef Results to AnalysisResults
- [`get_chef_stat()`](https://sims1253.github.io/pharmhand/dev/reference/get_chef_stat.md)
  : Register Chef Statistical Functions
- [`list_chef_stats()`](https://sims1253.github.io/pharmhand/dev/reference/list_chef_stats.md)
  : List Available Chef Statistical Functions

## Subgroup Analysis

Functions for subgroup and interaction analyses in clinical trials.

- [`apply_subgroups()`](https://sims1253.github.io/pharmhand/dev/reference/apply_subgroups.md)
  : Apply Subgroup Analysis

## Utilities

Helper functions for data filtering, treatment information, and other
common operations.

- [`get_filtered_data()`](https://sims1253.github.io/pharmhand/dev/reference/get_filtered_data.md)
  : Get Filtered Data
- [`get_trt_n()`](https://sims1253.github.io/pharmhand/dev/reference/get_trt_n.md)
  : Get Treatment Group Counts

## Internal

Internal package documentation for architecture and modules.

- [`S7_classes`](https://sims1253.github.io/pharmhand/dev/reference/S7_classes.md)
  : S7 Classes for Clinical Study Reports
- [`S7_generics`](https://sims1253.github.io/pharmhand/dev/reference/S7_generics.md)
  : S7 Generics and Methods for Clinical Reports
- [`S7_registration`](https://sims1253.github.io/pharmhand/dev/reference/S7_registration.md)
  : S7 Method Registration and Package Initialization
- [`adam_core`](https://sims1253.github.io/pharmhand/dev/reference/adam_core.md)
  : ADaM Analysis Core
- [`chef_integration`](https://sims1253.github.io/pharmhand/dev/reference/chef_integration.md)
  : Chef Integration
- [`formatting`](https://sims1253.github.io/pharmhand/dev/reference/formatting.md)
  : Format String Grammar
- [`layers`](https://sims1253.github.io/pharmhand/dev/reference/layers.md)
  : Layer System
- [`reporting_engine`](https://sims1253.github.io/pharmhand/dev/reference/reporting_engine.md)
  : Reporting Engine
- [`study_logic`](https://sims1253.github.io/pharmhand/dev/reference/study_logic.md)
  : Study Logic
