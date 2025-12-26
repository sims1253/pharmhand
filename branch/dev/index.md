# pharmhand

pharmhand creates clinical study tables and reports from ADaM datasets.
It wraps [officer](https://davidgohel.github.io/officer/) and
[flextable](https://davidgohel.github.io/flextable/) with S7 classes
designed for clinical reporting workflows, with a focus on HTA/AMNOG
dossier requirements.

## Installation

``` r
# install.packages("pak")
pak::pak("sims1253/pharmhand")
```

## Quick Start

``` r
library(pharmhand)

# Wrap ADaM data with automatic population filtering
adam_data <- ADaMData(
  data = adsl,
  domain = "ADSL",

  population = "SAF",
  trt_var = "TRT01A"
)

# Access filtered data and treatment counts directly
adam_data@filtered_data  # Auto-filtered to SAFFL == "Y"
adam_data@trt_n          # Treatment group N's computed automatically
```

## Key Features

### Efficacy Tables

``` r
# Time-to-event summary with median, HR, and landmarks
tte_table <- create_tte_summary_table(
  adtte,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01A",
  landmarks = c(6, 12)  # Survival rates at 6 and 12 months
)

# Responder analysis with CI, OR, RR, RD
responder_table <- create_responder_table(
  adrs,
  response_var = "AVALC",
  trt_var = "TRT01A",
  effect_measure = "OR"
)

# Subgroup analysis table for Word export
subgroup_table <- create_subgroup_table(
  adtte,
  subgroups = list(
    "Age" = list(var = "AGEGR1"),
    "Sex" = list(var = "SEX")
  ),
  effect_type = "hr"
)
```

### Safety Tables

All safety tables through a unified interface:

``` r
# AE Overview
create_ae_table(adae, adsl, type = "overview")

# AEs by SOC
create_ae_table(adae, adsl, type = "soc")

# Most common AEs (top 15)
create_ae_table(adae, adsl, type = "common", n_top = 15)

# SAEs, discontinuations, deaths
create_ae_table(adae, adsl, type = "sae")
create_ae_table(adae, adsl, type = "discontinuation")
create_ae_table(adae, adsl, type = "deaths")

# By severity or relationship
create_ae_table(adae, adsl, type = "severity")
create_ae_table(adae, adsl, type = "relationship")
```

### Plots

``` r
# Kaplan-Meier with risk table
km_plot <- create_km_plot(
  data = adtte,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01A",
  risk_table = TRUE,
  show_median = TRUE,
  landmarks = c(12, 24)
)

# Forest plot for subgroup analysis
forest_plot <- create_forest_plot(
  data = adtte,
  subgroups = list(
    "Age <65" = list(var = "AGEGR1", level = "<65"),
    "Age >=65" = list(var = "AGEGR1", level = ">=65")
  ),
  effect_type = "hr"
)
```

### Report Generation

``` r
# Build a complete report
report <- ClinicalReport(
  study_id = "STUDY-001",
  study_title = "Phase III Clinical Trial"
)

section <- ReportSection(
  title = "Efficacy Results",
  section_type = "efficacy"
)

section <- add_content(section, tte_table)
section <- add_content(section, km_plot)
report <- add_section(report, section)

# Export to Word
generate_word(report, "study_report.docx")
```

## S7 Classes

pharmhand uses S7 classes to structure clinical content:

- `ADaMData` - ADaM dataset with population filters and computed
  properties (`filtered_data`, `trt_n`)
- `ClinicalTable` - Table with flextable formatting
- `ClinicalPlot` - Plot (ggplot2/ggsurvplot) with export settings
- `ClinicalReport` - Report containing sections with tables and plots
- `StudyResult` - Container for tables and plots from a study

## Related Packages

pharmhand builds on:

- [officer](https://davidgohel.github.io/officer/) - Word document
  manipulation
- [flextable](https://davidgohel.github.io/flextable/) - Table
  formatting
- [admiral](https://pharmaverse.github.io/admiral/) - ADaM dataset
  creation
- [survival](https://cran.r-project.org/package=survival) - Survival
  analysis
- [chef](https://github.com/hta-pharma/chef) - HTA/AMNOG analysis
  pipelines (optional integration)
