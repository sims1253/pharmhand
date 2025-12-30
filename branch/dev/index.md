# pharmhand

pharmhand creates clinical study tables and reports from ADaM datasets.
It uses S7 classes to structure clinical data and outputs formatted
tables and plots for Word documents.

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

## Functions

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

## Classes

- `ADaMData` - ADaM dataset wrapper with population filtering
- `ClinicalTable` - Table with formatting
- `ClinicalPlot` - Plot with export settings  
- `ClinicalReport` - Report with sections
- `StudyResult` - Container for results

## Related Packages

- [officer](https://davidgohel.github.io/officer/) - Word documents
- [flextable](https://davidgohel.github.io/flextable/) - Table
  formatting
- [admiral](https://pharmaverse.github.io/admiral/) - ADaM datasets
- [survival](https://cran.r-project.org/package=survival) - Survival
  analysis
- [chef](https://hta-pharma.github.io/chef/) - HTA analyses (optional)

## Learn More

- [Get
  Started](https://sims1253.github.io/pharmhand/articles/pharmhand.html) -
  Quick introduction to pharmhand
- [Safety
  Tables](https://sims1253.github.io/pharmhand/articles/safety-tables.html) -
  Creating adverse event tables
- [Efficacy
  Tables](https://sims1253.github.io/pharmhand/articles/efficacy-tables.html) -
  Time-to-event and responder analyses
- [S7
  Architecture](https://sims1253.github.io/pharmhand/articles/s7-architecture.html) -
  Understanding the class system

Browse the [full documentation](https://sims1253.github.io/pharmhand/).
