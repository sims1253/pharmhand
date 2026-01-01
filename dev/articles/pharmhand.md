# Get started with pharmhand

pharmhand creates clinical tables and reports from ADaM datasets using
S7 classes.

## Installation

``` r
# install.packages("pak")
pak::pak("sims1253/pharmhand")
```

## Core workflow

Wrap ADaM datasets in `ADaMData` objects, create tables and plots, then
assemble reports.

### Wrap ADaM data

``` r
library(pharmhand)

adam_data <- ADaMData(
  data = adsl,
  domain = "ADSL",
  population = "SAF",
  trt_var = "TRT01A"
)

adam_data@filtered_data  # Filtered to SAFFL == "Y"
adam_data@trt_n          # Treatment group N's
```

### Create tables

``` r
demo_table <- create_demographics_table(
  adsl_data = adam_data,
  title = "Table 1.1: Demographics and Baseline Characteristics"
)

demo_table@flextable    # Formatted flextable
demo_table@metadata     # Analysis details
```

### Create plots

``` r
km_plot <- create_km_plot(
  data = adtte,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01P",
  title = "Figure 1: Kaplan-Meier Plot of Overall Survival",
  risk_table = TRUE,
  show_median = TRUE
)

km_plot@plot            # ggplot2/ggsurvplot object
```

### Build reports

``` r
report <- ClinicalReport(
  study_id = "STUDY-001",
  study_title = "Phase III Clinical Trial"
)

efficacy_section <- ReportSection(
  title = "Efficacy Results",
  section_type = "efficacy"
)

efficacy_section <- add_content(efficacy_section, demo_table)
efficacy_section <- add_content(efficacy_section, km_plot)
report <- add_section(report, efficacy_section)

generate_word(report, "clinical_study_report.docx")
```

## More examples

- [`vignette("baseline-tables")`](https://sims1253.github.io/pharmhand/dev/articles/baseline-tables.md) -
  Demographics, disposition, and baseline characteristics
- [`vignette("safety-tables")`](https://sims1253.github.io/pharmhand/dev/articles/safety-tables.md) -
  Safety tables and adverse event analysis
- [`vignette("efficacy-tables")`](https://sims1253.github.io/pharmhand/dev/articles/efficacy-tables.md) -
  Efficacy endpoints and time-to-event analysis
- [`vignette("s7-architecture")`](https://sims1253.github.io/pharmhand/dev/articles/s7-architecture.md) -
  S7 class system and extension points
