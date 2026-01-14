# Get started with pharmhand

pharmhand creates clinical tables and reports from ADaM datasets using
S7 classes.

## Installation

``` r
# install.packages("pak")
pak::pak("sims1253/pharmhand")
```

## Quick Start (Fastest Way)

Generate complete reports in a single call:

``` r
library(pharmhand)

# Load example ADaM datasets
library(pharmaverseadam)
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# One-line demographics report
quick_demographics_report(adsl, "demographics.docx")

# One-line safety report with multiple tables
quick_safety_report(adae, adsl, "safety.docx",
                   include_overview = TRUE,
                   include_soc = TRUE,
                   include_sae = TRUE)
```

That’s it! The functions automatically: - Accept data.frames directly
(no S7 wrapping needed) - Use sensible defaults (configurable via
[`options()`](https://rdrr.io/r/base/options.html)) - Create formatted
Word documents with multiple tables

## Core Workflow (Detailed Control)

For more control, build tables and reports step-by-step:

### Simple: Pass data.frames directly

``` r
library(pharmhand)

# Most functions accept data.frames directly
demo_table <- create_demographics_table(
  adsl,
  title = "Table 1.1: Demographics and Baseline Characteristics"
)

demo_table@flextable    # Formatted flextable
demo_table@metadata     # Analysis details
```

### Advanced: Wrap ADaM data for population filtering

``` r
# For population filtering, wrap in ADaMData
adam_data <- ADaMData(
  data = adsl,
  domain = "ADSL",
  population = "SAF",
  trt_var = "TRT01A"
)

adam_data@filtered_data  # Filtered to SAFFL == "Y"
adam_data@trt_n          # Treatment group N's

# Pass to table functions
demo_table <- create_demographics_table(adam_data)
```

### Create plots

``` r
# Example data for Kaplan-Meier plots
adtte <- pharmaverseadam::adtte

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

### G-BA Module 4 Compliance

``` r
# Create a Module 4 template table
module4_table <- create_hta_module4_table()

# Apply G-BA formatting and validate
module4_table <- to_gba_template(module4_table)
check_gba_compliance(module4_table, strict = FALSE)
```

## Configuration

Customize package defaults for your study:

``` r
# Set once at the start of your script
options(
  pharmhand.trt_var = "ARM",         # Treatment variable
  pharmhand.autofit = TRUE,          # Auto-fit columns
  pharmhand.conf_level = 0.95,       # Confidence level
  pharmhand.n_top = 20               # Top N for common AEs
)

# All functions now use your preferences
create_demographics_table(adsl)        # Uses ARM
create_ae_summary_table(adae, type = "common")  # Shows top 20
meta_analysis(yi, sei)                 # Uses 95% CI
```

Available options: - `pharmhand.trt_var` - Treatment variable (default:
“TRT01P”) - `pharmhand.subject_var` - Subject ID variable (default:
“USUBJID”) - `pharmhand.autofit` - Auto-fit table columns (default:
TRUE) - `pharmhand.conf_level` - Confidence level (default: 0.95) -
`pharmhand.n_top` - Top N for common tables (default: 15) -
`pharmhand.na_string` - NA display string (default: “–”)

## More examples

- [`vignette("baseline-tables")`](https://sims1253.github.io/pharmhand/dev/articles/baseline-tables.md) -
  Demographics, disposition, and baseline characteristics
- [`vignette("safety-tables")`](https://sims1253.github.io/pharmhand/dev/articles/safety-tables.md) -
  Safety tables and adverse event analysis
- [`vignette("efficacy-tables")`](https://sims1253.github.io/pharmhand/dev/articles/efficacy-tables.md) -
  Efficacy endpoints and time-to-event analysis
- [`vignette("s7-architecture")`](https://sims1253.github.io/pharmhand/dev/articles/s7-architecture.md) -
  S7 class system and extension points
