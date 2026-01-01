# Understanding the S7 Class Architecture

## Introduction

pharmhand uses S7 classes for clinical data analysis and reporting. This
vignette documents the class system.

## Why S7?

S7 provides property validation, computed properties, and multiple
dispatch.

## Core Classes

The pharmhand S7 architecture:

    ClinicalContent (abstract)
    ├── ClinicalTable
    ├── ClinicalPlot
    └── ReportSection
        ├── SOCPTSection
        ├── PopulationSection
        ├── SubgroupSection
        └── HTASection

    ADaMData
    ├── AnalysisResults
    ├── StudyResult
    ├── ClinicalReport
    └── AnalysisMeta

    Study Design Classes
    ├── OneArmStudy
    └── TwoArmStudy

    Endpoint Classes
    ├── PrimaryEndpoint
    ├── SecondaryEndpoint
    ├── SafetyEndpoint
    └── HTAEndpoint

## ADaMData

`ADaMData` wraps ADaM datasets with metadata and computed properties.

### Construction

``` r
library(pharmhand)

# Create a simple ADaM dataset
demo_data <- data.frame(
  USUBJID = paste0("SUBJ", 1:100),
  TRT01P = sample(c("Placebo", "Active"), 100, replace = TRUE),
  AGE = rnorm(100, mean = 65, sd = 12),
  SEX = sample(c("M", "F"), 100, replace = TRUE),
  ITTFL = "Y",
  PPSFL = sample(c("Y", "N"), 100, prob = c(0.9, 0.1), replace = TRUE),
  stringsAsFactors = FALSE
)

# Create ADaMData object
adam_data <- ADaMData(
  data = demo_data,
  domain = "ADSL",
  population = "ITT",
  trt_var = "TRT01P",
  subject_var = "USUBJID"
)

# Access properties
adam_data@domain
#> [1] "ADSL"
adam_data@population
#> [1] "ITT"
```

### Computed Properties

ADaMData provides computed properties for population filtering:

``` r
# Filtered data (respects population filter)
head(adam_data@filtered_data)
#>   USUBJID  TRT01P      AGE SEX ITTFL PPSFL
#> 1   SUBJ1 Placebo 75.34504   F     Y     Y
#> 2   SUBJ2 Placebo 62.08116   M     Y     Y
#> 3   SUBJ3  Active 62.52695   M     Y     N
#> 4   SUBJ4 Placebo 65.23013   F     Y     Y
#> 5   SUBJ5 Placebo 65.35473   F     Y     Y
#> 6   SUBJ6  Active 71.59793   F     Y     Y

# Treatment group counts
trt_counts <- adam_data@trt_n
trt_counts
#> # A tibble: 2 × 2
#>   TRT01P      N
#>   <chr>   <int>
#> 1 Active     49
#> 2 Placebo    51
```

### Population Filtering

``` r
# Switch to Per Protocol Set
adam_pps <- ADaMData(
  data = demo_data,
  domain = "ADSL",
  population = "PPS"  # Uses PPSFL column
)

# Compare population sizes
nrow(adam_data@filtered_data)  # ITT: all subjects
#> [1] 100
nrow(adam_pps@filtered_data)   # PPS: only PPSFL == "Y"
#> [1] 92

# Use "ALL" to bypass population filtering
adam_all <- ADaMData(
  data = demo_data,
  domain = "ADSL",
  population = "ALL"
)
nrow(adam_all@filtered_data)  # All 100 subjects
#> [1] 100
```

## ClinicalTable

`ClinicalTable` represents formatted clinical tables.

### Construction

``` r
# Create a simple clinical table
table_data <- data.frame(
  Parameter = c("Age (years)", "Sex: Male", "Sex: Female"),
  Statistic = c("65.2 ± 11.8", "52 (52%)", "48 (48%)"),
  stringsAsFactors = FALSE
)

clinical_table <- ClinicalTable(
  data = table_data,
  type = "demographics",
  title = "Baseline Demographics"
)

# Access computed properties
clinical_table@n_rows
#> [1] 3
clinical_table@column_names
#> [1] "Parameter" "Statistic"
```

### Integration

``` r
# Perform baseline analysis
baseline_results <- calculate_baseline(adam_data, vars = c("AGE", "SEX"))

# Convert to ClinicalTable with flextable
clinical_table <- ClinicalTable(
  data = baseline_results@stats,
  flextable = flextable::flextable(baseline_results@stats),
  type = "baseline",
  title = "Baseline Characteristics"
)

# Access the flextable
class(clinical_table@flextable)
#> [1] "flextable"
```

### Formatting

Access the flextable for styling:

``` r
ft <- clinical_table@flextable
class(ft)
#> [1] "flextable"
```

## ClinicalPlot

`ClinicalPlot` wraps ggplot2 objects with metadata and export
capabilities.

### Construction

``` r
library(ggplot2)

# Create a simple scatter plot
sex_numeric <- as.numeric(factor(demo_data$SEX, levels = c("M", "F")))
p <- ggplot(demo_data, aes(x = AGE, y = sex_numeric)) +
  geom_point(alpha = 0.6) +
  labs(x = "Age (years)", y = "Sex (M=1, F=2)")

# Wrap in ClinicalPlot
clinical_plot <- ClinicalPlot(
  plot = p,
  type = "scatter",
  title = "Age Distribution by Sex",
  width = 6,
  height = 4,
  dpi = 300
)

# Access plot properties
clinical_plot@width
#> [1] 6
clinical_plot@height
#> [1] 4
clinical_plot@type
#> [1] "scatter"
```

### Survival Plots

``` r
# For survival plots, is_survival property is automatically detected
clinical_plot@is_survival  # FALSE for our scatter plot
#> [1] FALSE
```

### Export

Save plots in multiple formats:

``` r
# Save as different formats
png_path <- save_plot_as(clinical_plot, format = "png")
pdf_path <- save_plot_as(clinical_plot, format = "pdf")

# Files are created with specified dimensions and DPI
file.exists(png_path)
#> [1] TRUE
file.exists(pdf_path)
#> [1] TRUE
```

## ClinicalReport

`ClinicalReport` represents a complete report with sections.

### Construction

``` r
# Create a basic report structure
report <- ClinicalReport(
  study_id = "STUDY-001",
  study_title = "Phase III Clinical Study",
  metadata = list(
    protocol_version = "1.0",
    analysis_date = Sys.Date()
  )
)

# Add sections
baseline_section <- ReportSection(
  title = "Baseline Characteristics",
  section_type = "baseline"
)

safety_section <- ReportSection(
  title = "Safety Analysis",
  section_type = "safety"
)

# Add sections to report
report@sections <- list(
  baseline = baseline_section,
  safety = safety_section
)

report@n_sections
#> [1] 2
```

### Section Types

``` r
# SOC-PT section for adverse events
ae_section <- SOCPTSection(
  title = "Adverse Events by System Organ Class",
  soc_var = "AEBODSYS",
  pt_var = "AEDECOD",
  group_var = "TRT01P"
)

# Population analysis section
pop_section <- PopulationSection(
  title = "Population Analysis",
  pop_var = "FASFL",
  group_var = "TRT01P"
)

# Subgroup analysis section
subgroup_section <- SubgroupSection(
  title = "Subgroup Analysis",
  subgroup_var = "AGEGR1",
  group_var = "TRT01P"
)
```

## Study Design Classes

pharmhand provides specialized classes for different study designs.

### OneArmStudy

``` r
one_arm_data <- data.frame(
  USUBJID = paste0("SUBJ", 1:50),
  TRT01P = "Active",  # Single treatment
  AGE = rnorm(50, mean = 60, sd = 10),
  RESPONSE = sample(c("Y", "N"), 50, prob = c(0.3, 0.7), replace = TRUE),
  AEBODSYS = sample(c("GI", "CNS", "Respiratory"), 50, replace = TRUE),
  AEDECOD = sample(c("Nausea", "Headache", "Cough"), 50, replace = TRUE),
  stringsAsFactors = FALSE
)

one_arm_study <- OneArmStudy(
  data = one_arm_data,
  study_id = "ONE-ARM-001",
  study_title = "Single-Arm Oncology Study"
)

# Analyze the study
one_arm_study <- analyze_study(one_arm_study)

# Results are stored in the study object
names(one_arm_study@results)
#> [1] "baseline" "safety"
```

### TwoArmStudy

``` r
two_arm_data <- data.frame(
  USUBJID = paste0("SUBJ", 1:100),
  TRT01P = sample(c("Placebo", "Active"), 100, replace = TRUE),
  AGE = rnorm(100, mean = 65, sd = 12),
  RESPONSE = sample(c("Y", "N"), 100, prob = c(0.4, 0.6), replace = TRUE),
  AEBODSYS = sample(c("GI", "CNS", "Respiratory"), 100, replace = TRUE),
  AEDECOD = sample(c("Nausea", "Headache", "Cough"), 100, replace = TRUE),
  stringsAsFactors = FALSE
)

two_arm_study <- TwoArmStudy(
  data = two_arm_data,
  group_var = "TRT01P",
  study_id = "TWO-ARM-001",
  study_title = "Randomized Controlled Trial"
)

# Analyze the comparative study
two_arm_study <- analyze_study(two_arm_study)
names(two_arm_study@results)
#> [1] "baseline" "safety"
```

## Generics and Methods

pharmhand uses S7’s multiple dispatch system.

### Generics

``` r
# analyze() - performs analysis based on input type
baseline_results <- analyze(adam_data)  # ADaMData method

# as_flextable() - converts to flextable format
ft <- as_flextable(baseline_results)  # AnalysisResults method

# as_gt() - converts to gt format
gt_tbl <- as_gt(baseline_results)     # AnalysisResults method

# to_word() - converts to Word-compatible format
word_obj <- to_word(clinical_table)   # ClinicalTable method
```

### Dispatch

``` r
# Single dispatch on first argument
analyze(adam_data)           # Uses ADaMData method
#> <pharmhand::AnalysisResults>
#>  @ stats    : tibble [2 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ TRT01P: chr [1:2] "Active" "Placebo"
#>  $ N     : int [1:2] 49 51
#>  @ type     : chr "baseline"
#>  @ groupings: list()
#>  @ metadata : list()
analyze_study(one_arm_study) # Uses OneArmStudy method
#> <pharmhand::OneArmStudy>
#>  @ data       :'data.frame': 50 obs. of  6 variables:
#>  .. $ USUBJID : chr  "SUBJ1" "SUBJ2" "SUBJ3" "SUBJ4" ...
#>  .. $ TRT01P  : chr  "Active" "Active" "Active" "Active" ...
#>  .. $ AGE     : num  51.7 55 48.1 52.5 74.6 ...
#>  .. $ RESPONSE: chr  "N" "N" "N" "N" ...
#>  .. $ AEBODSYS: chr  "GI" "CNS" "GI" "CNS" ...
#>  .. $ AEDECOD : chr  "Nausea" "Nausea" "Nausea" "Headache" ...
#>  @ study_id   : chr "ONE-ARM-001"
#>  @ study_title: chr "Single-Arm Oncology Study"
#>  @ results    :List of 2
#>  .. $ baseline: <pharmhand::AnalysisResults>
#>  ..  ..@ stats    : tibble [1 × 8] (S3: tbl_df/tbl/data.frame)
#>  $ variable: chr "AGE"
#>  $ TRT01P  : chr "Active"
#>  $ n       : int 50
#>  $ mean    : num 60.4
#>  $ sd      : num 10.6
#>  $ median  : num 60
#>  $ min     : num 38.2
#>  $ max     : num 81.6
#>  ..  ..@ type     : chr "baseline"
#>  ..  ..@ groupings: list()
#>  ..  ..@ metadata :List of 1
#>  .. .. .. $ categorical: tibble [8 × 4] (S3: tbl_df/tbl/data.frame)
#>  .. .. ..  ..$ variable: chr [1:8] "AEBODSYS" "AEBODSYS" "AEBODSYS" "AEDECOD" ...
#>  .. .. ..  ..$ TRT01P  : chr [1:8] "Active" "Active" "Active" "Active" ...
#>  .. .. ..  ..$ n       : int [1:8] 15 17 18 18 13 19 35 15
#>  .. .. ..  ..$ label   : chr [1:8] "CNS (n=15, 30%)" "GI (n=17, 34%)" "Respiratory (n=18, 36%)" "Cough (n=18, 36%)" ...
#>  .. $ safety  : <pharmhand::AnalysisResults>
#>  ..  ..@ stats    : tibble [12 × 8] (S3: tbl_df/tbl/data.frame)
#>  $ AEBODSYS: chr [1:12] "CNS" "CNS" "CNS" "CNS" ...
#>  $ TRT01P  : chr [1:12] "Active" "Active" "Active" "Active" ...
#>  $ n       : int [1:12] 15 8 3 4 17 4 5 8 18 6 ...
#>  $ N_tot   : int [1:12] 50 50 50 50 50 50 50 50 50 50 ...
#>  $ pct     : num [1:12] 30 16 6 8 34 8 10 16 36 12 ...
#>  $ level   : chr [1:12] "SOC" "PT" "PT" "PT" ...
#>  $ label   : chr [1:12] "CNS" "Cough" "Headache" "Nausea" ...
#>  $ AEDECOD : chr [1:12] NA "Cough" "Headache" "Nausea" ...
#>  ..  ..@ type     : chr "safety_ae"
#>  ..  ..@ groupings: list()
#>  ..  ..@ metadata : list()
#>  @ metadata   : list()

# Multiple dispatch - method selected based on content type
doc <- officer::read_docx()

# Create simple objects for demonstration
simple_df <- data.frame(Var = c("A", "B"), Value = c("1.2", "3.4"))
table_obj <- ClinicalTable(
  data = simple_df,
  flextable = flextable::flextable(simple_df),
  type = "simple"
)

plot_obj <- ClinicalPlot(
  plot = ggplot2::ggplot(data.frame(x = 1:5, y = 1:5),
                         ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(),
  title = "Demo Plot"
)

# Different methods based on second argument type
doc <- add_to_docx(doc, table_obj)  # Uses ClinicalTable method
doc <- add_to_docx(doc, plot_obj)   # Uses ClinicalPlot method
```

### Content Management

``` r
# Add content to StudyResult
study_result <- StudyResult(
  study_id = "TEST-001",
  study_title = "Test Study"
)

# Create a fresh clinical table for this example
table_data <- data.frame(
  Variable = c("Age", "Sex"),
  Statistic = c("Mean (SD)", "n (%)"),
  Value = c("60.5 (10.2)", "50 (50%)")
)
clinical_table <- ClinicalTable(
  data = table_data,
  type = "demographics",
  title = "Demographics"
)

# Create a fresh clinical plot for this example
clinical_plot <- ClinicalPlot(
  plot = ggplot2::ggplot(data.frame(x = 1:10, y = 1:10),
                          ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(),
  title = "Sample Plot"
)

# Add table with automatic naming
study_result <- add_table(study_result, clinical_table, name = "demographics")

# Add plot with automatic naming
study_result <- add_plot(study_result, clinical_plot, name = "efficacy_plot")

# Content is automatically tracked
study_result@table_names
#> [1] "demographics"
study_result@plot_names
#> [1] "efficacy_plot"
```

## Endpoint Classes

pharmhand provides endpoint classes for different analyses.

### Primary and Secondary Endpoints

``` r
# Primary endpoint
primary_endpoint <- PrimaryEndpoint(
  name = "Overall Survival",
  variable = "OS",
  type = "time_to_event",
  description = "Time from randomization to death from any cause",
  hypothesis = "superiority",
  alpha = 0.05
)

# Secondary endpoint
secondary_endpoint <- SecondaryEndpoint(
  name = "Progression-Free Survival",
  variable = "PFS",
  type = "time_to_event",
  description = "Time from randomization to disease progression or death",
  priority = 1,
  exploratory = FALSE
)
```

### Safety Endpoints

``` r
safety_endpoint <- SafetyEndpoint(
  name = "Serious Adverse Events",
  variable = "AESER",
  type = "adverse_event",
  description = "Incidence of serious adverse events",
  severity = "serious",
  relatedness = "related"
)
```

### HTA Endpoints

``` r
hta_endpoint <- HTAEndpoint(
  name = "Response Rate",
  variable = "AVALC",
  type = "binary",
  description = "Objective response rate per RECIST v1.1",
  strata = c("AGEGR1", "SEX"),
  criteria = list(
    measurable_disease = "MEASUR == 'Y'",
    baseline_scan = "BLSCAN == 'Y'"
  )
)
```

## AnalysisMeta

`AnalysisMeta` provides audit trail capabilities.

### Construction

``` r
# Manual creation
meta <- AnalysisMeta(
  source_vars = c("AGE", "SEX", "HEIGHT"),
  filters = list(
    population = "ITTFL == 'Y'",
    baseline = "ABLFL == 'Y'"
  ),
  row_id = "demographics_age_mean",
  derivation = "Mean age calculated using available baseline measurements",
  timestamp = Sys.time(),
  package_version = as.character(packageVersion("pharmhand")),
  r_version = paste0(R.version$major, ".", R.version$minor)
)

# Helper function for common cases
meta_auto <- create_analysis_meta(
  source_vars = c("AGE", "SEX"),
  derivation = "Mean ± SD for continuous, n (%) for categorical",
  row_id = "baseline_summary"
)
```

## Extending pharmhand

The S7 architecture makes it easy to extend pharmhand.

### Custom Classes

``` r
# Create a custom endpoint type
custom_endpoint <- S7::new_class(
  "CustomEndpoint",
  package = "myextension",
  parent = PrimaryEndpoint,
  properties = list(
    custom_param = S7::new_property(
      S7::class_numeric,
      validator = function(value) {
        if (any(value < 0)) return("custom_param must be non-negative")
        NULL
      }
    )
  )
)
```

### Custom Methods

``` r
# Add a method to an existing generic
S7::method(analyze, custom_endpoint) <- function(x, data, ...) {
  # Custom analysis logic here
  results <- list(
    estimate = mean(data[[x@variable]]),
    custom_value = x@custom_param
  )

  AnalysisResults(
    stats = as.data.frame(results),
    type = "custom_analysis",
    metadata = list(endpoint = x@name)
  )
}
```

### Integration

``` r
# Add a custom variable for demonstration
demo_data$CUSTOM_VAR <- rnorm(nrow(demo_data), mean = 100, sd = 15)

# Custom endpoint works with standard generics
custom_endpoint_obj <- custom_endpoint(
  name = "Custom Analysis",
  variable = "CUSTOM_VAR",
  type = "custom",
  custom_param = 42
)

# Analysis results can be converted to standard formats
custom_results <- analyze(custom_endpoint_obj, data = demo_data)
ft <- as_flextable(custom_results)
```

## Property Validation

### Validation

``` r
validated_class <- S7::new_class(
  "ValidatedClass",
  properties = list(
    critical_value = S7::new_property(
      S7::class_numeric,
      validator = function(value) {
        if (length(value) != 1) return("Must be single value")
        if (value < 0 || value > 1) return("Must be between 0 and 1")
        NULL
      }
    )
  )
)
```

### Computed Properties

``` r
smart_class <- S7::new_class(
  "SmartClass",
  properties = list(
    data = S7::new_property(S7::class_data.frame),
    n_complete = S7::new_property(
      S7::class_integer,
      getter = function(self) sum(complete.cases(self@data))
    )
  )
)
```
