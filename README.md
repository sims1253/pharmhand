# FunctionReport

![R](https://img.shields.io/badge/R-4.4.0%2B-blue)
![License](https://img.shields.io/badge/License-MIT-green)
![Version](https://img.shields.io/badge/Version-0.0.0.9000-orange)

> Wrapper for R to MS Office packages for creating clinical study reports with ADaM format support.

## Description

**FunctionReport** is a comprehensive R package designed for creating clinical study reports. It provides a high-level wrapper around popular R-to-MS Office packages like [`officer`](https://davidgohel.github.io/officer/) and [`flextable`](https://davidgohel.github.io/flextable/), specifically tailored for clinical trial reporting.

The package supports ADaM (Analysis Data Model) formats and includes:

- **Clinical Tables**: Demographics, adverse events, efficacy tables, and more
- **Clinical Plots**: Kaplan-Meier, forest plots, waterfall plots, spider plots
- **Statistical Analysis**: Descriptive statistics, hypothesis testing, survival analysis
- **Parallel Processing**: Built-in parallelization for large datasets
- **ADaM Helpers**: Utilities for reading and processing ADaM datasets
- **Result Classes**: R6 classes for structured result management

## Installation

### Install from CRAN (when available)

```r
install.packages("FunctionReport")
```

### Install from GitHub

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install FunctionReport
devtools::install_github("yourusername/FunctionReport")
```

### Install from Local Source

```r
# Navigate to the package directory
setwd("/path/to/Functioneer/FunctionReport")

# Install the package
devtools::install()
```

## Main Functions

| Category | Key Functions |
|----------|---------------|
| **Document Creation** | [`create_word_document()`](R/word_document.R:20), [`save_word_document()`](R/word_document.R:48) |
| **Tables** | [`create_summary_table()`](R/clinical_tables.R:35), [`create_demographics_table()`](R/clinical_tables.R:122), [`create_adverse_events_table()`](R/clinical_tables.R:273) |
| **Plots** | [`create_km_plot()`](R/clinical_plots.R:41), [`create_forest_plot()`](R/clinical_plots.R:113), [`create_waterfall_plot()`](R/clinical_plots.R:201) |
| **Statistics** | [`calculate_mean_sd()`](R/statistical_helpers.R:73), [`perform_ttest()`](R/statistical_helpers.R:350), [`perform_logrank()`](R/statistical_helpers.R:602) |
| **ADaM** | [`read_adam()`](R/adam_helpers.R:36), [`get_adsl()`](R/adam_helpers.R:108), [`filter_by_population()`](R/adam_helpers.R:248) |
| **Parallel** | [`setup_parallel()`](R/parallel_helpers.R:48), [`get_optimal_workers()`](R/parallel_helpers.R:123) |
| **Classes** | [`ClinicalTable`](R/result_classes.R:15), [`ClinicalPlot`](R/result_classes.R:137), [`StudyResult`](R/result_classes.R:333) |

## Quick Start

### Basic Example: Create a Clinical Report

```r
library(FunctionReport)

# Load ADaM data
adsl <- read_adam("data/adsl.xpt", "ADSL")
adae <- read_adam("data/adae.xpt", "ADAE")

# Create a Word document
doc <- create_word_document()

# Add title
doc <- add_title(doc, "Clinical Study Report", level = 1)

# Add demographics table
demo_table <- create_demographics_table(adsl, "TRT01P")
doc <- officer::body_add_flextable(doc, demo_table)

# Add page break
doc <- add_page_break(doc)

# Add adverse events table
ae_table <- create_adverse_events_table(adae, "AEBODSYS", "AEDECOD", "TRT01P")
doc <- officer::body_add_flextable(doc, ae_table)

# Save the document
save_word_document(doc, "clinical_report.docx")
```

### Using Result Classes

```r
library(FunctionReport)

# Create a study result object
study <- StudyResult$new(
  study_id = "STUDY-001",
  study_title = "Phase III Clinical Trial"
)

# Add a table
demo_table <- ClinicalTable$new(
  data = adsl,
  title = "Demographics",
  type = "demographics"
)
study$add_table(demo_table, "demographics")

# Add a plot
km_plot <- ClinicalPlot$new(
  plot = create_km_plot(fit, data = adtte),
  title = "Kaplan-Meier Survival Curve",
  type = "km"
)
study$add_plot(km_plot, "survival")

# Generate Word document
study$to_word("study_report.docx")
```

### Parallel Processing Example

```r
library(FunctionReport)

# Setup parallel processing
setup_parallel(workers = 4)

# Create multiple tables in parallel
tables <- create_tables_parallel(
  data = adsl,
  group_var = "TRT01P",
  vars = c("AGE", "SEX", "RACE")
)

# Create multiple plots in parallel
plots <- create_plots_parallel(
  data = adtte,
  plot_types = c("km", "forest", "waterfall")
)

# Stop parallel processing
stop_parallel()
```

## Features

### Clinical Tables

- **Summary Tables**: Comprehensive summary statistics with customizable grouping
- **Demographics Tables**: Age, sex, race, ethnicity, and baseline characteristics
- **Adverse Events Tables**: SOC/PT hierarchy with subject counts and percentages
- **Efficacy Tables**: Treatment outcomes with statistical comparisons
- **Population Tables**: Analysis set definitions and subject flow
- **Subgroup Tables**: Stratified analyses by demographic or clinical factors

### Clinical Plots

- **Kaplan-Meier Plots**: Survival curves with confidence intervals and risk tables
- **Forest Plots**: Effect sizes and confidence intervals across subgroups
- **Waterfall Plots**: Tumor size changes for oncology studies
- **Spider Plots**: Longitudinal tumor measurements
- **Distribution Plots**: Histograms, box plots, and density plots
- **Correlation Plots**: Scatter plots with regression lines

### Statistical Analysis

- **Descriptive Statistics**: Mean, SD, median, IQR, N, percentages
- **Confidence Intervals**: t-distribution and normal approximation methods
- **Hypothesis Tests**: t-test, Wilcoxon, chi-square, Fisher's exact
- **Survival Analysis**: Kaplan-Meier estimation, log-rank test
- **Effect Sizes**: Cohen's d, odds ratios, hazard ratios
- **Sample Size/Power**: Power analysis for common study designs

### ADaM Support

- **Dataset Reading**: Support for XPT, CSV, RDS, and SAS7BDAT formats
- **Dataset Validation**: Automatic validation of required ADaM variables
- **Population Filtering**: SAF, FAS, PPS, ITT population definitions
- **Subgroup Analysis**: Flexible subgroup definitions and filtering
- **Dataset Merging**: Subject-level merging of multiple ADaM datasets

### Parallel Processing

- **Multiple Backends**: Multisession, multicore, cluster, sequential
- **Automatic Worker Detection**: Optimal worker count based on CPU and memory
- **Memory Management**: Automatic garbage collection and memory optimization
- **Benchmarking Tools**: Performance comparison and bottleneck identification
- **Caching**: Result caching for expensive computations

### Result Classes

- **ClinicalTable**: Structured table representation with formatting methods
- **ClinicalPlot**: Structured plot representation with export capabilities
- **StudyResult**: Complete study result container with document generation

## Modules

### Core Modules

| Module | Description | File |
|--------|-------------|------|
| [`word_document`](R/word_document.R) | Word document creation and manipulation | `R/word_document.R` |
| [`clinical_tables`](R/clinical_tables.R) | Clinical table creation functions | `R/clinical_tables.R` |
| [`clinical_plots`](R/clinical_plots.R) | Clinical plot creation functions | `R/clinical_plots.R` |
| [`adam_helpers`](R/adam_helpers.R) | ADaM dataset utilities | `R/adam_helpers.R` |
| [`statistical_helpers`](R/statistical_helpers.R) | Statistical analysis functions | `R/statistical_helpers.R` |
| [`result_classes`](R/result_classes.R) | R6 classes for results | `R/result_classes.R` |
| [`parallel_helpers`](R/parallel_helpers.R) | Parallel processing utilities | `R/parallel_helpers.R` |

### Supporting Modules

| Module | Description | File |
|--------|-------------|------|
| [`flextable_wrapper`](R/flextable_wrapper.R) | Flextable formatting utilities | `R/flextable_wrapper.R` |
| [`plot_wrapper`](R/plot_wrapper.R) | Plot conversion utilities | `R/plot_wrapper.R` |
| [`table_themes`](R/table_themes.R) | Table styling themes | `R/table_themes.R` |
| [`utils`](R/utils.R) | General utility functions | `R/utils.R` |
| [`soc_pt_helpers`](R/soc_pt_helpers.R) | SOC/PT hierarchy helpers | `R/soc_pt_helpers.R` |

## Performance

### Parallelization

The package leverages the **futureverse** ecosystem for parallel processing:

```r
# Setup parallel backend
setup_parallel(backend = "multisession", workers = 4)

# Execute operations in parallel
results <- future_lapply(data_list, function(x) {
  create_summary_table(x, "TRT01P", c("AGE", "SEX"))
})

# Automatic parallel table creation
tables <- create_tables_parallel(data, group_var, vars)

# Automatic parallel plot creation
plots <- create_plots_parallel(data, plot_types)
```

### Optimization Features

- **Memory-Efficient Operations**: [`memory_efficient_join()`](R/utils.R), [`reduce_memory_footprint()`](R/utils.R)
- **Data Loading Optimization**: [`optimize_data_loading()`](R/utils.R), [`read_adam_parallel()`](R/adam_helpers.R)
- **Factor Level Optimization**: [`optimize_factor_levels()`](R/utils.R)
- **Caching**: [`cache_set()`](R/utils.R), [`cache_get()`](R/utils.R), [`memoize()`](R/utils.R)

### Benchmarking

```r
# Benchmark parallel vs sequential
benchmark_parallel_vs_sequential(
  fn = create_summary_table,
  data = large_dataset,
  workers = c(1, 2, 4, 8)
)

# Find performance bottlenecks
find_bottlenecks(report_generation_function)

# Generate performance report
generate_performance_report("performance_report.docx")
```

## Tests

The package includes comprehensive test coverage using **testthat**:

```r
# Run all tests
testthat::test_dir("tests/testthat")

# Run specific test file
testthat::test_file("tests/testthat/test-clinical-tables.R")

# Run tests with coverage
covr::package_coverage()
```

### Test Categories

- **Unit Tests**: Individual function testing
- **Integration Tests**: Multi-function workflow testing
- **Regression Tests**: Output consistency verification
- **Performance Tests**: Benchmark and optimization validation

## Contributing

We welcome contributions to FunctionReport! Please follow these guidelines:

### Development Setup

```bash
# Clone the repository
git clone https://github.com/yourusername/FunctionReport.git
cd FunctionReport

# Install development dependencies
devtools::install_dev_deps()

# Run linter
lintr::lint_dir("R")

# Run tests
devtools::test()
```

### Code Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use `styler` for automatic formatting:
  ```r
  styler::style_pkg()
  ```
- Run `lintr` before submitting:
  ```r
  lintr::lint_package()
  ```

### Submitting Changes

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Documentation

- Use roxygen2 for function documentation
- Include examples in `@examples` blocks
- Run `devtools::document()` to update documentation

## License

This package is licensed under the **MIT License**.

```
MIT License

Copyright (c) 2024 FunctionReport Contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Acknowledgments

- Built on top of excellent packages: [`officer`](https://davidgohel.github.io/officer/), [`flextable`](https://davidgohel.github.io/flextable/), [`ggplot2`](https://ggplot2.tidyverse.org/)
- Parallel processing powered by the [futureverse](https://futureverse.org/)
- Statistical functions based on standard R packages: [`survival`](https://cran.r-project.org/package=survival), [`survminer`](https://cran.r-project.org/package=survminer)

## Contact

For questions, issues, or suggestions, please:

- Open an issue on [GitHub](https://github.com/yourusername/FunctionReport/issues)
- Contact: first.last@example.com

---

**FunctionReport** - Streamlining clinical study report generation in R.