#' S7 Classes for Clinical Study Reports
#'
#' Core S7 class definitions for clinical study report generation.
#' These classes replace the previous R6 implementation with S7's
#' modern OOP system supporting multiple dispatch, properties, and
#' proper validation.
#'
#' @name S7_classes
NULL

#' ADaMData Class
#'
#' Base class for ADaM datasets with population filters.
#'
#' @param data A data frame containing the ADaM dataset
#' @param domain Character string for the ADaM domain (e.g., "ADSL", "ADAE")
#' @param population Character string for population filter (default: "FAS")
#' @param subject_var Character string for subject ID variable (default: "USUBJID")
#' @param trt_var Character string for treatment variable (default: "TRT01P")
#' @param metadata List of additional metadata
#'
#' @return An ADaMData object
#' @export ADaMData
ADaMData <- S7::new_class(
  "ADaMData",
  package = "FunctionReport",
  properties = list(
    data = S7::new_property(
      S7::class_data.frame,
      default = data.frame(),
      validator = function(value) {
        admiraldev::assert_data_frame(value)
        NULL
      }
    ),
    domain = S7::new_property(
      S7::class_character,
      default = "",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    population = S7::new_property(
      S7::class_character,
      default = "FAS",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    subject_var = S7::new_property(
      S7::class_character,
      default = "USUBJID",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    trt_var = S7::new_property(
      S7::class_character,
      default = "TRT01P",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' AnalysisResults Class
#'
#' Container for pre-calculated statistics.
#'
#' @param stats A data frame containing the statistical results
#' @param type Character string for result type
#' @param groupings List of grouping variables used
#' @param metadata List of additional metadata
#'
#' @return An AnalysisResults object
#' @export AnalysisResults
AnalysisResults <- S7::new_class(
  "AnalysisResults",
  package = "FunctionReport",
  properties = list(
    stats = S7::new_property(S7::class_data.frame, default = data.frame()),
    type = S7::new_property(S7::class_character, default = ""),
    groupings = S7::new_property(S7::class_list, default = list()),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' Abstract base class for all clinical content
#'
#' @keywords internal
#' @noRd
ClinicalContent <- S7::new_class(
  "ClinicalContent",
  package = "FunctionReport",
  properties = list(
    type = S7::new_property(S7::class_character),
    title = S7::new_property(S7::class_any),
    metadata = S7::new_property(S7::class_list, default = list())
  ),
  abstract = TRUE
)

#' ClinicalTable Class
#'
#' An S7 class for representing clinical study tables with methods
#' for formatting and conversion to Word elements.
#'
#' @export
#'
#' @param data A data frame containing the table data
#' @param flextable A flextable object (optional, will be created if not provided)
#' @param type Character string for table type (e.g., "demographics", "adverse_events")
#' @param title Character string for table title
#' @param metadata List of additional metadata
#'
#' @return A ClinicalTable object
#'
#'
#' @examples
#' \dontrun{
#' table <- ClinicalTable(
#'   data = data.frame(x = 1:3, y = c("a", "b", "c")),
#'   type = "test",
#'   title = "Test Table"
#' )
#' }
ClinicalTable <- S7::new_class(
  "ClinicalTable",
  package = "FunctionReport",
  parent = ClinicalContent,
  properties = list(
    data = S7::new_property(
      class = S7::class_data.frame,
      validator = function(value) {
        admiraldev::assert_data_frame(value)
        NULL
      }
    ),
    flextable = S7::new_property(S7::class_any),
    type = S7::new_property(S7::class_character),
    title = S7::new_property(S7::class_any),
    metadata = S7::new_property(S7::class_list, default = list()),
    # Computed properties (read-only)
    n_rows = S7::new_property(
      class = S7::class_integer,
      getter = function(self) nrow(self@data)
    ),
    n_cols = S7::new_property(
      class = S7::class_integer,
      getter = function(self) ncol(self@data)
    ),
    column_names = S7::new_property(
      class = S7::class_character,
      getter = function(self) names(self@data)
    )
  )
)

#' ClinicalPlot Class
#'
#' An S7 class for representing clinical study plots (Kaplan-Meier,
#' forest plots, waterfall plots, etc.) with methods for formatting
#' and conversion to Word elements.
#'
#' @export
#'
#' @param plot A ggplot or ggsurvplot object
#' @param data A data frame containing the plot data (optional)
#' @param type Character string for plot type (e.g., "km", "forest", "waterfall")
#' @param title Character string for plot title
#' @param width Numeric value for plot width in inches
#' @param height Numeric value for plot height in inches
#' @param dpi Numeric value for plot DPI
#' @param metadata List of additional metadata
#'
#' @return A ClinicalPlot object
#'
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point()
#' plot <- ClinicalPlot(plot = p, type = "scatter", title = "MPG vs HP")
#' }
ClinicalPlot <- S7::new_class(
  "ClinicalPlot",
  package = "FunctionReport",
  parent = ClinicalContent,
  properties = list(
    plot = S7::new_property(
      class = S7::class_any,
      validator = function(value) {
        if (!inherits(value, c("ggplot", "ggsurvplot", "gg"))) {
          cli::cli_abort("Plot must be a ggplot or ggsurvplot object")
        }
        NULL
      }
    ),
    data = S7::new_property(S7::class_any),
    type = S7::new_property(S7::class_any),
    title = S7::new_property(S7::class_any),
    width = S7::new_property(
      S7::class_numeric,
      default = 6,
      validator = function(value) {
        admiraldev::assert_numeric_vector(value, len = 1)
        NULL
      }
    ),
    height = S7::new_property(
      S7::class_numeric,
      default = 4,
      validator = function(value) {
        admiraldev::assert_numeric_vector(value, len = 1)
        NULL
      }
    ),
    dpi = S7::new_property(
      S7::class_numeric,
      default = 300,
      validator = function(value) {
        admiraldev::assert_numeric_vector(value, len = 1)
        NULL
      }
    ),
    metadata = S7::new_property(S7::class_list, default = list()),
    # Computed property for survival plots
    is_survival = S7::new_property(
      class = S7::class_logical,
      getter = function(self) inherits(self@plot, "ggsurvplot")
    )
  )
)

#' StudyResult Class
#'
#' An S7 class for representing complete clinical study results
#' containing multiple tables and plots with methods for generating
#' comprehensive Word documents.
#'
#' @export
#'
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param tables Named list of ClinicalTable objects
#' @param plots Named list of ClinicalPlot objects
#' @param metadata List of additional metadata
#'
#' @return A StudyResult object
#'
#'
#' @examples
#' \dontrun{
#' result <- StudyResult(
#'   study_id = "STUDY001",
#'   study_title = "Phase III Study"
#' )
#' }
StudyResult <- S7::new_class(
  "StudyResult",
  package = "FunctionReport",
  properties = list(
    study_id = S7::new_property(S7::class_any),
    study_title = S7::new_property(S7::class_any),
    tables = S7::new_property(S7::class_list, default = list()),
    plots = S7::new_property(S7::class_list, default = list()),
    metadata = S7::new_property(S7::class_list, default = list()),
    # Computed properties
    n_tables = S7::new_property(
      class = S7::class_integer,
      getter = function(self) length(self@tables)
    ),
    n_plots = S7::new_property(
      class = S7::class_integer,
      getter = function(self) length(self@plots)
    ),
    table_names = S7::new_property(
      class = S7::class_character,
      getter = function(self) names(self@tables)
    ),
    plot_names = S7::new_property(
      class = S7::class_character,
      getter = function(self) names(self@plots)
    )
  )
)

#' ReportSection Class
#'
#' An S7 class representing a report section containing multiple
#' clinical content items (tables and plots).
#'
#' @param title Character string for section title
#' @param section_type Character string for section type
#' @param content List of ClinicalContent objects
#' @param metadata List of additional metadata
#'
#' @return A ReportSection object
#' @export
#'
#'
#' @examples
#' \dontrun{
#' section <- ReportSection(
#'   title = "Demographics",
#'   section_type = "baseline"
#' )
#' }
ReportSection <- S7::new_class(
  "ReportSection",
  package = "FunctionReport",
  properties = list(
    title = S7::new_property(S7::class_any),
    section_type = S7::new_property(S7::class_character),
    content = S7::new_property(S7::class_list, default = list()),
    metadata = S7::new_property(S7::class_list, default = list()),
    # Computed properties
    n_content = S7::new_property(
      class = S7::class_integer,
      getter = function(self) length(self@content)
    )
  )
)

#' ClinicalReport Class
#'
#' An S7 class representing a complete clinical study report
#' with multiple sections for baseline characteristics, safety,
#' and efficacy analyses.
#'
#' @export
#'
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param sections List of ReportSection objects
#' @param config ConfigurationRegistry object (optional)
#' @param metadata List of additional metadata
#'
#' @return A ClinicalReport object
#'
#'
#' @examples
#' \dontrun{
#' report <- ClinicalReport(
#'   study_id = "STUDY001",
#'   study_title = "Phase III Study"
#' )
#' }
ClinicalReport <- S7::new_class(
  "ClinicalReport",
  package = "FunctionReport",
  properties = list(
    study_id = S7::new_property(S7::class_character),
    study_title = S7::new_property(S7::class_character),
    sections = S7::new_property(S7::class_list, default = list()),
    config = S7::new_property(S7::class_any),
    metadata = S7::new_property(S7::class_list, default = list()),
    # Computed properties
    n_sections = S7::new_property(
      class = S7::class_integer,
      getter = function(self) length(self@sections)
    )
  )
)

#' OneArmStudy Class
#'
#' An S7 class for representing and analyzing single-arm clinical studies.
#' Provides methods for calculating statistics, creating tables and plots,
#' and performing hypothesis tests.
#'
#' @param data A data frame containing the study data
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param results List of analysis results
#' @param metadata List of additional metadata
#'
#' @return A OneArmStudy object
#' @export OneArmStudy
OneArmStudy <- S7::new_class(
  "OneArmStudy",
  package = "FunctionReport",
  properties = list(
    data = S7::new_property(
      S7::class_data.frame,
      default = data.frame(),
      validator = function(value) {
        admiraldev::assert_data_frame(value)
        NULL
      }
    ),
    study_id = S7::new_property(
      S7::class_any,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    study_title = S7::new_property(
      S7::class_any,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    results = S7::new_property(S7::class_list, default = list()),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' TwoArmStudy Class
#'
#' An S7 class for representing and analyzing two-arm clinical studies.
#' Provides methods for comparing treatment groups, creating tables and plots,
#' and performing hypothesis tests.
#'
#' @param data A data frame containing the study data
#' @param group_var Character string for treatment group variable
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param results List of analysis results
#' @param metadata List of additional metadata
#'
#' @return A TwoArmStudy object
#' @export TwoArmStudy
TwoArmStudy <- S7::new_class(
  "TwoArmStudy",
  package = "FunctionReport",
  properties = list(
    data = S7::new_property(
      S7::class_data.frame,
      default = data.frame(),
      validator = function(value) {
        admiraldev::assert_data_frame(value)
        NULL
      }
    ),
    group_var = S7::new_property(
      S7::class_character,
      default = "",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    study_id = S7::new_property(
      S7::class_any,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    study_title = S7::new_property(
      S7::class_any,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    results = S7::new_property(S7::class_list, default = list()),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' PrimaryEndpoint Class
#'
#' An S7 class for representing primary endpoints in clinical studies.
#'
#' @export
#'
#' @param name Character string for endpoint name
#' @param variable Character string for variable name in the dataset
#' @param type Character string for endpoint type
#' @param description Character string for endpoint description
#' @param hypothesis Character string for hypothesis type
#' @param margin Numeric value for non-inferiority margin
#' @param alpha Numeric value for significance level
#' @param metadata List of additional metadata
#'
#' @return A PrimaryEndpoint object
#'
#'
#' @examples
#' \dontrun{
#' endpoint <- PrimaryEndpoint(
#'   name = "Primary",
#'   variable = "VAL",
#'   type = "continuous"
#' )
#' }
PrimaryEndpoint <- S7::new_class(
  "PrimaryEndpoint",
  package = "FunctionReport",
  properties = list(
    name = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    variable = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    type = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    description = S7::new_property(S7::class_any),
    hypothesis = S7::new_property(
      S7::class_character,
      default = "superiority",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    margin = S7::new_property(S7::class_any),
    alpha = S7::new_property(
      S7::class_numeric,
      default = 0.05,
      validator = function(value) {
        admiraldev::assert_numeric_vector(value, len = 1)
        NULL
      }
    ),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' SecondaryEndpoint Class
#'
#' An S7 class for representing secondary endpoints in clinical studies.
#'
#' @export
#'
#' @param name Character string for endpoint name
#' @param variable Character string for variable name in the dataset
#' @param type Character string for endpoint type
#' @param description Character string for endpoint description
#' @param priority Numeric value for priority
#' @param exploratory Logical, whether this is an exploratory endpoint
#' @param metadata List of additional metadata
#'
#' @return A SecondaryEndpoint object
#'
#'
#' @examples
#' \dontrun{
#' endpoint <- SecondaryEndpoint(
#'   name = "Secondary",
#'   variable = "VAL2",
#'   type = "continuous"
#' )
#' }
SecondaryEndpoint <- S7::new_class(
  "SecondaryEndpoint",
  package = "FunctionReport",
  properties = list(
    name = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    variable = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    type = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    description = S7::new_property(S7::class_any),
    priority = S7::new_property(
      S7::class_numeric,
      default = 1,
      validator = function(value) {
        admiraldev::assert_numeric_vector(value, len = 1)
        NULL
      }
    ),
    exploratory = S7::new_property(S7::class_logical, default = FALSE),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' SafetyEndpoint Class
#'
#' An S7 class for representing safety endpoints in clinical studies.
#'
#' @export
#'
#' @param name Character string for endpoint name
#' @param variable Character string for variable name in the dataset
#' @param type Character string for endpoint type
#' @param description Character string for endpoint description
#' @param severity Character string for severity level
#' @param relatedness Character string for relatedness to treatment
#' @param metadata List of additional metadata
#'
#' @return A SafetyEndpoint object
#'
#'
#' @examples
#' \dontrun{
#' endpoint <- SafetyEndpoint(
#'   name = "Safety",
#'   variable = "AE",
#'   type = "adverse_event"
#' )
#' }
SafetyEndpoint <- S7::new_class(
  "SafetyEndpoint",
  package = "FunctionReport",
  properties = list(
    name = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    variable = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    type = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    description = S7::new_property(S7::class_any),
    severity = S7::new_property(S7::class_any),
    relatedness = S7::new_property(S7::class_any),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' PerformanceReport Class
#'
#' An S7 class for representing performance benchmarking reports.
#'
#' @export
#'
#' @param title Character string for report title
#' @param author Character string for report author
#' @param description Character string for report description
#' @param benchmarks List of benchmark results
#' @param metadata List of additional metadata
#'
#' @return A PerformanceReport object
#'
#'
#' @examples
#' \dontrun{
#' report <- PerformanceReport(
#'   title = "Performance Report",
#'   author = "R User"
#' )
#' }
PerformanceReport <- S7::new_class(
  "PerformanceReport",
  package = "FunctionReport",
  properties = list(
    title = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    author = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    description = S7::new_property(S7::class_any),
    benchmarks = S7::new_property(S7::class_list, default = list()),
    metadata = S7::new_property(S7::class_list, default = list())
  )
)

#' SOCPTSection Class
#'
#' An S7 class for representing SOC-PT (System Organ Class - Preferred Term)
#' sections in adverse events reports.
#'
#' @param title Character string for section title
#' @param soc_var Character string for SOC variable name
#' @param pt_var Character string for PT variable name
#' @param group_var Character string for treatment group variable
#' @param content List of ClinicalContent objects
#' @param section_type Character string for section type
#' @param metadata List of additional metadata
#'
#' @return A SOCPTSection object
#' @export
#'
#'
#' @examples
#' \dontrun{
#' section <- SOCPTSection(
#'   title = "Adverse Events",
#'   soc_var = "AEBODSYS",
#'   pt_var = "AEDECOD",
#'   group_var = "TRT"
#' )
#' }
SOCPTSection <- S7::new_class(
  "SOCPTSection",
  package = "FunctionReport",
  parent = ReportSection,
  properties = list(
    soc_var = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    pt_var = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    group_var = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    )
  )
)

#' PopulationSection Class
#'
#' An S7 class for representing population analysis sections
#' in clinical study reports.
#'
#' @param title Character string for section title
#' @param pop_var Character string for population variable name
#' @param group_var Character string for treatment group variable
#' @param content List of ClinicalContent objects
#' @param section_type Character string for section type
#' @param metadata List of additional metadata
#'
#' @return A PopulationSection object
#' @export
#'
#'
#' @examples
#' \dontrun{
#' section <- PopulationSection(
#'   title = "Population Analysis",
#'   pop_var = "FASFL",
#'   group_var = "TRT"
#' )
#' }
PopulationSection <- S7::new_class(
  "PopulationSection",
  package = "FunctionReport",
  parent = ReportSection,
  properties = list(
    pop_var = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    group_var = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    )
  )
)

#' SubgroupSection Class
#'
#' An S7 class for representing subgroup analysis sections
#' in clinical study reports.
#'
#' @param title Character string for section title
#' @param subgroup_var Character string for subgroup variable name
#' @param group_var Character string for treatment group variable
#' @param content List of ClinicalContent objects
#' @param section_type Character string for section type
#' @param metadata List of additional metadata
#'
#' @return A SubgroupSection object
#' @export
#'
#'
#' @examples
#' \dontrun{
#' section <- SubgroupSection(
#'   title = "Subgroup Analysis",
#'   subgroup_var = "AGEGR1",
#'   group_var = "TRT"
#' )
#' }
SubgroupSection <- S7::new_class(
  "SubgroupSection",
  package = "FunctionReport",
  parent = ReportSection,
  properties = list(
    subgroup_var = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    group_var = S7::new_property(
      S7::class_character,
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    )
  )
)

#' HTAEndpoint Class
#'
#' An S7 class for representing Health Technology Assessment endpoints,
#' with integration to the chef pipeline for AMNOG-style analyses.
#'
#' @export
#'
#' @param name Character string for endpoint name
#' @param variable Character string for variable name in the dataset
#' @param type Character string for endpoint type
#' @param description Character string for endpoint description
#' @param hypothesis Character string for hypothesis type
#' @param margin Numeric value for non-inferiority margin
#' @param alpha Numeric value for significance level
#' @param chef_spec List containing chef endpoint specification
#' @param strata Character vector of stratification variables
#' @param criteria List of inclusion criteria for chef pipeline
#' @param metadata List of additional metadata
#'
#' @return An HTAEndpoint object
#'
#' @examples
#' \dontrun{
#' endpoint <- HTAEndpoint(
#'   name = "Response Rate",
#'   variable = "AVALC",
#'   type = "binary",
#'   strata = c("SEX", "AGEGR1")
#' )
#' }
HTAEndpoint <- S7::new_class(
  "HTAEndpoint",
  package = "FunctionReport",
  parent = PrimaryEndpoint,
  properties = list(
    chef_spec = S7::new_property(S7::class_list, default = list()),
    strata = S7::new_property(
      S7::class_character,
      default = character(),
      validator = function(value) {
        admiraldev::assert_character_vector(value)
        NULL
      }
    ),
    criteria = S7::new_property(S7::class_list, default = list())
  )
)

#' AnalysisMeta Class
#'
#' An S7 class for tracking analysis metadata and audit trails,
#' inspired by Tplyr's tplyr_meta for regulatory traceability.
#'
#' @export
#'
#' @param source_vars Character vector of source variable names
#' @param filters List of filter expressions applied
#' @param row_id Character identifier for row-level tracing
#' @param derivation Character description of how the value was derived
#' @param timestamp POSIXct timestamp of analysis
#' @param package_version Character string of package version used
#' @param r_version Character string of R version used
#'
#' @return An AnalysisMeta object
#'
#' @examples
#' \dontrun{
#' meta <- AnalysisMeta(
#'   source_vars = c("AGE", "SEX"),
#'   derivation = "count distinct USUBJID",
#'   row_id = "demographics_age_mean"
#' )
#' }
AnalysisMeta <- S7::new_class(
  "AnalysisMeta",
  package = "FunctionReport",
  properties = list(
    source_vars = S7::new_property(S7::class_character, default = character()),
    filters = S7::new_property(S7::class_list, default = list()),
    row_id = S7::new_property(S7::class_character, default = ""),
    derivation = S7::new_property(S7::class_character, default = ""),
    timestamp = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !inherits(value, "POSIXct")) {
          "timestamp must be NULL or POSIXct"
        }
      }
    ),
    package_version = S7::new_property(S7::class_character, default = ""),
    r_version = S7::new_property(S7::class_character, default = "")
  )
)

#' Create Analysis Metadata
#'
#' Helper function to create an AnalysisMeta object with current environment info.
#'
#' @param source_vars Character vector of source variable names
#' @param filters List of filter expressions
#' @param row_id Character identifier for the row
#' @param derivation Character description of derivation
#'
#' @return An AnalysisMeta object with timestamp and version info populated
#' @export
create_analysis_meta <- function(
  source_vars = character(),
  filters = list(),
  row_id = "",
  derivation = ""
) {
  AnalysisMeta(
    source_vars = source_vars,
    filters = filters,
    row_id = row_id,
    derivation = derivation,
    timestamp = Sys.time(),
    package_version = as.character(utils::packageVersion("FunctionReport")),
    r_version = paste0(R.version$major, ".", R.version$minor)
  )
}

#' HTASection Class
#'
#' An S7 class for representing HTA-specific report sections
#' with support for AMNOG/G-BA dossier requirements.
#'
#' @param title Character string for section title
#' @param section_type Character string for section type
#' @param endpoint HTAEndpoint object for this section
#' @param comparator Character string for comparator description
#' @param population Character string for population description
#' @param content List of ClinicalContent objects
#' @param metadata List of additional metadata
#'
#' @return An HTASection object
#' @export
#'
#' @examples
#' \dontrun{
#' section <- HTASection(
#'   title = "Primary Efficacy Analysis",
#'   section_type = "efficacy",
#'   comparator = "Placebo",
#'   population = "ITT"
#' )
#' }
HTASection <- S7::new_class(
  "HTASection",
  package = "FunctionReport",
  parent = ReportSection,
  properties = list(
    endpoint = S7::new_property(S7::class_any, default = NULL),
    comparator = S7::new_property(
      S7::class_character,
      default = "",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    ),
    population = S7::new_property(
      S7::class_character,
      default = "",
      validator = function(value) {
        admiraldev::assert_character_scalar(value)
        NULL
      }
    )
  )
)
