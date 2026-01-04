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
#' @param subject_var Character string for subject ID variable
#'   (default: "USUBJID")
#' @param trt_var Character string for treatment variable (default: "TRT01P")
#' @param metadata List of additional metadata
#'
#' @usage ADaMData(
#'   data = structure(list(), names = character(0),
#'     row.names = integer(0), class = "data.frame"),
#'   domain = "",
#'   population = "FAS",
#'   subject_var = "USUBJID",
#'   trt_var = "TRT01P",
#'   metadata = list()
#' )
#'
#' @return An ADaMData object
#' @export ADaMData
ADaMData <- S7::new_class(
	"ADaMData",
	package = "pharmhand",
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
		metadata = S7::new_property(S7::class_list, default = list()),
		# Computed property: filtered data respecting population
		filtered_data = S7::new_property(
			class = S7::class_data.frame,
			getter = function(self) {
				df <- self@data
				if (self@population != "ALL" && nrow(df) > 0) {
					pop_fl <- paste0(self@population, "FL")
					if (pop_fl %in% names(df)) {
						df <- df[df[[pop_fl]] == "Y", , drop = FALSE]
					}
				}
				df
			}
		),
		# Computed property: treatment counts from filtered data
		trt_n = S7::new_property(
			class = S7::class_data.frame,
			getter = function(self) {
				df <- self@filtered_data
				if (nrow(df) == 0) {
					return(data.frame())
				}
				trt_var <- self@trt_var
				subject_var <- self@subject_var
				df |>
					dplyr::group_by(dplyr::across(dplyr::all_of(trt_var))) |>
					dplyr::summarise(
						N = dplyr::n_distinct(.data[[subject_var]]),
						.groups = "drop"
					)
			}
		)
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
#' @usage AnalysisResults(
#'   stats = structure(list(), names = character(0),
#'     row.names = integer(0), class = "data.frame"),
#'   type = "",
#'   groupings = list(),
#'   metadata = list()
#' )
#'
#' @return An AnalysisResults object
#' @export AnalysisResults
AnalysisResults <- S7::new_class(
	"AnalysisResults",
	package = "pharmhand",
	properties = list(
		stats = S7::new_property(S7::class_data.frame, default = data.frame()),
		type = S7::new_property(S7::class_character, default = ""),
		groupings = S7::new_property(S7::class_list, default = list()),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

# =============================================================================
# Statistical Result Classes
# =============================================================================

#' StatResult Class (Abstract Base)
#'
#' Abstract base class for statistical analysis results. Provides common
#' properties for effect estimates, confidence intervals, and p-values.
#'
#' @param estimate Numeric effect estimate
#' @param ci Numeric vector of length 2: c(lower, upper)
#'   confidence interval bounds
#' @param ci_level Numeric confidence level (default: 0.95)
#' @param p_value Numeric p-value (can be NA)
#' @param method Character string describing the statistical method used
#' @param n Integer sample size or number of studies
#' @param metadata List of additional metadata
#'
#' @return A StatResult object
#' @keywords internal
StatResult <- S7::new_class(
	"StatResult",
	package = "pharmhand",
	properties = list(
		estimate = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (length(value) != 1) {
					return("estimate must be a single numeric value")
				}
				NULL
			}
		),
		ci = S7::new_property(
			S7::class_numeric,
			validator = function(value) {
				if (length(value) != 2) {
					return("ci must be a numeric vector of length 2: c(lower, upper)")
				}
				# Allow NA bounds, only check order when both are non-NA
				if (!is.na(value[1]) && !is.na(value[2]) && value[1] > value[2]) {
					return("ci lower bound must be <= upper bound")
				}
				NULL
			}
		),
		ci_level = S7::new_property(
			S7::class_numeric,
			default = 0.95,
			validator = function(value) {
				if (length(value) != 1 || value <= 0 || value >= 1) {
					return("ci_level must be a single value between 0 and 1")
				}
				NULL
			}
		),
		p_value = S7::new_property(S7::class_any, default = NA_real_),
		method = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		n = S7::new_property(S7::class_any, default = NA_integer_),
		metadata = S7::new_property(S7::class_list, default = list())
	),
	abstract = TRUE
)

#' ComparisonResult Class
#'
#' An S7 class for representing results of a single treatment comparison
#' (e.g., hazard ratio, risk difference, odds ratio).
#'
#' @export
#'
#' @param estimate Numeric effect estimate
#' @param ci Numeric vector c(lower, upper)
#' @param ci_level Numeric confidence level
#' @param p_value Numeric p-value
#' @param method Character string for statistical method
#' @param n Integer sample size
#' @param effect_measure Character string: "hr", "or", "rr", "rd", "md", "smd"
#' @param treatment Character string for treatment arm name
#' @param control Character string for control arm name
#' @param metadata List of additional metadata
#'
#' @return A ComparisonResult object
#'
#' @examples
#' \dontrun{
#' result <- ComparisonResult(
#'   estimate = 0.75,
#'   ci = c(0.60, 0.93),
#'   p_value = 0.008,
#'   effect_measure = "hr",
#'   treatment = "Drug A",
#'   control = "Placebo",
#'   method = "Cox proportional hazards"
#' )
#' }
ComparisonResult <- S7::new_class(
	"ComparisonResult",
	package = "pharmhand",
	parent = StatResult,
	properties = list(
		effect_measure = S7::new_property(
			S7::class_character,
			default = "hr",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_measures <- c("hr", "or", "rr", "rd", "md", "smd", "irr")
				if (!value %in% valid_measures) {
					return(sprintf(
						"effect_measure must be one of: %s",
						paste(valid_measures, collapse = ", ")
					))
				}
				NULL
			}
		),
		treatment = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		control = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		)
	)
)

#' MetaResult Class
#'
#' An S7 class for representing meta-analysis results including pooled
#' estimates, heterogeneity statistics, and study weights.
#'
#' @export
#'
#' @param estimate Numeric pooled effect estimate
#' @param ci Numeric vector c(lower, upper)
#' @param ci_level Numeric confidence level
#' @param p_value Numeric p-value
#' @param method Character string for statistical method
#' @param n Integer number of studies
#' @param model Character string: "fixed" or "random"
#' @param effect_measure Character string: "hr", "or", "rr", "rd", "md", "smd"
#' @param heterogeneity List with Q, I2, tau2, H2 statistics
#' @param weights Numeric vector of study weights
#' @param prediction_interval Numeric vector c(lower, upper)
#'   for prediction interval
#' @param study_results List of individual study ComparisonResult objects
#' @param metadata List of additional metadata
#'
#' @return A MetaResult object
#'
#' @examples
#' result <- MetaResult(
#'   estimate = 0.80,
#'   ci = c(0.70, 0.91),
#'   ci_level = 0.95,
#'   p_value = 0.001,
#'   n = 5L,
#'   model = "random",
#'   effect_measure = "hr",
#'   heterogeneity = list(
#'     Q = 15.2, Q_df = 4L, Q_pvalue = 0.004,
#'     I2 = 73.7, H2 = 3.8, tau2 = 0.025, tau = 0.158
#'   ),
#'   method = "REML with Knapp-Hartung adjustment"
#' )
#' result@estimate
#' result@heterogeneity$I2
MetaResult <- S7::new_class(
	"MetaResult",
	package = "pharmhand",
	parent = StatResult,
	properties = list(
		model = S7::new_property(
			S7::class_character,
			default = "random",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				if (!value %in% c("fixed", "random")) {
					return("model must be 'fixed' or 'random'")
				}
				NULL
			}
		),
		effect_measure = S7::new_property(
			S7::class_character,
			default = "hr",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_measures <- c("hr", "or", "rr", "rd", "md", "smd", "irr")
				if (!value %in% valid_measures) {
					return(sprintf(
						"effect_measure must be one of: %s",
						paste(valid_measures, collapse = ", ")
					))
				}
				NULL
			}
		),
		heterogeneity = S7::new_property(
			S7::class_list,
			default = list(
				Q = NA_real_,
				I2 = NA_real_,
				tau2 = NA_real_,
				H2 = NA_real_
			)
		),
		weights = S7::new_property(S7::class_any, default = NULL),
		prediction_interval = S7::new_property(S7::class_any, default = NULL),
		study_results = S7::new_property(S7::class_list, default = list())
	)
)

#' EvidenceGrade Class
#'
#' An S7 class for representing IQWiG evidence grading results
#' (Beleg/Hinweis/Anhaltspunkt).
#'
#' @export
#'
#' @param grade Character string: "proof" (Beleg), "indication" (Hinweis),
#'   "hint" (Anhaltspunkt), or "none" (kein Beleg)
#' @param grade_de Character string: German grade name
#' @param direction Character string: "benefit", "harm", or "none"
#' @param certainty Numeric certainty score (0-1)
#' @param n_studies Integer number of studies
#' @param domains List of domain assessments
#' @param justification Character string explaining the grade
#' @param metadata List of additional metadata
#'
#' @return An EvidenceGrade object
#'
#' @examples
#' \dontrun{
#' grade <- EvidenceGrade(
#'   grade = "indication",
#'   grade_de = "Hinweis",
#'   direction = "benefit",
#'   n_studies = 3L,
#'   justification = "Consistent results from 3 RCTs with moderate risk of bias"
#' )
#' }
EvidenceGrade <- S7::new_class(
	"EvidenceGrade",
	package = "pharmhand",
	properties = list(
		grade = S7::new_property(
			S7::class_character,
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_grades <- c("proof", "indication", "hint", "none")
				if (!value %in% valid_grades) {
					return(sprintf(
						"grade must be one of: %s",
						paste(valid_grades, collapse = ", ")
					))
				}
				NULL
			}
		),
		grade_de = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		direction = S7::new_property(
			S7::class_character,
			default = "none",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				if (!value %in% c("benefit", "harm", "none")) {
					return("direction must be 'benefit', 'harm', or 'none'")
				}
				NULL
			}
		),
		certainty = S7::new_property(
			S7::class_numeric,
			default = NA_real_
		),
		n_studies = S7::new_property(S7::class_any, default = NA_integer_),
		domains = S7::new_property(S7::class_list, default = list()),
		justification = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

#' Abstract base class for all clinical content
#'
#' @keywords internal
#' @noRd
ClinicalContent <- S7::new_class(
	"ClinicalContent",
	package = "pharmhand",
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
#' @param flextable A flextable object (optional, created if not provided)
#' @param type Character string for table type
#'   (e.g., "demographics", "adverse_events")
#' @param title Character string for table title
#' @param metadata List of additional metadata
#'
#' @usage ClinicalTable(
#'   type = character(0),
#'   title = NULL,
#'   metadata = list(),
#'   data = (function (.data = list(), row.names = NULL) {
#'     if (is.null(row.names)) {
#'       list2DF(.data)
#'     } else {
#'       out <- list2DF(.data, length(row.names))
#'       attr(out, "row.names") <- row.names
#'       out
#'     }
#'   })(),
#'   flextable = NULL
#' )
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
	package = "pharmhand",
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
#' @param type Character string for plot type (e.g., "km", "forest")
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
	package = "pharmhand",
	parent = ClinicalContent,
	properties = list(
		plot = S7::new_property(
			class = S7::class_any,
			validator = function(value) {
				if (!inherits(value, c("ggplot", "ggsurvplot", "gg"))) {
					ph_abort("Plot must be a ggplot or ggsurvplot object")
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
	package = "pharmhand",
	properties = list(
		study_id = S7::new_property(
			S7::class_character,
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		study_title = S7::new_property(
			S7::class_character,
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
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
	package = "pharmhand",
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
	package = "pharmhand",
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

#' Study Class (Abstract Base)
#'
#' Abstract base class for clinical studies. Provides common properties
#' shared by all study types.
#'
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param design Character string for study design:
#'   "rct", "observational", "single-arm"
#' @param population Character string for population (e.g., "ITT", "FAS", "PP")
#' @param endpoints List of Endpoint objects
#' @param results List of analysis results
#' @param risk_of_bias Risk of bias assessment result (optional)
#' @param metadata List of additional metadata
#'
#' @return A Study object
#' @keywords internal
Study <- S7::new_class(
	"Study",
	package = "pharmhand",
	properties = list(
		study_id = S7::new_property(
			S7::class_character,
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		study_title = S7::new_property(
			S7::class_character,
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		design = S7::new_property(
			S7::class_character,
			default = "rct",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_designs <- c("rct", "observational", "single-arm", "crossover")
				if (!value %in% valid_designs) {
					return(sprintf(
						"design must be one of: %s",
						paste(valid_designs, collapse = ", ")
					))
				}
				NULL
			}
		),
		population = S7::new_property(
			S7::class_character,
			default = "ITT",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		endpoints = S7::new_property(S7::class_list, default = list()),
		results = S7::new_property(S7::class_list, default = list()),
		risk_of_bias = S7::new_property(S7::class_any, default = NULL),
		metadata = S7::new_property(S7::class_list, default = list())
	),
	abstract = TRUE
)

#' SingleArmStudy Class
#'
#' An S7 class for representing and analyzing single-arm clinical studies.
#' Inherits from Study.
#'
#' @export
#'
#' @param data A data frame containing the study data
#' @param treatment_var Character string for treatment variable name
#'   (default: "TRT01P")
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param design Character string for study design (default: "single-arm")
#' @param population Character string for population
#' @param endpoints List of Endpoint objects
#' @param results List of analysis results
#' @param risk_of_bias Risk of bias assessment result
#' @param metadata List of additional metadata
#'
#' @return A SingleArmStudy object
#'
#' @examples
#' \dontrun{
#' study <- SingleArmStudy(
#'   data = my_data,
#'   study_id = "STUDY001",
#'   study_title = "Phase II Study"
#' )
#' }
SingleArmStudy <- S7::new_class(
	"SingleArmStudy",
	package = "pharmhand",
	parent = Study,
	properties = list(
		data = S7::new_property(
			S7::class_data.frame,
			default = data.frame(),
			validator = function(value) {
				admiraldev::assert_data_frame(value)
				NULL
			}
		),
		treatment_var = S7::new_property(
			S7::class_character,
			default = "TRT01P",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		design = S7::new_property(
			S7::class_character,
			default = "single-arm"
		)
	)
)

#' TwoArmStudy Class
#'
#' An S7 class for representing and analyzing two-arm clinical studies.
#' Inherits from Study.
#'
#' @export
#'
#' @param data A data frame containing the study data
#' @param treatment_var Character string for treatment variable name
#' @param comparator Character string describing the comparator arm
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param design Character string for study design (default: "rct")
#' @param population Character string for population
#' @param endpoints List of Endpoint objects
#' @param results List of analysis results
#' @param risk_of_bias Risk of bias assessment result
#' @param metadata List of additional metadata
#'
#' @return A TwoArmStudy object
#'
#' @examples
#' \dontrun{
#' study <- TwoArmStudy(
#'   data = my_data,
#'   study_id = "STUDY001",
#'   study_title = "Phase III RCT",
#'   treatment_var = "TRT01P",
#'   comparator = "Placebo"
#' )
#' }
TwoArmStudy <- S7::new_class(
	"TwoArmStudy",
	package = "pharmhand",
	parent = Study,
	properties = list(
		data = S7::new_property(
			S7::class_data.frame,
			default = data.frame(),
			validator = function(value) {
				admiraldev::assert_data_frame(value)
				NULL
			}
		),
		treatment_var = S7::new_property(
			S7::class_character,
			default = "TRT01P",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		comparator = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		)
	)
)

#' MultiArmStudy Class
#'
#' An S7 class for representing and analyzing multi-arm clinical studies
#' (3+ treatment arms). Inherits from Study.
#'
#' @export
#'
#' @param data A data frame containing the study data
#' @param treatment_var Character string for treatment variable name
#' @param arms Character vector of treatment arm names
#' @param reference_arm Character string for the reference/control arm
#' @param study_id Character string for study identifier
#' @param study_title Character string for study title
#' @param design Character string for study design (default: "rct")
#' @param population Character string for population
#' @param endpoints List of Endpoint objects
#' @param results List of analysis results
#' @param risk_of_bias Risk of bias assessment result
#' @param metadata List of additional metadata
#'
#' @return A MultiArmStudy object
#'
#' @examples
#' \dontrun{
#' study <- MultiArmStudy(
#'   data = my_data,
#'   study_id = "STUDY001",
#'   study_title = "Phase III Multi-Arm RCT",
#'   treatment_var = "TRT01P",
#'   arms = c("Drug A", "Drug B", "Drug C", "Placebo"),
#'   reference_arm = "Placebo"
#' )
#' }
MultiArmStudy <- S7::new_class(
	"MultiArmStudy",
	package = "pharmhand",
	parent = Study,
	properties = list(
		data = S7::new_property(
			S7::class_data.frame,
			default = data.frame(),
			validator = function(value) {
				admiraldev::assert_data_frame(value)
				NULL
			}
		),
		treatment_var = S7::new_property(
			S7::class_character,
			default = "TRT01P",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		),
		arms = S7::new_property(
			S7::class_character,
			default = character(),
			validator = function(value) {
				admiraldev::assert_character_vector(value)
				if (length(value) > 0 && length(value) < 3) {
					return("arms must have at least 3 elements for a multi-arm study")
				}
				NULL
			}
		),
		reference_arm = S7::new_property(
			S7::class_character,
			default = "",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				NULL
			}
		)
	)
)

#' StudySet Class
#'
#' An S7 class for representing a collection of studies for evidence synthesis
#' (meta-analysis, indirect comparison, network meta-analysis).
#'
#' @export
#'
#' @param studies List of Study objects
#' @param endpoint Endpoint object being synthesized
#' @param comparison_type Type of comparison: "direct", "indirect", "network"
#' @param common_comparator Common comparator for indirect/network comparisons
#' @param characteristics Data frame of study-level characteristics (optional,
#'   NULL by default)
#' @param metadata List of additional metadata
#'
#' @return A StudySet object
#'
#' @examples
#' \dontrun{
#' study_set <- StudySet(
#'   studies = list(study1, study2, study3),
#'   endpoint = os_endpoint,
#'   comparison_type = "direct"
#' )
#' }
StudySet <- S7::new_class(
	"StudySet",
	package = "pharmhand",
	properties = list(
		studies = S7::new_property(
			S7::class_list,
			default = list(),
			validator = function(value) {
				if (length(value) > 0) {
					# Check all elements are Study objects
					for (i in seq_along(value)) {
						if (!S7::S7_inherits(value[[i]], Study)) {
							return(sprintf(
								"Element %d of studies must be a Study object",
								i
							))
						}
					}
				}
				NULL
			}
		),
		endpoint = S7::new_property(S7::class_any, default = NULL),
		comparison_type = S7::new_property(
			S7::class_character,
			default = "direct",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_types <- c("direct", "indirect", "network")
				if (!value %in% valid_types) {
					return(sprintf(
						"comparison_type must be one of: %s",
						paste(valid_types, collapse = ", ")
					))
				}
				NULL
			}
		),
		common_comparator = S7::new_property(S7::class_any, default = NULL),
		characteristics = S7::new_property(
			S7::class_any,
			default = NULL
		),
		metadata = S7::new_property(S7::class_list, default = list()),
		# Computed properties
		n_studies = S7::new_property(
			class = S7::class_integer,
			getter = function(self) length(self@studies)
		),
		study_ids = S7::new_property(
			class = S7::class_character,
			getter = function(self) {
				vapply(self@studies, function(s) s@study_id, character(1))
			}
		)
	)
)

#' Endpoint Class
#'
#' An S7 class for representing clinical study endpoints.
#' Replaces the previous PrimaryEndpoint, SecondaryEndpoint, and SafetyEndpoint
#' classes with a unified class using the `category` property.
#'
#' @export
#'
#' @param name Character string for endpoint name
#' @param variable Character string for variable name in the dataset
#' @param type Character string for endpoint type:
#'   "continuous", "binary", "tte", "count", "pro"
#' @param category Character string for endpoint category:
#'   "primary", "secondary", "safety", "exploratory"
#' @param description Character string for endpoint description
#' @param hypothesis Character string for hypothesis type:
#'   "superiority", "non-inferiority", "equivalence"
#' @param margin Numeric value for non-inferiority/equivalence margin
#'   (if applicable)
#' @param alpha Numeric value for significance level (default: 0.05)
#' @param priority Numeric value for analysis priority (default: 1)
#' @param metadata List of additional metadata
#'
#' @return An Endpoint object
#'
#' @examples
#' \dontrun{
#' # Primary efficacy endpoint
#' endpoint <- Endpoint(
#'   name = "Overall Survival",
#'   variable = "AVAL",
#'   type = "tte",
#'   category = "primary",
#'   hypothesis = "superiority"
#' )
#'
#' # Safety endpoint
#' ae_endpoint <- Endpoint(
#'   name = "Treatment-Emergent AEs",
#'   variable = "AETERM",
#'   type = "count",
#'   category = "safety"
#' )
#' }
Endpoint <- S7::new_class(
	"Endpoint",
	package = "pharmhand",
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
			default = "continuous",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_types <- c("continuous", "binary", "tte", "count", "pro")
				if (!value %in% valid_types) {
					return(sprintf(
						"type must be one of: %s",
						paste(valid_types, collapse = ", ")
					))
				}
				NULL
			}
		),
		category = S7::new_property(
			S7::class_character,
			default = "primary",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_cats <- c("primary", "secondary", "safety", "exploratory")
				if (!value %in% valid_cats) {
					return(sprintf(
						"category must be one of: %s",
						paste(valid_cats, collapse = ", ")
					))
				}
				NULL
			}
		),
		description = S7::new_property(S7::class_any, default = NULL),
		hypothesis = S7::new_property(
			S7::class_character,
			default = "superiority",
			validator = function(value) {
				admiraldev::assert_character_scalar(value)
				valid_hyp <- c("superiority", "non-inferiority", "equivalence")
				if (!value %in% valid_hyp) {
					return(sprintf(
						"hypothesis must be one of: %s",
						paste(valid_hyp, collapse = ", ")
					))
				}
				NULL
			}
		),
		margin = S7::new_property(S7::class_any, default = NULL),
		alpha = S7::new_property(
			S7::class_numeric,
			default = 0.05,
			validator = function(value) {
				admiraldev::assert_numeric_vector(value, len = 1)
				if (value <= 0 || value >= 1) {
					return("alpha must be between 0 and 1")
				}
				NULL
			}
		),
		priority = S7::new_property(
			S7::class_numeric,
			default = 1,
			validator = function(value) {
				admiraldev::assert_numeric_vector(value, len = 1)
				NULL
			}
		),
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
	package = "pharmhand",
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
	package = "pharmhand",
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
	package = "pharmhand",
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
	package = "pharmhand",
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
#' @param category Character string for endpoint category
#'   (inherited from Endpoint)
#' @param priority Numeric value for analysis priority (inherited from Endpoint)
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
	package = "pharmhand",
	parent = Endpoint,
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
	package = "pharmhand",
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
					return("timestamp must be NULL or POSIXct")
				}
				NULL
			}
		),
		package_version = S7::new_property(S7::class_character, default = ""),
		r_version = S7::new_property(S7::class_character, default = "")
	)
)

#' Create Analysis Metadata
#'
#' Helper to create an AnalysisMeta object with current environment info.
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
		package_version = as.character(utils::packageVersion("pharmhand")),
		r_version = {
			# Convert patch to character for safe NA handling
			patch_char <- as.character(R.version$patch)

			# Use patch if it has length > 0, is not NA, and is not empty (nzchar)
			if (length(patch_char) > 0 && !is.na(patch_char) && nzchar(patch_char)) {
				patch <- patch_char
			} else {
				# Fall back to parsing from minor version
				minor_parts <- strsplit(as.character(R.version$minor), "\\.")[[1]]
				patch <- if (length(minor_parts) > 1) minor_parts[2] else "0"
			}

			# Get minor version (first part of R.version$minor)
			minor <- strsplit(as.character(R.version$minor), "\\.")[[1]][1]

			paste0(R.version$major, ".", minor, ".", patch)
		}
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
	package = "pharmhand",
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
