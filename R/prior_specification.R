#' @title Prior Specification Interface
#' @name prior_specification
#' @description Functions for specifying and managing prior distributions
#'   in Bayesian analyses.
NULL

# =============================================================================
# PriorSpecification S7 Class
# =============================================================================

#' PriorSpecification Class
#'
#' An S7 class for representing prior distribution specifications.
#'
#' @export
#'
#' @param distribution Character string specifying the distribution type
#' @param parameters List of distribution parameters
#' @param description Character string describing the prior
#' @param domain Character string indicating the parameter domain
#' @param reference Character string with reference/citation
#' @param metadata List of additional metadata
#'
#' @return A PriorSpecification object
PriorSpecification <- S7::new_class(
	"PriorSpecification",
	package = "pharmhand",
	properties = list(
		distribution = S7::new_property(
			S7::class_character,
			validator = function(value) {
				valid_distributions <- c(
					"normal",
					"beta",
					"gamma",
					"half_cauchy",
					"half_normal",
					"exponential",
					"uniform",
					"cauchy",
					"t",
					"log_normal",
					"inverse_gamma",
					"laplace",
					"student_t"
				)
				if (!value %in% valid_distributions) {
					return(sprintf(
						"distribution must be one of: %s",
						paste(valid_distributions, collapse = ", ")
					))
				}
				NULL
			}
		),
		parameters = S7::new_property(
			S7::class_list,
			validator = function(value) {
				if (!is.list(value)) {
					return("parameters must be a list")
				}
				NULL
			}
		),
		description = S7::new_property(
			S7::class_character,
			default = ""
		),
		domain = S7::new_property(
			S7::class_character,
			default = ""
		),
		reference = S7::new_property(
			S7::class_character,
			default = ""
		),
		metadata = S7::new_property(S7::class_list, default = list())
	)
)

# =============================================================================
# Prior Specification Functions
# =============================================================================

#' Create Prior Specification
#'
#' Creates a prior distribution specification for use in Bayesian analyses.
#'
#' @param distribution Character string specifying the distribution:
#'   "normal", "beta", "gamma", "half_cauchy", "half_normal", "exponential",
#'   "uniform", "cauchy", "t", "log_normal", "inverse_gamma", "laplace",
#'   "student_t"
#' @param parameters List of distribution parameters (see details)
#' @param description Character string describing the prior
#' @param domain Character string indicating the parameter domain
#' @param reference Character string with reference/citation
#' @param metadata List of additional metadata
#'
#' @return A PriorSpecification object
#'
#' @details
#' Distribution parameters:
#' \describe{
#'   \item{normal}{mean, sd}
#'   \item{beta}{shape1, shape2}
#'   \item{gamma}{shape, rate (or scale)}
#'   \item{half_cauchy}{location, scale}
#'   \item{half_normal}{sd}
#'   \item{exponential}{rate}
#'   \item{uniform}{min, max}
#'   \item{cauchy}{location, scale}
#'   \item{t}{df, location, scale}
#'   \item{log_normal}{meanlog, sdlog}
#'   \item{inverse_gamma}{shape, scale}
#'   \item{laplace}{location, scale}
#'   \item{student_t}{df, location, scale}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Vague normal prior
#' prior1 <- create_prior_specification(
#'   distribution = "normal",
#'   parameters = list(mean = 0, sd = 10),
#'   description = "Vague prior for overall effect",
#'   domain = "overall_effect"
#' )
#'
#' # Informative beta prior
#' prior2 <- create_prior_specification(
#'   distribution = "beta",
#'   parameters = list(shape1 = 2, shape2 = 2),
#'   description = "Moderately informative prior for probability",
#'   domain = "probability"
#' )
#'
#' # Half-Cauchy prior for variance component
#' prior3 <- create_prior_specification(
#'   distribution = "half_cauchy",
#'   parameters = list(location = 0, scale = 0.5),
#'   description = "Default prior for heterogeneity",
#'   domain = "heterogeneity"
#' )
#' }
create_prior_specification <- function(
	distribution,
	parameters,
	description = "",
	domain = "",
	reference = "",
	metadata = list()
) {
	# Validate distribution
	if (!is.character(distribution) || length(distribution) != 1) {
		ph_abort("'distribution' must be a single character string")
	}

	# Validate parameters
	if (!is.list(parameters)) {
		ph_abort("'parameters' must be a list")
	}

	# Validate parameters for the specific distribution
	validation_error <- validate_prior_parameters(distribution, parameters)
	if (!is.null(validation_error)) {
		ph_abort(validation_error)
	}

	# Create the prior specification
	PriorSpecification(
		distribution = distribution,
		parameters = parameters,
		description = description,
		domain = domain,
		reference = reference,
		metadata = metadata
	)
}

#' Get Default Prior
#'
#' Returns a default prior specification for common Bayesian analysis domains.
#'
#' @param domain Character string specifying the domain:
#'   "overall_effect", "heterogeneity", "treatment_effect", "baseline_risk",
#'   "probability", "variance", "correlation", "few_studies"
#'
#' @return A PriorSpecification object, or NULL if domain not recognized
#' @export
#'
#' @examples
#' \dontrun{
#' # Default prior for overall effect
#' prior <- get_default_prior("overall_effect")
#'
#' # Default prior for heterogeneity
#' prior <- get_default_prior("heterogeneity")
#' }
get_default_prior <- function(domain) {
	priors <- list(
		# Overall effect (log scale for ratio measures)
		overall_effect = create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 10),
			description = "Vague prior for overall effect (log scale)",
			domain = "overall_effect",
			reference = "Spiegelhalter et al. 2004"
		),

		# Between-study heterogeneity (tau)
		heterogeneity = create_prior_specification(
			distribution = "half_cauchy",
			parameters = list(location = 0, scale = 0.5),
			description = "Half-Cauchy prior for between-study SD",
			domain = "heterogeneity",
			reference = "Gelman 2006"
		),

		# Treatment effect (more informative)
		treatment_effect = create_prior_specification(
			distribution = "normal",
			parameters = list(mean = 0, sd = 2),
			description = "Moderately informative prior for treatment effect",
			domain = "treatment_effect",
			reference = "Standard practice"
		),

		# Baseline risk (probability scale)
		baseline_risk = create_prior_specification(
			distribution = "beta",
			parameters = list(shape1 = 1, shape2 = 1),
			description = "Uniform prior for baseline risk",
			domain = "baseline_risk",
			reference = "Non-informative"
		),

		# General probability parameters
		probability = create_prior_specification(
			distribution = "beta",
			parameters = list(shape1 = 1, shape2 = 1),
			description = "Uniform prior for probability",
			domain = "probability",
			reference = "Non-informative"
		),

		# Variance components
		variance = create_prior_specification(
			distribution = "half_cauchy",
			parameters = list(location = 0, scale = 1),
			description = "Half-Cauchy prior for variance",
			domain = "variance",
			reference = "Gelman 2006"
		),

		# Correlation parameters
		correlation = create_prior_specification(
			distribution = "uniform",
			parameters = list(min = -1, max = 1),
			description = "Uniform prior for correlation",
			domain = "correlation",
			reference = "Non-informative"
		),

		# Specialized prior for few studies
		few_studies = create_prior_specification(
			distribution = "half_cauchy",
			parameters = list(location = 0, scale = 0.25),
			description = "Conservative prior for few studies heterogeneity",
			domain = "few_studies",
			reference = "Conservative for small k"
		)
	)

	# Return the requested prior or NULL if not found
	if (domain %in% names(priors)) {
		return(priors[[domain]])
	} else {
		ph_warn(sprintf(
			"Unrecognized domain '%s'. Available domains: %s",
			domain,
			paste(names(priors), collapse = ", ")
		))
		return(NULL)
	}
}

#' Validate Prior Parameters
#'
#' Validates that parameters are appropriate for the specified distribution.
#'
#' @param distribution Character string specifying the distribution
#' @param parameters List of distribution parameters
#'
#' @return NULL if valid, character string with error message if invalid
#' @export
#'
#' @examples
#' \dontrun{
#' # Valid normal distribution
#' validate_prior_parameters("normal", list(mean = 0, sd = 1))
#'
#' # Invalid normal distribution (missing sd)
#' validate_prior_parameters("normal", list(mean = 0))
#' }
validate_prior_parameters <- function(distribution, parameters) {
	# Check required parameters for each distribution
	required_params <- switch(
		distribution,
		normal = c("mean", "sd"),
		beta = c("shape1", "shape2"),
		gamma = {
			if ("scale" %in% names(parameters) && !"rate" %in% names(parameters)) {
				c("shape", "scale")
			} else {
				c("shape", "rate")
			}
		},
		half_cauchy = c("location", "scale"),
		half_normal = "sd",
		exponential = "rate",
		uniform = c("min", "max"),
		cauchy = c("location", "scale"),
		t = c("df", "location", "scale"),
		log_normal = c("meanlog", "sdlog"),
		inverse_gamma = c("shape", "scale"),
		laplace = c("location", "scale"),
		student_t = c("df", "location", "scale"),
		NULL # Unknown distribution
	)

	if (is.null(required_params)) {
		return(NULL) # Unknown distribution, no validation
	}

	# Check that all required parameters are present
	missing_params <- setdiff(required_params, names(parameters))
	if (length(missing_params) > 0) {
		return(sprintf(
			"Missing required parameters for %s distribution: %s",
			distribution,
			paste(missing_params, collapse = ", ")
		))
	}

	# Validate parameter values based on distribution
	switch(
		distribution,
		normal = {
			if (parameters$sd <= 0) {
				return("Normal distribution 'sd' must be positive")
			}
		},
		beta = {
			if (parameters$shape1 <= 0 || parameters$shape2 <= 0) {
				return("Beta distribution parameters must be positive")
			}
		},
		gamma = {
			shape_val <- parameters$shape
			rate_or_scale <- if ("rate" %in% names(parameters)) {
				parameters$rate
			} else {
				parameters$scale
			}
			if (shape_val <= 0 || rate_or_scale <= 0) {
				return("Gamma distribution parameters must be positive")
			}
		},
		half_cauchy = {
			if (parameters$scale <= 0) {
				return("Half-Cauchy distribution 'scale' must be positive")
			}
		},
		half_normal = {
			if (parameters$sd <= 0) {
				return("Half-Normal distribution 'sd' must be positive")
			}
		},
		exponential = {
			if (parameters$rate <= 0) {
				return("Exponential distribution 'rate' must be positive")
			}
		},
		uniform = {
			if (parameters$min >= parameters$max) {
				return("Uniform distribution 'min' must be less than 'max'")
			}
		},
		cauchy = {
			if (parameters$scale <= 0) {
				return("Cauchy distribution 'scale' must be positive")
			}
		},
		t = {
			if (parameters$df <= 0) {
				return("t distribution 'df' must be positive")
			}
			if (parameters$scale <= 0) {
				return("t distribution 'scale' must be positive")
			}
		},
		log_normal = {
			if (parameters$sdlog <= 0) {
				return("Log-Normal distribution 'sdlog' must be positive")
			}
		},
		inverse_gamma = {
			if (parameters$shape <= 0 || parameters$scale <= 0) {
				return("Inverse-Gamma distribution parameters must be positive")
			}
		},
		laplace = {
			if (parameters$scale <= 0) {
				return("Laplace distribution 'scale' must be positive")
			}
		},
		student_t = {
			if (parameters$df <= 0) {
				return("Student-t distribution 'df' must be positive")
			}
			if (parameters$scale <= 0) {
				return("Student-t distribution 'scale' must be positive")
			}
		}
	)

	NULL # All validations passed
}

#' Summarize Prior Specification
#'
#' Creates a summary data frame from a prior specification.
#'
#' @param prior A PriorSpecification object
#'
#' @return A data frame with prior summary information
#' @export
#'
#' @examples
#' \dontrun{
#' prior <- create_prior_specification("normal", list(mean = 0, sd = 1))
#' summary <- summarize_prior_specification(prior)
#' print(summary)
#' }
summarize_prior_specification <- function(prior) {
	if (!S7::S7_inherits(prior, PriorSpecification)) {
		ph_abort("'prior' must be a PriorSpecification object")
	}

	# Format parameters as string
	param_str <- paste(
		names(prior@parameters),
		prior@parameters,
		sep = "=",
		collapse = ", "
	)

	data.frame(
		distribution = prior@distribution,
		parameters = param_str,
		description = prior@description,
		domain = prior@domain,
		reference = prior@reference,
		stringsAsFactors = FALSE
	)
}

#' Create Prior Specification Set
#'
#' Creates a named list of prior specifications for use in Bayesian models.
#'
#' @param priors Named list of PriorSpecification objects
#'
#' @return Named list of PriorSpecification objects
#' @export
#'
#' @examples
#' \dontrun{
#' priors <- create_prior_specification_set(list(
#'   overall = get_default_prior("overall_effect"),
#'   heterogeneity = get_default_prior("heterogeneity")
#' ))
#' }
create_prior_specification_set <- function(priors) {
	if (!is.list(priors)) {
		ph_abort("'priors' must be a list")
	}

	# Validate that all elements are named
	prior_names <- names(priors)
	if (
		is.null(prior_names) || any(prior_names == "") || any(is.na(prior_names))
	) {
		ph_abort("All elements in 'priors' must be named")
	}

	# Validate that all elements are PriorSpecification objects
	for (name in names(priors)) {
		if (!S7::S7_inherits(priors[[name]], PriorSpecification)) {
			ph_abort(sprintf(
				"Element '%s' must be a PriorSpecification object",
				name
			))
		}
	}

	# Return as named list
	priors
}

#' Create Default Prior Set for Meta-Analysis
#'
#' Creates a standard set of priors for Bayesian meta-analysis.
#'
#' @param few_studies Logical. Whether to use priors appropriate for few studies
#' @param informative Logical. Whether to use more informative priors
#'
#' @return Named list of PriorSpecification objects
#' @export
#'
#' @examples
#' \dontrun{
#' # Default priors for standard meta-analysis
#' priors <- create_meta_analysis_priors()
#'
#' # Priors for few studies
#' priors <- create_meta_analysis_priors(few_studies = TRUE)
#'
#' # More informative priors
#' priors <- create_meta_analysis_priors(informative = TRUE)
#' }
create_meta_analysis_priors <- function(
	few_studies = FALSE,
	informative = FALSE
) {
	if (few_studies && informative) {
		ph_warn(
			"Combining few_studies=TRUE with informative=TRUE may be contradictory"
		)
	}

	if (few_studies) {
		# Conservative priors for few studies
		priors <- list(
			overall_effect = create_prior_specification(
				distribution = "normal",
				parameters = list(mean = 0, sd = 2),
				description = "Conservative prior for overall effect (few studies)",
				domain = "overall_effect"
			),
			heterogeneity = create_prior_specification(
				distribution = "half_cauchy",
				parameters = list(location = 0, scale = 0.25),
				description = "Conservative prior for heterogeneity (few studies)",
				domain = "heterogeneity"
			)
		)
	} else if (informative) {
		# More informative priors
		priors <- list(
			overall_effect = create_prior_specification(
				distribution = "normal",
				parameters = list(mean = 0, sd = 1),
				description = "Informative prior for overall effect",
				domain = "overall_effect"
			),
			heterogeneity = create_prior_specification(
				distribution = "half_normal",
				parameters = list(sd = 0.5),
				description = "Informative prior for heterogeneity",
				domain = "heterogeneity"
			)
		)
	} else {
		# Default vague priors
		priors <- list(
			overall_effect = get_default_prior("overall_effect"),
			heterogeneity = get_default_prior("heterogeneity")
		)
	}

	create_prior_specification_set(priors)
}
