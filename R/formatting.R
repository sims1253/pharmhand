#' Format String Grammar
#'
#' Declarative formatting system for clinical tables.
#'
#' @name formatting
NULL

#' Create Format Specification
#'
#' Define numeric formats using pattern strings.
#'
#' @param pattern Format pattern string (default "a.a")
#' @param null_format String to display for NULL/NA values (default "--")
#' @param empty_format String to display for empty values (default "")
#' @param neg_format How to format negative numbers: "sign", "parens", or "abs"
#'
#' @return A FormatSpec object
#' @export
#'
#' @examples
#' \dontrun{
#' fmt <- format_spec("xx.x")
#' apply_format(fmt, 12.345) # "12.3"
#'
#' # Auto-width with 2 decimals
#' fmt <- format_spec("a.xx")
#' apply_format(fmt, c(1.5, 123.456)) # "  1.50", "123.46"
#' }
format_spec <- function(
	pattern = "a.a",
	null_format = "--",
	empty_format = "",
	neg_format = "sign"
) {
	FormatSpec(
		pattern = pattern,
		null_format = null_format,
		empty_format = empty_format,
		neg_format = neg_format
	)
}

#' FormatSpec S7 Class
#'
#' Internal class for storing format specifications.
#'
#' @keywords internal
#' @export
FormatSpec <- S7::new_class(
	"FormatSpec",
	package = "pharmhand",
	properties = list(
		pattern = S7::new_property(
			S7::class_character,
			validator = function(value) {
				if (!grepl("^[ax\\.\\+0-9]+$", value)) {
					"Pattern must contain only 'a', 'x', '.', '+', or digits"
				}
			}
		),
		null_format = S7::new_property(S7::class_character, default = "--"),
		empty_format = S7::new_property(S7::class_character, default = ""),
		neg_format = S7::new_property(
			S7::class_character,
			default = "sign",
			validator = function(value) {
				if (!value %in% c("sign", "parens", "abs")) {
					"neg_format must be 'sign', 'parens', or 'abs'"
				}
			}
		),
		# Parsed components (computed on creation)
		int_width = S7::new_property(S7::class_any, default = NULL),
		dec_width = S7::new_property(S7::class_any, default = NULL),
		auto_int = S7::new_property(S7::class_logical, default = FALSE),
		auto_dec = S7::new_property(S7::class_logical, default = FALSE),
		dec_adjust = S7::new_property(S7::class_integer, default = 0L)
	)
)

#' Parse a format pattern
#'
#' Extract integer width, decimal width, and auto flags from pattern.
#'
#' @param pattern Format pattern string
#'
#' @return List with parsed components
#' @keywords internal
parse_format_pattern <- function(pattern) {
	# Split on decimal point

	parts <- strsplit(pattern, "\\.")[[1]]

	int_part <- parts[1]
	dec_part <- if (length(parts) > 1) parts[2] else ""

	# Parse integer part
	if (grepl("^a$", int_part)) {
		auto_int <- TRUE
		int_width <- NULL
	} else {
		auto_int <- FALSE
		int_width <- nchar(gsub("[^x]", "", int_part))
	}

	# Parse decimal part
	dec_adjust <- 0L
	if (nchar(dec_part) == 0) {
		auto_dec <- FALSE
		dec_width <- 0L
	} else if (grepl("^a(\\+[0-9]+)?$", dec_part)) {
		auto_dec <- TRUE
		dec_width <- NULL
		# Check for +N adjustment
		if (grepl("\\+", dec_part)) {
			dec_adjust <- as.integer(gsub(".*\\+", "", dec_part))
		}
	} else {
		auto_dec <- FALSE
		dec_width <- nchar(gsub("[^x]", "", dec_part))
	}

	list(
		int_width = int_width,
		dec_width = dec_width,
		auto_int = auto_int,
		auto_dec = auto_dec,
		dec_adjust = dec_adjust
	)
}

#' Apply a format specification to values
#'
#' Format numeric values according to the specification.
#'
#' @param spec A FormatSpec object or pattern string
#' @param x Numeric vector to format
#' @param align Logical, right-align values for tabular display
#'
#' @return Character vector of formatted values
#' @export
#'
#' @examples
#' \dontrun{
#' apply_format("xx.x", 12.345) # "12.3"
#' apply_format("a.xx", c(1, 123)) # "  1.00", "123.00"
#' }
apply_format <- function(spec, x, align = TRUE) {
	# Convert pattern string to FormatSpec if needed
	if (is.character(spec)) {
		spec <- format_spec(spec)
	}

	if (!S7::S7_inherits(spec, FormatSpec)) {
		ph_abort("'spec' must be a FormatSpec or pattern string")
	}

	# Parse pattern
	parsed <- parse_format_pattern(spec@pattern)

	# Handle NULL/NA
	result <- character(length(x))
	na_mask <- is.na(x)
	result[na_mask] <- spec@null_format

	if (all(na_mask)) {
		return(result)
	}

	x_valid <- x[!na_mask]

	# Determine actual widths
	if (parsed$auto_int) {
		int_width <- max(nchar(as.character(floor(abs(x_valid)))))
	} else {
		int_width <- parsed$int_width
	}

	if (parsed$auto_dec) {
		# Auto-detect decimals based on data precision
		dec_width <- max(vapply(
			x_valid,
			function(v) {
				s <- format(v, scientific = FALSE)
				if (grepl("\\.", s)) nchar(gsub(".*\\.", "", s)) else 0L
			},
			integer(1)
		)) +
			parsed$dec_adjust
		dec_width <- max(dec_width, 0L)
	} else {
		dec_width <- parsed$dec_width
	}

	# Format values
	# Account for extra characters when using parens format for negatives
	# Parens format: "(12.3)" = 2 extra chars vs positive "12.3"
	total_width <- int_width + ifelse(dec_width > 0, dec_width + 1, 0)
	if (spec@neg_format == "parens") {
		total_width <- total_width + 2
	}

	formatted <- vapply(
		x_valid,
		function(v) {
			# Handle negatives
			if (spec@neg_format == "abs") {
				v <- abs(v)
			}

			if (dec_width > 0) {
				s <- sprintf(paste0("%.", dec_width, "f"), v)
			} else {
				s <- sprintf("%.0f", v)
			}

			# Apply negative format
			if (v < 0 && spec@neg_format == "parens") {
				s <- paste0("(", gsub("-", "", s, fixed = TRUE), ")")
			}

			# Right-align if requested
			if (align && nchar(s) < total_width) {
				s <- paste0(strrep(" ", total_width - nchar(s)), s)
			}

			s
		},
		character(1)
	)

	result[!na_mask] <- formatted
	result
}

#' Composite Format Specification
#'
#' Create a format that combines multiple values (e.g., "n (pct%)").
#'
#' @param template Character template with placeholders
#' @param ... Named format specs for each placeholder
#'
#' @return A CompositeFormat object
#' @export
#'
#' @examples
#' \dontrun{
#' fmt <- composite_format(
#'   "{n} ({pct}%)",
#'   n = "a",
#'   pct = "xx.x"
#' )
#' apply_composite(fmt, n = 15, pct = 23.456) # "15 (23.5%)"
#' }
composite_format <- function(template, ...) {
	specs <- list(...)
	CompositeFormat(
		template = template,
		specs = specs
	)
}

#' CompositeFormat S7 Class
#'
#' @keywords internal
#' @export
CompositeFormat <- S7::new_class(
	"CompositeFormat",
	package = "pharmhand",
	properties = list(
		template = S7::new_property(S7::class_character),
		specs = S7::new_property(S7::class_list, default = list())
	)
)

#' Apply a composite format
#'
#' Substitutes placeholder values into a composite format template. Each
#' placeholder in the template (e.g., `{n}`) is replaced with the corresponding
#' formatted value.
#'
#' @param fmt CompositeFormat object
#' @param ... Named values matching template placeholders
#'
#' @return Formatted character string
#'
#' @note This function uses simple string substitution via `gsub()`. If a
#'   placeholder name accidentally appears as literal text in the template
#'   (not as a placeholder), it will also be replaced. Ensure placeholder names
#'   are unique and unlikely to appear as regular text in templates.
#'
#' @export
apply_composite <- function(fmt, ...) {
	values <- list(...)

	result <- fmt@template

	# Note: gsub replaces ALL occurrences of the placeholder pattern.
	# If the placeholder name appears as literal text, it will also be replaced.
	for (name in names(values)) {
		spec <- fmt@specs[[name]]
		if (is.null(spec)) {
			spec <- "a"
		}

		formatted_val <- apply_format(spec, values[[name]], align = FALSE)
		result <- gsub(paste0("\\{", name, "\\}"), formatted_val, result)
	}

	result
}

#' Common Clinical Format Presets
#'
#' Pre-defined format specifications for common clinical table patterns.
#'
#' @name format_presets
NULL

#' @describeIn format_presets Count with percentage: "n (xx.x%)"
#' @export
fmt_n_pct <- function() {
	composite_format("{n} ({pct}%)", n = "a", pct = "xx.x")
}

#' @describeIn format_presets Mean with SD: "xx.x (xx.xx)"
#' @export
fmt_mean_sd <- function() {
	composite_format("{mean} ({sd})", mean = "a.x", sd = "a.xx")
}

#' @describeIn format_presets Median with range: "xx.x (xx.x, xx.x)"
#' @export
fmt_median_range <- function() {
	composite_format(
		"{median} ({min}, {max})",
		median = "a.x",
		min = "a.x",
		max = "a.x"
	)
}

#' @describeIn format_presets Confidence interval: "xx.xx (xx.xx, xx.xx)"
#' @export
fmt_ci <- function() {
	composite_format(
		"{est} ({lower}, {upper})",
		est = "a.xx",
		lower = "a.xx",
		upper = "a.xx"
	)
}

# =============================================================================
# IQWiG-Compliant Formatting Functions
# =============================================================================

#' Format P-Value (IQWiG-Compliant)
#'
#' Formats p-values according to IQWiG Methods v8.0, Chapter 10.3.2 (p. 212):
#' - 3 decimal places
#' - Values < 0.001 displayed as "<0.001" (or "<0,001" in German locale)
#' - Locale-aware decimal separator
#'
#' @param p Numeric p-value or vector of p-values
#' @param digits Integer number of decimal places (default: 3, per IQWiG)
#' @param threshold Numeric threshold below which to display "<threshold"
#'   (default: 0.001)
#' @param locale Character locale for decimal separator: "en" or "de"
#'   (default: current pharmhand locale)
#' @param trim Logical, if TRUE remove trailing zeros (default: FALSE)
#' @param na_string String for missing values
#'   (default: getOption("pharmhand.na_string", "NA"))
#'
#' @return Character vector of formatted p-values
#' @export
#'
#' @references
#' IQWiG (2023). Allgemeine Methoden, Version 8.0, Chapter 10.3.2, p. 212.
#'
#' @examples
#' format_pvalue(0.0234)
#' # [1] "0.023"
#'
#' format_pvalue(0.0234, locale = "de")
#' # [1] "0,023"
#'
#' format_pvalue(0.00005)
#' # [1] "<0.001"
#'
#' format_pvalue(c(0.05, 0.001, 0.0001))
#' # [1] "0.050" "0.001" "<0.001"
format_pvalue <- function(
	p,
	digits = 3L,
	threshold = 0.001,
	locale = get_locale(),
	trim = FALSE,
	na_string = getOption("pharmhand.na_string", "NA")
) {
	# Vectorized implementation
	vapply(
		p,
		function(pval) {
			# Handle NA
			if (is.na(pval)) {
				return(na_string)
			}

			# Validate range
			if (pval < 0 || pval > 1) {
				warning("p-value outside [0, 1] range: ", pval)
			}

			# Format based on threshold
			if (pval < threshold) {
				result <- paste0(
					"<",
					format_number(threshold, digits, locale, trim, na_string)
				)
			} else {
				result <- format_number(pval, digits, locale, trim, na_string)
			}

			result
		},
		character(1)
	)
}

#' Format Confidence Interval (IQWiG-Compliant)
#'
#' Formats confidence intervals according to IQWiG Methods v8.0, Chapter 10.3.2:
#' - Semicolon separator: \code{[lower; upper]}
#' - Locale-aware decimal separator
#'
#' @param lower Numeric lower bound (or vector)
#' @param upper Numeric upper bound (or vector)
#' @param digits Integer number of decimal places (default: 2)
#' @param locale Character locale for decimal separator: "en" or "de"
#'   (default: current pharmhand locale)
#' @param trim Logical, if TRUE remove trailing zeros (default: FALSE)
#' @param na_string String for missing values
#'   (default: getOption("pharmhand.na_string", "NA"))
#' @param brackets Character vector of length 2 for opening/closing brackets
#'   (default: \code{c("[", "]")})
#'
#' @return Character vector of formatted confidence intervals
#' @export
#'
#' @references
#' IQWiG (2023). Allgemeine Methoden, Version 8.0, Chapter 10.3.2, p. 212.
#'
#' @examples
#' format_ci(0.85, 1.23)
#' # [1] "[0.85; 1.23]"
#'
#' format_ci(0.85, 1.23, locale = "de")
#' # [1] "[0,85; 1,23]"
#'
#' format_ci(c(0.5, 0.7), c(0.9, 1.1))
#' # [1] "[0.50; 0.90]" "[0.70; 1.10]"
format_ci <- function(
	lower,
	upper,
	digits = 2L,
	locale = get_locale(),
	trim = FALSE,
	na_string = getOption("pharmhand.na_string", "NA"),
	brackets = c("[", "]")
) {
	# Validate inputs
	if (length(lower) != length(upper)) {
		ph_abort("'lower' and 'upper' must have the same length")
	}

	# Vectorized formatting
	mapply(
		function(l, u) {
			if (is.na(l) || is.na(u)) {
				return(na_string)
			}

			l_fmt <- format_number(l, digits, locale, trim, na_string)
			u_fmt <- format_number(u, digits, locale, trim, na_string)

			paste0(brackets[1], l_fmt, "; ", u_fmt, brackets[2])
		},
		lower,
		upper,
		USE.NAMES = FALSE
	)
}

#' @keywords internal
round_half_up <- function(x, digits = 0L) {
	factor <- 10^digits
	sign(x) * floor(abs(x) * factor + 0.5) / factor
}

#' Format Number with Locale
#'
#' Helper function to format a number with locale-aware decimal separator.
#'
#' @param x Numeric value
#' @param digits Integer number of decimal places
#' @param locale Character locale: "en" (period) or "de" (comma)
#' @param trim Logical, if TRUE remove trailing zeros (default: FALSE)
#' @param na_string String for missing values
#'   (default: getOption("pharmhand.na_string", "NA"))
#'
#' @return Character formatted number
#' @export
#'
#' @examples
#' format_number(1234.567, 2, "en")
#' # [1] "1234.57"
#'
#' format_number(1234.567, 2, "de")
#' # [1] "1234,57"
format_number <- function(
	x,
	digits = 2L,
	locale = get_locale(),
	trim = FALSE,
	na_string = getOption("pharmhand.na_string", "NA")
) {
	if (length(x) == 0) {
		return(character())
	}

	vapply(
		x,
		function(value) {
			if (is.na(value)) {
				return(na_string)
			}

			rounded <- round_half_up(value, digits)
			result <- sprintf(paste0("%.", digits, "f"), rounded)

			# Trim trailing zeros and decimal point if requested
			if (trim) {
				result <- sub("0+$", "", result)
				result <- sub("\\.$", "", result)
			}

			# Replace decimal separator for German locale
			if (locale == "de") {
				result <- gsub(".", ",", result, fixed = TRUE)
			}

			result
		},
		character(1)
	)
}

#' Format Percentage
#'
#' Formats percentages with locale-aware decimal separator.
#'
#' @param x Numeric value (proportion 0-1 or percentage 0-100)
#' @param digits Integer number of decimal places (default: 1)
#' @param locale Character locale: "en" or "de"
#' @param trim Logical, if TRUE remove trailing zeros (default: FALSE)
#' @param na_string String for missing values
#'   (default: getOption("pharmhand.na_string", "NA"))
#' @param is_proportion Logical, if TRUE treats x as proportion (0-1),
#'   otherwise as percentage (0-100). Default: FALSE
#' @param symbol Logical, if TRUE appends "%" symbol. Default: TRUE
#'
#' @return Character formatted percentage
#' @export
#'
#' @examples
#' format_percentage(23.5)
#' # [1] "23.5%"
#'
#' format_percentage(0.235, is_proportion = TRUE)
#' # [1] "23.5%"
#'
#' format_percentage(23.5, locale = "de")
#' # [1] "23,5%"
format_percentage <- function(
	x,
	digits = 1L,
	locale = get_locale(),
	trim = FALSE,
	na_string = getOption("pharmhand.na_string", "NA"),
	is_proportion = FALSE,
	symbol = TRUE
) {
	if (length(x) == 0) {
		return(character())
	}

	# Convert proportion to percentage if needed
	if (is_proportion) {
		x <- x * 100
	}

	result <- format_number(x, digits, locale, trim, na_string)

	if (symbol) {
		result <- ifelse(result == na_string, result, paste0(result, "%"))
	}

	result
}
