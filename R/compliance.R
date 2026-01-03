# =============================================================================
# G-BA Compliance Checks
# =============================================================================

#' Check G-BA Compliance for Tables
#'
#' Validates tables before export to ensure basic G-BA Module 4 compliance.
#' This function checks structural requirements like column names, presence of
#' header/body rows, and applied G-BA theme markers.
#'
#' @param x A ClinicalTable, flextable, data.frame, or list of these
#' @param strict Logical, if TRUE throws an error on non-compliance
#' @param require_theme Logical, require G-BA theme marker (default: TRUE)
#' @param require_title Logical, require a title line (default: FALSE)
#' @param require_header Logical, require a header row in flextable
#'   (default: TRUE)
#' @param require_body Logical, require a body row in flextable (default: TRUE)
#' @param require_unique_colnames Logical, require unique column names
#'   (default: TRUE)
#' @param require_nonempty_colnames Logical, require non-empty column names
#'   (default: TRUE)
#'
#' @return A list with `ok`, `errors`, and `warnings`
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Statistic = "n", Value = 10)
#' check_gba_compliance(df, strict = FALSE)
#' }
check_gba_compliance <- function(
	x,
	strict = TRUE,
	require_theme = TRUE,
	require_title = FALSE,
	require_header = TRUE,
	require_body = TRUE,
	require_unique_colnames = TRUE,
	require_nonempty_colnames = TRUE
) {
	check_one <- function(obj, label = "object") {
		errors <- character()
		warnings <- character()
		data <- NULL
		ft <- NULL
		title <- NULL

		if (S7::S7_inherits(obj, ClinicalTable)) {
			data <- obj@data
			ft <- obj@flextable
			title <- obj@title
		} else if (inherits(obj, "flextable")) {
			ft <- obj
		} else if (is.data.frame(obj)) {
			data <- obj
		} else {
			errors <- c(
				errors,
				sprintf(
					"%s must be a ClinicalTable, flextable, or data.frame",
					label
				)
			)
			return(list(errors = errors, warnings = warnings))
		}

		# Data frame checks
		if (!is.null(data)) {
			col_names <- names(data)
			if (length(col_names) == 0) {
				errors <- c(errors, sprintf("%s has no columns", label))
			} else {
				if (require_nonempty_colnames) {
					empty <- is.na(col_names) | nchar(col_names) == 0
					if (any(empty)) {
						errors <- c(
							errors,
							sprintf(
								"%s has empty column names",
								label
							)
						)
					}
				}
				if (require_unique_colnames && anyDuplicated(col_names)) {
					errors <- c(
						errors,
						sprintf(
							"%s has duplicate column names",
							label
						)
					)
				}
			}
		}

		# Flextable checks
		if (!is.null(ft)) {
			if (require_header && flextable::nrow_part(ft, "header") < 1) {
				errors <- c(
					errors,
					sprintf(
						"%s has no header row",
						label
					)
				)
			}
			if (require_body && flextable::nrow_part(ft, "body") < 1) {
				errors <- c(
					errors,
					sprintf(
						"%s has no body rows",
						label
					)
				)
			}

			col_names <- if (!is.null(ft$col_keys)) {
				ft$col_keys
			} else if (!is.null(ft$body$dataset)) {
				names(ft$body$dataset)
			} else {
				character()
			}
			if (length(col_names) == 0) {
				errors <- c(errors, sprintf("%s has no columns", label))
			} else {
				if (require_nonempty_colnames) {
					empty <- is.na(col_names) | nchar(col_names) == 0
					if (any(empty)) {
						errors <- c(
							errors,
							sprintf(
								"%s has empty column names",
								label
							)
						)
					}
				}
				if (require_unique_colnames && anyDuplicated(col_names)) {
					errors <- c(
						errors,
						sprintf(
							"%s has duplicate column names",
							label
						)
					)
				}
			}

			if (require_theme) {
				theme <- attr(ft, "pharmhand_theme")
				if (is.null(theme) || theme != "gba") {
					errors <- c(
						errors,
						sprintf(
							"%s is missing G-BA theme marker",
							label
						)
					)
				}
			}
		}

		# Title checks
		if (require_title) {
			if (!is.null(title)) {
				if (nchar(title) == 0) {
					errors <- c(errors, sprintf("%s has no title", label))
				}
			} else if (!is.null(ft)) {
				if (flextable::nrow_part(ft, "header") < 2) {
					errors <- c(
						errors,
						sprintf(
							"%s has no title header row",
							label
						)
					)
				}
			}
		}

		list(errors = errors, warnings = warnings)
	}

	# Handle list input
	if (is.list(x) && !inherits(x, "flextable") && !is.data.frame(x)) {
		item_names <- names(x)
		if (is.null(item_names)) {
			item_names <- character(length(x))
		}

		results <- lapply(seq_along(x), function(i) {
			name <- item_names[i]
			obj <- x[[i]]
			label <- if (is.na(name) || nchar(name) == 0) {
				sprintf("item[[%d]]", i)
			} else {
				name
			}
			check_one(obj, label)
		})

		errors <- unlist(lapply(results, `[[`, "errors"), use.names = FALSE)
		warnings <- unlist(lapply(results, `[[`, "warnings"), use.names = FALSE)

		ok <- length(errors) == 0
		if (strict && !ok) {
			ph_abort(paste(
				"Non-compliant with G-BA requirements:\n-",
				paste(errors, collapse = "\n- ")
			))
		}

		return(list(ok = ok, errors = errors, warnings = warnings))
	}

	# Single object
	res <- check_one(x)
	ok <- length(res$errors) == 0
	if (strict && !ok) {
		ph_abort(paste(
			"Non-compliant with G-BA requirements:\n-",
			paste(res$errors, collapse = "\n- ")
		))
	}

	list(ok = ok, errors = res$errors, warnings = res$warnings)
}
