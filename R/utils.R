#' Format numbers with specified decimal places
#'
#' Formats numeric values with a specified number of decimal places.
#'
#' @param x Numeric vector to format.
#' @param digits Integer specifying the number of decimal places. Default is 2.
#'
#' @return Character vector with formatted numbers.
#'
#' @export
#'
#' @examples
#' format_number(45.2345, digits = 2) # Returns "45.23"
#' format_number(c(1.5, 2.345, 3.6789), digits = 1) # Returns c("1.5", "2.3", "3.7")
format_number <- function(x, digits = 2) {
  formatted <- formatC(x, format = "f", digits = digits)
  return(formatted)
}

#' Format values as percentages
#'
#' Formats numeric values as percentages with specified decimal places.
#'
#' @param x Numeric vector to format (values should be in decimal form, e.g., 0.5 for 50%).
#' @param digits Integer specifying the number of decimal places. Default is 1.
#'
#' @return Character vector with formatted percentages.
#'
#' @export
#'
#' @examples
#' format_percentage(0.5, digits = 1) # Returns "50.0%"
#' format_percentage(c(0.123, 0.456, 0.789), digits = 1) # Returns c("12.3%", "45.6%", "78.9%")
format_percentage <- function(x, digits = 1) {
  formatted <- paste0(formatC(x * 100, format = "f", digits = digits), "%")
  return(formatted)
}

#' Format confidence intervals
#'
#' Formats lower and upper bounds of a confidence interval into a string.
#'
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param digits Integer specifying the number of decimal places. Default is 2.
#' @param separator Character string to use as separator. Default is ", ".
#'
#' @return Character vector with formatted confidence intervals.
#'
#' @export
#'
#' @examples
#' format_ci(1.23, 4.56, digits = 2) # Returns "1.23, 4.56"
#' format_ci(c(1.23, 5.67), c(4.56, 8.90), digits = 1) # Returns c("1.2, 4.6", "5.7, 8.9")
format_ci <- function(lower, upper, digits = 2, separator = ", ") {
  formatted <- paste0(
    formatC(lower, format = "f", digits = digits),
    separator,
    formatC(upper, format = "f", digits = digits)
  )
  return(formatted)
}

#' Format p-values
#'
#' Formats p-values according to common scientific conventions.
#' Values less than 0.001 are formatted as "<0.001".
#'
#' @param p Numeric vector of p-values.
#' @param digits Integer specifying the number of decimal places for values >= 0.001. Default is 3.
#' @param threshold Numeric value below which p-values are formatted as
#' "<threshold". Default is 0.001.
#'
#' @return Character vector with formatted p-values.
#'
#' @export
#'
#' @examples
#' format_pvalue(0.0005, digits = 3) # Returns "<0.001"
#' format_pvalue(0.0234, digits = 3) # Returns "0.023"
#' format_pvalue(c(0.0005, 0.0234, 0.4567), digits = 3)
#' # Returns c("<0.001", "0.023", "0.457")
format_pvalue <- function(p, digits = 3, threshold = 0.001) {
  formatted <- ifelse(
    p < threshold,
    paste0("<", format(threshold, scientific = FALSE)),
    formatC(p, format = "f", digits = digits)
  )
  return(formatted)
}
