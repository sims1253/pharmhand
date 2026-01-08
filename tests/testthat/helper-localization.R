# Helper functions for localization tests

#' Reset locale state to default
#'
#' Resets the locale and custom translations to their initial state.
#' Useful for ensuring tests don't interfere with each other.
reset_locale_state <- function() {
	set_locale("en")
	reset_custom_translations()
}
