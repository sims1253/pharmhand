#' @title Plotting Common Utilities
#' @name plotting_common
#' @description Common utilities and themes for clinical plots.
NULL

#' @keywords internal
# Reordered Okabe-Ito palette for clinical plots
# Skips black (#000000) as first color, prioritizes
# more visually distinct colors
# Original order: black, orange, sky blue, bluish green,
# yellow, blue, vermillion, reddish purple
# Reordered: orange, sky blue, bluish green, blue,
# vermillion, reddish purple, yellow, black
.PH_DEFAULT_PALETTE <- c(
	"#E69F00", # orange
	"#56B4E9", # sky blue
	"#009E73", # bluish green
	"#0072B2", # blue
	"#D55E00", # vermillion
	"#CC79A7", # reddish purple
	"#F0E442", # yellow
	"#000000" # black (least useful for clinical plots)
)

#' Resolve color palette from parameter or options
#' @keywords internal
.resolve_palette <- function(palette = NULL) {
	if (!is.null(palette)) {
		return(palette)
	}

	opt_palette <- getOption("pharmhand.palette", default = NULL)
	if (is.null(opt_palette)) {
		return(.PH_DEFAULT_PALETTE)
	}

	if (is.character(opt_palette) && length(opt_palette) == 1) {
		tryCatch(
			grDevices::palette.colors(n = NULL, palette = opt_palette),
			error = function(e) {
				ph_warn(
					paste0("Palette '", opt_palette, "' not found, using default")
				)
				.PH_DEFAULT_PALETTE
			}
		)
	} else if (is.character(opt_palette) && length(opt_palette) > 1) {
		opt_palette
	} else {
		.PH_DEFAULT_PALETTE
	}
}

#' Create pharmhand plotting theme with consistent white background
#' @param base_size Base font size for plot text elements (default: 11)
#' @keywords internal
.pharmhand_theme <- function(base_size = 11) {
	ggplot2::theme_minimal(base_size = base_size) +
		ggplot2::theme(
			panel.background = ggplot2::element_rect(fill = "white", color = NA),
			plot.background = ggplot2::element_rect(fill = "white", color = NA),
			panel.border = ggplot2::element_rect(fill = NA, color = "gray90"),
			panel.grid.major = ggplot2::element_line(color = "gray90"),
			panel.grid.minor = ggplot2::element_blank(),
			legend.background = ggplot2::element_rect(
				fill = "white",
				color = "gray90"
			),
			legend.key = ggplot2::element_rect(fill = "white", color = NA),
			legend.position = "bottom"
		)
}
