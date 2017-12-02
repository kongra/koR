# Copyright (c) Konrad Grzanek
# Created 2015-09-10

#' @import ggplot2
#' @import scales
#' @import grid
#' @import RColorBrewer
NULL

#' Custom pleasant theme from: http://minimaxir.com/2015/02/ggplot-tutorial/
#' @export
brewerTheme <- function(paletteName = "Greys", textSize = 7) {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal(paletteName, n = 9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text  = palette[6]
  color.axis.title = palette[7]
  color.title      = palette[9]

  # Begin construction of chart
  theme_bw(base_size = 9) +

  # Set the entire chart region to a light gray color
  theme(panel.background = element_rect(fill  = color.background,
                                        color = color.background)) +
  theme(plot.background  = element_rect(fill  = color.background,
                                        color = color.background)) +
  theme(panel.border     = element_rect(color = color.background)) +

  # Format the grid
  theme(panel.grid.major = element_line(color = color.grid.major, size = .25)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks       = element_blank()) +

  # Format the legend, but hide by default
  theme(legend.position   = "none") +
  theme(legend.background = element_rect(fill = color.background)) +
  theme(legend.text       = element_text(size = textSize, color = color.axis.title)) +

  # Set title and axis labels, and format these and tick marks
  theme(plot.title   = element_text(color = color.title, size = textSize + 2,
                                    vjust = 1.25)) +
  theme(axis.text.x  = element_text(size  = textSize, color = color.axis.text)) +
  theme(axis.text.y  = element_text(size  = textSize, color = color.axis.text)) +
  theme(axis.title.x = element_text(size  = textSize + 1 ,color = color.axis.title,
                                    vjust = 0)) +
  theme(axis.title.y = element_text(size  = textSize + 1, color = color.axis.title,
                                    vjust = 1.25)) +

  # Plot margins
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#' Theme compatible with LaTeX as much as possible.
#' @export
latexTheme <- function() {
  theme_bw() +
  theme(text = element_text(family = "CM Roman"))
}
