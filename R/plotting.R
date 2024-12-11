#' @title Useful plotting libraries
#'

require(patchwork)
require(ggpattern)
require(gridExtra)
require(GGally)
require(Cairo)
require(gg.layers)
require(wesanderson)
require(paletteer)
require(ggsignif)

#' Customised ggplot theme
#'
#' @description Customised ggplot theme based on theme_minimal()
#'
#' @param text_size font text size in pt
#'
#' @return theme that can be added to ggplot
#'


theme_custom <- function(text_size = 8) {
  theme_minimal() %+replace%
    theme(
      text = element_text(size = text_size),
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(
        size = text_size + 2,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(0, 0, 5, 0)
      ),
      strip.text = element_text(size = text_size),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.ticks = element_line(),
      legend.text = element_text(size = 8)
    )
}

#' @title Save plots
#'
#' @description Save and print plots as pdf eps or tikz file.
#'
#' @param pl ggplot
#' @param pdf_file filepath to pdf
#' @param eps_file filepath to eps
#' @param tikz_file filepath to tikz
#' @param w width in cm
#' @param h height in cm
#'
#' @return NULL (save and print plot)
#'

save_plot <- function(
    pl,
    pdf_file = NULL, eps_file = NULL, tikz_file = NULL,
    w = 8, h = 4) {
  print(pl)
  if (!is.null(pdf_file)) {
    ggsave(pdf_file, width = w / cm(1), height = h / cm(1))
  }
  if (!is.null(tikz_file)) {
    tikz(tikz_file, width = w / cm(1), height = h / cm(1))
    print(pl)
    dev.off()
  }
  if (!is.null(eps_file)) {
    cairo_ps(filename = eps_file, width = w / cm(1), height = h / cm(1))
  }
}
