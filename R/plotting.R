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
require(ggtext)
require(tools)

#' @title Customised ggplot theme
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
      strip.text = element_text(
        size = text_size,
        margin = margin(b = 5.5, l = 5.5, r = 5.5, t = 5.5),
        face = 2
      ),
      panel.grid.major = element_blank(),
      panel.border = element_rect(fill = NA),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.ticks = element_line(),
      legend.text = element_text(size = 8),
      panel.spacing.x = unit(.5, "cm"),
      panel.spacing.y = unit(.5, "cm"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

#' @title Save and print ggplots
#'
#' @description Convenience function to save and print ggplot objects as .pdf, .tif, or .png.
#'
#' @param pl ggplot
#' @param pdf_file filepath (specify type: "plot.pdf", "plot.tif", "plot.png")
#' @param w width in cm
#' @param h height in cm
#' @param ... additional arguments passed to ggsave
#'
#' @return NULL (save and print plot)
#'

save_plot <- function(
    pl,
    pdf_file = NULL,
    w = 8, h = 4,
    ...) {
  # print plot
  print(pl)
  # save as .tif
  if (tools::file_ext(pdf_file) == "tif") {
    ggsave(
      plot = pl,
      filename = pdf_file,
      width = w / cm(1), height = h / cm(1),
      device = "tiff",
      compress = "lzw",
      ...
    )
  } else {
    # save as .pdf or .png
    ggsave(
      plot = pl,
      filename = pdf_file,
      width = w / cm(1), height = h / cm(1)
    )
  }
}
