#' A clean theme for ggplot2
#' 
#' @param size_lg size of axis titles
#' @param size_sm size of all other text
#'
#' @import ggplot2
#'
#' @export
clean_theme = function(size_lg = 6, size_sm = 5) {
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = size_sm),
        axis.text.y = element_text(size = size_sm),
        axis.ticks.length.x = unit(0.15, 'lines'),
        axis.ticks.length.y = unit(0.15, 'lines'),
        axis.title.x = element_text(size = size_lg),
        axis.title.y = element_text(size = size_lg),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = size_sm),
        # strip.background = element_rect(fill = "grey90", color = "grey90",
        #                                 size = 0),
        strip.background = element_blank(),
        axis.line.y = element_line(colour = "grey50"),
        axis.line.x = element_line(colour = "grey50"),
        axis.ticks = element_line(colour = "grey50"),
        legend.position = "top",
        legend.text = element_text(size = size_sm),
        legend.title = element_text(size = size_sm),
        legend.key.size = unit(0.6, "lines"),
        legend.margin = margin(rep(0, 4)),
        # legend.box.margin = ggplot2::margin(rep(0, 4), unit = 'lines'),
        # legend.box.spacing = ggplot2::margin(rep(0, 4)),
        legend.background = element_blank(),
        plot.title = element_text(size = size_lg, hjust = 0.5),
        axis.ticks.length = unit(2, 'pt'),)
}

#' A grid theme for ggplot2
#' 
#' @param size_lg size of axis titles
#' @param size_sm size of all other text
#'
#' @import ggplot2
#'
#' @export
grid_theme = function(size_lg = 6, size_sm = 5) {
  theme_bw() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = size_sm),
          axis.text.y = element_text(size = size_sm),
          axis.ticks.length.x = unit(0.15, 'lines'),
          axis.ticks.length.y = unit(0.15, 'lines'),
          axis.title.x = element_text(size = size_lg),
          axis.title.y = element_text(size = size_lg),
          strip.text = element_text(size = size_sm),
          strip.background = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks = element_line(colour = "grey50"),
          legend.position = "top",
          legend.text = element_text(size = size_sm),
          legend.title = element_text(size = size_sm),
          legend.key.size = unit(0.6, "lines"),
          legend.margin = margin(rep(0, 4)),
          # legend.box.margin = ggplot2::margin(rep(0, 4), unit = 'lines'),
          # legend.box.spacing = ggplot2::margin(rep(0, 4)),
          legend.background = element_blank(),
          plot.title = element_text(size = size_lg, hjust = 0.5),
          axis.ticks.length = unit(2, 'pt'),)
}

#' A boxed theme for ggplot2
#' 
#' @param size_lg size of axis titles
#' @param size_sm size of all other text
#'
#' @import ggplot2
#'
#' @export
boxed_theme = function(size_lg = 6, size_sm = 5) {
  theme_bw() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = size_sm),
          axis.text.y = element_text(size = size_sm),
          axis.ticks.length.x = unit(0.15, 'lines'),
          axis.ticks.length.y = unit(0.15, 'lines'),
          axis.title.x = element_text(size = size_lg),
          axis.title.y = element_text(size = size_lg),
          panel.grid = element_blank(),
          strip.text = element_text(size = size_sm),
          strip.background = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks = element_line(colour = "grey50"),
          legend.position = "top",
          legend.text = element_text(size = size_sm),
          legend.title = element_text(size = size_sm),
          legend.key.size = unit(0.6, "lines"),
          legend.margin = margin(rep(0, 4)),
          # legend.box.margin = ggplot2::margin(rep(0, 4), unit = 'lines'),
          # legend.box.spacing = ggplot2::margin(rep(0, 4)),
          legend.background = element_blank(),
          plot.title = element_text(size = size_lg, hjust = 0.5))
}

#' A UMAP theme for ggplot2
#' 
#' @param size_lg size of axis titles
#' @param size_sm size of all other text
#' 
#' @import ggplot2
#'
#' @export
umap_theme = function(size_lg = 6, size_sm = 5) {
  boxed_theme(size_lg = size_lg, size_sm = size_sm) +
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_text(hjust = 0, vjust = 0),
          axis.title.x = element_text(hjust = 0))
}

#' Darken a color
#'
#' @param color the color to darken
#' @param factor amount of darkening to apply
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @export
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

#' Lighten a color
#'
#' @param color the color to lighten
#' @param factor amount of lightening to apply
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @export
lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  for (value in col){
    if (value > 255) {
      col[col == value] = 255
    }
  }
  col <- rgb(t(col), maxColorValue=255)
  col
}

#' Create scientific labels for ggplot
#'
#' @param l values to transform
#'
#' @export
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # remove zero
  l <- gsub("0e\\+00", "0", l)
  # remove one
  l <- gsub("^1e\\+00", "1", l)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # remove + from exponent
  l <- gsub("e\\+" ,"e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # remove 1 x 10^ (replace with 10^)
  l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
  # return this as an expression
  parse(text=l)
}

#' Plot a color palette
#' 
#' @param pal palette to plot
#'
#' @importFrom grid grid.raster
#'
#' @export
plot_pal = function(pal) {
  grid::grid.raster(pal, interpolate=F)
}

#' Winsorize a vector of data
#'
#' Winsorize a vector by clipping extreme values. If either limit is \code{NA},
#' the data will not be winsorized at that end. 
#' 
#' @param vec vector to process
#' @param limits vector of length 2; values beyond which outliers will be 
#'   winsorized
#'
#' @export
winsorize = function(vec, limits) {
  lower_limit = limits[1]
  upper_limit = limits[2]
  if (!is.na(upper_limit))
    vec[vec > upper_limit] = upper_limit
  if (!is.na(lower_limit))
    vec[vec < lower_limit] = lower_limit
  return(vec)
}
