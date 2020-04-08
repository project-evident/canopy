library(extrafont)
library(ggplot2)
library(ggthemes)

# font Gill Sans Light
theme_cc = theme_gdocs(base_size = 16, base_family = "Lato Light") +
  theme(plot.title = element_text(family = "Freight"),
        plot.background = element_blank())

theme_cc_few = theme_few(base_family = "Lato Light") +
  theme(plot.title = element_text(family = "Freight"),
        axis.text = element_text(colour = "black"))

theme_set(theme_cc)
fig_width = 9
fig_height = 7
# create color palettes

cc_cols = c(
  green = "#8BBA26",
  red = "#E73C36",
  `light blue` = "#36C7DE",
  purple = "#9541F1",
  `dark blue` = "#1F3A56")
cc_gray = "#EAEAEA"

cc_cols_accessor = function(...) {
  cols = c(...)
  if(is.null(cols)) return(cc_cols)
  cc_cols[cols]
}

cc_palettes = list(
  main = cc_cols_accessor(c("green", "red", "light blue", "purple", "dark blue"))
)

cc_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- cc_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}



## Reordering within facets
## from: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R

#' Reorder an x or y axis within facets
#'
#' Reorder a column before plotting with faceting, such that the values are ordered
#' within each facet. This requires two functions: \code{reorder_within} applied to
#' the column, then either \code{scale_x_reordered} or \code{scale_y_reordered} added
#' to the plot.
#' This is implemented as a bit of a hack: it appends ___ and then the facet
#' at the end of each string.
#'
#' @param x Vector to reorder.
#' @param by Vector of the same length, to use for reordering.
#' @param within Vector of the same length that will later be used for faceting
#' @param fun Function to perform within each subset to determine the resulting
#' ordering. By default, mean.
#' @param sep Separator to distinguish the two. You may want to set this manually
#' if ___ can exist within one of your labels.
#' @param ... In \code{reorder_within} arguments passed on to \code{\link{reorder}}.
#' In the scale functions, extra arguments passed on to
#' \code{\link[ggplot2]{scale_x_discrete}} or \code{\link[ggplot2]{scale_y_discrete}}.
#'
#' @source "Ordering categories within ggplot2 Facets" by Tyler Rinker:
#' \url{https://trinkerrstuff.wordpress.com/2016/12/23/ordering-categories-within-ggplot2-facets/}
#'
#' @examples
#'
#' library(tidyr)
#' library(ggplot2)
#'
#' iris_gathered <- gather(iris, metric, value, -Species)
#'
#' # reordering doesn't work within each facet (see Sepal.Width):
#' ggplot(iris_gathered, aes(reorder(Species, value), value)) +
#'   geom_boxplot() +
#'   facet_wrap(~ metric)
#'
#' # reorder_within and scale_x_reordered work.
#' # (Note that you need to set scales = "free_x" in the facet)
#' ggplot(iris_gathered, aes(reorder_within(Species, value, metric), value)) +
#'   geom_boxplot() +
#'   scale_x_reordered() +
#'   facet_wrap(~ metric, scales = "free_x")
#'
#' @export
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


#' @rdname reorder_within
#' @export
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


#' @rdname reorder_within
#' @export
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}




