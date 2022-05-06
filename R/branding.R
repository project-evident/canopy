library(extrafont)
library(ggplot2)
library(ggthemes)
library(glue)

source("R/label_tags.R")
source("R/label_dems.R")
source("R/label_percent_bins.R")


# font Gill Sans Light

# theme_cc = theme_gdocs(base_size = 16, base_family = "Lato Light") +
#   theme(
#     plot.title = element_text(family = "Freight"),
#     plot.background = element_blank(),
#     axis.text = element_text(colour = "black"),
#     axis.title = element_text(colour = "black"),
#     panel.border = element_rect(colour = "gray40"),
#     strip.text = element_text(size = rel(0.8))
#   )
# 
# theme_cc_few = theme_few(base_size = 16, base_family = "Lato Light") +
#   theme(plot.title = element_text(family = "Freight"),
#         axis.text = element_text(colour = "black"))

theme_cc = theme_gdocs(base_size = 16) +
  theme(
    #plot.title = element_text(family = "Freight"),
    plot.background = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    panel.border = element_rect(colour = "gray40"),
    strip.text = element_text(size = rel(0.8))
  )

theme_cc_few = theme_few(base_size = 16) +
  theme(
    #plot.title = element_text(family = "Freight"),
    axis.text = element_text(colour = "black")
  )


theme_set(theme_cc)
fig_width = 11
fig_height = 7
# create color palettes

cc_cols = c(
  green = "#8BBA26",
  red = "#E73C36",
  `light blue` = "#36C7DE",
  purple = "#9541F1",
  `dark blue` = "#1F3A56")
cc_gray = "#EAEAEA"

locale_cols = c(cc_cols["dark blue"], cc_cols["green"], cc_cols["light blue"])
names(locale_cols) = c("Urban", "Suburban", "Rural")
scale_fill_locale = scale_fill_manual(values = locale_cols)
scale_color_locale = scale_color_manual(values = locale_cols)

hs_cols = cc_cols[c("purple", "green")] %>% setNames(c("High School", "Not High School"))

scale_fill_charter = scale_fill_manual(
  values = unname(cc_cols[c("green", "dark blue")]),
  labels = c("Yes" = "Charter", "No" = "Traditional"),
  na.value = "gray50"
)

scale_fill_type = scale_fill_manual(
  values = unname(cc_cols[c("green", "dark blue", "purple")]),
  labels = c("Charter", "District", "Independent")
)

bar_y_scale_count = 
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1)),
    breaks = scales::breaks_extended(Q = c(1, 5, 2, 4, 3))
) 

bar_y_scale_percent = 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) 

bar_x_scale_percent = 
  scale_x_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) 

bar_theme = theme(panel.grid.major.x = element_blank())


scale_fill_cc_gradient = scale_fill_gradient2(
      limits = c(-1, 1),
      expand = c(0, 0), 
      low = cc_cols["dark blue"], 
      mid = "white",
      high = cc_cols["green"],
      midpoint = 0
    )

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

geom_col = function(...) ggplot2::geom_col(..., width = 0.6)




scale_fill_level = function(...) {
    scale_fill_manual(..., values = cc_cols[c("light blue", "green", "purple")] %>% set_names("elementary", "middle", "high"))
}


clust_5_order = c(
  "Blended Learning",
  "Project-Based Learning",
  "Competency-Based Education",
  "Equity & Social-Emotional Learning",
  "Flexible Pathways to College & Career"
)


cut_label = function(x) {
  to_mod = str_detect(x, "[\\(\\)\\[\\]]")
  
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


ggsave_cc = function(plot, file, dir, fig_width = 9, fig_height = 7, write_data = TRUE) {
  for (ext in c("png", "svg")) {
    ggsave(filename = sprintf("%s/%s.%s", dir, file, ext),
           plot = plot,
           width = fig_width, height = fig_height)
  }
  if(write_data) write_csv(plot$data, file = sprintf("%s/%s_data.csv", dir, file))
}


