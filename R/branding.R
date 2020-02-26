library(extrafont)
library(ggplot2)
library(ggthemes)

# font Gill Sans Light
theme_set(theme_gdocs(base_size = 16, base_family = "GillSans-Light"))
fig_width = 9

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





