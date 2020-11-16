label_percent_bins = function(breaks) {
  n = length(breaks)
  if(n < 3) stop("Need at least 3 breaks!")
  lo = glue("<{scales::percent_format()(breaks[2])}")
  hi = glue(">{scales::percent_format()(breaks[n - 1])}")
  med = NULL
  if(n > 3) {
    med = paste(
      scales::percent_format()(breaks[2:(n - 2)]),
      scales::percent_format()(breaks[3:(n - 1)]),
      sep = "-"
    )
  }
  return(c(lo, med, hi))
}

# label_percent_bins(seq(0, 1, .25))
# label_percent_bins(seq(0, 1, length.out = 3))
# label_percent_bins(seq(0, 1, length.out = 6))
