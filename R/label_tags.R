library(readr)

tag_labels = read_tsv("data/tag_labels.tsv")

label_tags = function(x) {
  if(any(!x %in% tag_labels$tag)) stop("Missing tag label")
  tag_labels$label[match(x, tag_labels$tag)]
}
