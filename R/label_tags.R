library(readr)

tag_labels = read_tsv("data/tag_labels.tsv")

tag_labels_2020 = read_csv("data/Canopy Tags Public Access.csv") %>%
  select(tag = `Variable name`, label = `Tag name`)

tag_labels = full_join(tag_labels, tag_labels_2020) %>%
  arrange(tag, description) %>%
  filter(!duplicated(tag))

label_tags = function(x, capitalize = "none") {
  if(any(!x %in% tag_labels$tag)) warning("Missing tag label")
  labels = tag_labels$label[match(x, tag_labels$tag)]
  labels[is.na(labels)] = x[is.na(labels)]
  if(capitalize == "title") {
    labels = str_to_title(labels)
  }
  if(capitalize == "first") {
    labels = str_to_sentence(labels)
  }
  return(labels)
}


scale_x_tag = function(...) scale_x_discrete(labels = label_tags)
scale_y_tag = function(...) scale_y_discrete(labels = label_tags)
