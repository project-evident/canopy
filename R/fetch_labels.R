library(googlesheets4)

tag_labels = sheets_read("https://docs.google.com/spreadsheets/d/1aHNNETwbPiQ_nJJlDzgwx9hHcE_0t_RzvucVeNjgjAg/edit?ts=5e8f7424#gid=0")

names(tag_labels) = c("tag", "description", "label")

tag_labels = filter(tag_labels, !is.na(label))

if(nrow(tag_labels) != 88) stop("Wrong number of labels imported!")

fix_accent_a = function(x) {
  x = str_replace_all(x, "Ã", "à")
  x = str_replace_all(x, "\\s+", " ")
  x
}

tag_labels = mutate_all(tag_labels, fix_accent_a)

readr::write_tsv(tag_labels, "data/tag_labels.tsv")
