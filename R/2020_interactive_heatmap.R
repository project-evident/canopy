library(heatmaply)

conf_cor_2020 = logistic_data %>% select(any_of(tag_vec)) %>% cor(use = "pairwise.complete")
diag(conf_cor_2020) = NA


heatmaply_cor(
  conf_cor_2020,
  showticklabels = c(F, F),
  main = "Correlation heatmap for Fall 2020",
  hclust_method = "ward.D2",
  show_dendrogram = c(T, F),
  label_names = c("row", "column", "correlation"),
  label_format_fun = function(...) round(..., digits = 2),
  file = "reporting/2020/2020 Interactive Correlation Heatmap.html"
)

file.copy(from = "reporting/2020/2020 Interactive Correlation Heatmap.html",
          to = "G:\\Shared drives\\Proj Evident - Clay Christensen Institute\\Fall 2020\\",
          overwrite = TRUE)

