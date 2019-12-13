library(heatmaply)
load( here("data", "cleaned.rdata"))

rsc = t2[match(colnames(conf_cor), t2$var), c("t1_alt", "t1_var"), drop = FALSE]
names(rsc) = c("Alternate Tag Category", "Associated General Approach")

conf_cor_hm = conf_cor
diag(conf_cor_hm) = NA


heatmaply_cor(
  conf_cor_hm,
  showticklabels = c(F, F),
  main = "Correlation heatmap for Tier 2 Tags",
  hclust_method = "ward.D2",
  show_dendrogram = c(T, F),
  row_side_colors = rsc,
  file = here("reporting", "tier2_tags_heatmap.html")
)
