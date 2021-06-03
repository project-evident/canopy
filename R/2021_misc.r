library(tidyverse)
library(psych)
library(cluster)
library(parameters)

source("R/import_tags.r")
source("R/import_all.r")
source("R/branding.R")


tags = import_tags()
tag_vec = tags$tag


out_folder = "reporting/2021Q1/misc/"
canopy = readRDS(most_recent_file("canopy_all", path = "data/")) %>%
  group_by(school_id) %>%
  slice_max(year) %>% ## use most recent response per school
  ungroup %>%
  mutate(across(any_of(tag_vec), ~ as.integer(!is.na(.))))


# violin plots of demographics ####

violin_dems = c("FRPL_percent", "LEP_percent", "IDEA_percent", "non_white_percent")

canopy %>%
  filter(year %in% c("2020", "2021")) %>%
  mutate(non_white_percent = 1 - white_percent) %>%
  select(violin_dems, school_id) %>%
  pivot_longer(
    cols = all_of(violin_dems),
    names_to = "Demographic"
  ) %>%
  ggplot(aes(x = Demographic, y = value)) +
  geom_violin(fill = cc_cols["light blue"]) +
  geom_point(color = "gray40", size = 0.8, position = position_jitter(width = 0.05)) +
  bar_y_scale_percent +
  scale_x_discrete(labels = label_dems) +
  labs(
    y = "Percent of student body",
    title = "Distribution of demographics in\nCanopy schools (2020-2021)",
    x = ""
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))
  ) -> violin_dem_plot

ggsave_cc(violin_dem_plot, file = "violin plots of demographics", dir = out_folder, write_data = FALSE)



# EFA (or confirmatory?) on tags from all 222 schools ####

clust_tags = tags %>% filter(coalesce(cluster, "General") != "COVID-19") %>% pull(tag)

canopy_cluster = function(
  data,
  clust_tags,
  n_clust = 5,
  filename,
  out_folder
) {
  cor_data = data %>% 
    select(any_of(clust_tags)) %>% 
    cor(use = "pairwise.complete.obs")
  
  fa_result = fa(cor_data, nfactors = n_clust, rotate = "oblimin", fm = "minres")
  
  if(!missing(filename)) {
    fa_result %>%
      model_parameters(sort = TRUE, threshold = "max") %>%
      write_tsv(paste0(out_folder, filename, "Max.txt"), na = "")
    fa_result %>%
      model_parameters(sort = TRUE, threshold = 0.28) %>%
      write_tsv(paste0(out_folder, filename, "Threshold.txt"), na = "")
    fa_result %>%
      model_parameters(sort = TRUE) %>%
      write_tsv(paste0(out_folder, filename, "All.txt"), na = "")
  }
  
  invisible(fa_result)
}

canopy_cluster(
  data = canopy,
  clust_tags = clust_tags,
  n_clust = 5,
  filename = "EFA 2019-2021 5 clusters",
  out_folder = out_folder 
)

canopy_cluster(
  data = canopy,
  clust_tags = clust_tags,
  n_clust = 6,
  filename = "EFA 2019-2021 6 clusters",
  out_folder = out_folder 
)

canopy_cluster(
  data = canopy,
  clust_tags = clust_tags,
  n_clust = 7,
  filename = "EFA 2019-2021 7 clusters",
  out_folder = out_folder 
)


# representation of how much changed from 2019 to 2020/21 ####




# last priority - representation of how much of a difference there is between
# nominator Tier 1 tags and school Tier 1 tags (only for 2020 and 2021 data,
# where schools did not see nominator tags before submitting their own)