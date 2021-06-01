library(tidyverse)

library(psych)
library(cluster)
#library(igraph)
library(parameters)



source("R/import_tags.r")
source("R/import_all.r")
source("R/branding.R")


tags = import_tags()
tag_vec = tags$tag


out_folder = "reporting/2021Q1/blog3-hs/"
canopy = readRDS(most_recent_file("canopy_all", path = "data/")) %>%
  group_by(school_id) %>%
  slice_max(year) %>% ## use most recent response per school
  ungroup %>%
  mutate(across(any_of(tag_vec), ~ as.integer(!is.na(.))))
  

## hand-coded high school from Chelsea
## run this for initial import, otherwise use the saved tsv file
if(file.exists("data/high_school_extras.tsv")) {
  hs_extras = read_tsv("data/high_school_extras.tsv")
} else{
  library(googlesheets4)
  sheet_id = "1U5YQlRAJLpjBe5cUweS002qCDynFcgkPddFn8lCcu3E"
  tabs = c("Fall 2020 School Data", "Winter 2020-21 School Data")
  hs_extras = lapply(tabs, read_sheet, ss = sheet_id, col_types = "c", na = c("", "-"))
  names(hs_extras) = c("2020", "2021")
  hs_extras = bind_rows(hs_extras, .id = "year")
  hs_extras = select(hs_extras, school_id, `Add to HS pool?`)
  if(anyDuplicated(hs_extras$school_id)) stop("Deduplicate hs_extras!")
  ## no duplicates in this now, but if made more general check for them!
  write_tsv(hs_extras, "data/high_school_extras.tsv")
}

canopy = canopy %>% left_join(hs_extras, by = "school_id")

canopy = canopy %>% 
  mutate(is_hs = `12_grade` == "Yes" | `Add to HS pool?` == "Yes")

hs = canopy %>% 
  filter(is_hs)
  
hs_tags = tags %>% filter(coalesce(cluster, "General") != "COVID-19") %>% pull(tag)
hs_tag_data = hs %>% select(any_of(hs_tags))
(hs_tag_counts = colSums(hs_tag_data) %>% sort)
## 
      #     lab_rotation          dual_language    individual_rotation       families_leaders 
      #                9                     12                     17                     22 
      # station_rotation             a_la_carte       interoperability         sel_curriculum 
      #               26                     27                     37                     38 
## was thinking of dropping any tags too infreuqent, but these actually look reasonable
#hs_tag_data = hs_tag_data %>% select(!any_of(hs_tag_counts[hs_tag_counts < 18] %>% names))

hs_cor = hs %>% select(any_of(hs_tags)) %>% cor
  
fa_pa = fa.parallel(hs_cor, fm = "pa", fa = "fa", n.obs = nrow(hs_cor))
fa_mr = fa.parallel(hs_cor, fm = "minres", fa = "fa", n.obs = nrow(hs_cor))
## 3 clusters
### ... turned out to be useless. Moved up to 5

hs_efa_3 = fa(hs_cor, nfactors = 3, rotate = "oblimin", fm = "minres")
print(hs_efa_3, sort = T)
print(hs_efa_3$loadings, cutoff = 0.25)

hs_efa_3 %>%
    model_parameters(sort = TRUE, threshold = "max") %>%
    write_tsv(paste0(out_folder, "EFA Results Max.txt"), na = "")

hs_efa_3 %>%
    model_parameters(sort = TRUE, threshold = 0.28) %>%
    write_tsv(paste0(out_folder, "EFA Results Threshold.txt"), na = "")

hs_efa_3 %>%
    model_parameters(sort = TRUE) %>%
    write_tsv(paste0(out_folder, "EFA Results All.txt"), na = "")


hs_efa_5 = fa(hs_cor, nfactors = 5, rotate = "oblimin", fm = "minres")
print(hs_efa_5, sort = T)
print(hs_efa_5$loadings, cutoff = 0.25)

hs_efa_5 %>%
    model_parameters(sort = TRUE, threshold = "max") %>%
    write_tsv(paste0(out_folder, "HS5 EFA Results Max.txt"), na = "")

hs_efa_5 %>%
    model_parameters(sort = TRUE, threshold = 0.28) %>%
    write_tsv(paste0(out_folder, "HS5 EFA Results Threshold.txt"), na = "")

hs_efa_5 %>%
    model_parameters(sort = TRUE) %>%
    write_tsv(paste0(out_folder, "HS5 EFA Results All.txt"), na = "")

## Basic HS stats

## tags at high schools vs non high schools
canopy %>%
  mutate(is_hs = coalesce(is_hs, FALSE)) %>%
  group_by(is_hs) %>% 
  summarize(across(hs_tags, mean)) %>%
  pivot_longer(-is_hs, names_to = "tag", values_to = "prop_tagged_level") %>%
  group_by(tag) %>%
  mutate(hs_bump = prop_tagged_level[is_hs] - prop_tagged_level[!is_hs]) %>%
  arrange(desc(hs_bump), tag, is_hs) %>%
  ungroup() %>%
  mutate(
    tag = reorder(factor(tag), -hs_bump),
    hs_label = case_when(is_hs ~ "High School", !is_hs ~ "Not High School", TRUE ~ NA_character_)
  ) -> hs_vs

hs_vs_top = hs_vs %>%
  slice(1:30)

hs_vs_top_wide =
  hs_vs_top %>% select(-hs_label) %>%
  pivot_wider(names_from = is_hs, values_from = prop_tagged_level, names_prefix = "hs")
  
ggplot(hs_vs_top, aes(x = prop_tagged_level, y = tag)) +
  geom_segment(data = hs_vs_top_wide, aes(x = hsFALSE, xend = hsTRUE, yend = tag)) +
  geom_text(
    data = hs_vs_top_wide,
    aes(
      x = (hsFALSE + hsTRUE) / 2,
      label = glue("{p} spread", p = scales::percent(hs_bump, accuracy = 1))
    ),
    vjust = 0, nudge_y = 0.15
  ) +
  geom_point(aes(color = hs_label), size = 3) +
  scale_y_discrete(limits = rev, labels = label_tags) +
  bar_x_scale_percent +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = c(.75, .65),
    plot.margin = margin(t = 8, r = 20, b = 8, l = 20, unit = "pt")
  ) +
  scale_color_manual(values = hs_cols) +
  labs(
    x = "Percent of schools tagged, by level",
    y = "",
    color = "",
    title = "Practices more commmon\nin high schools"
  ) -> hs_tag_plot

ggsave_cc(hs_tag_plot, file = "hs_exclusive_tags", dir = out_folder, fig_height = 9)

## use 5 clusters
### look at school characteristics by cluster alignment for each cluster
### school size -> hypothesis, correlated with MR1
### also look at locale, FRL, non-white %

hs_efa_5 %>%
  model_parameters(sort = TRUE, threshold = 0.28) %>%
  select(-Complexity, -Uniqueness) %>%
  #mutate(across(where(is.numeric), sign)) %>%
  pivot_longer(starts_with("MR"), values_drop_na = TRUE) %>%
  pivot_wider(names_from = "Variable", values_fill = 0) ->
  efa_clusts

clust_tags = names(efa_clusts)[-1]
clust_names = efa_clusts$name
for(i in 1:nrow(efa_clusts)) {
  hs[[efa_clusts$name[i]]] = rowSums(hs[clust_tags] * efa_clusts[rep(i, nrow(hs)), -1])
}

## echoing blog 1, we'll look at row correlations with the clusters
hs$non_white_percent = 1 - hs$white_percent

hs = hs %>%
  mutate(
    Urban = locale == "Urban",
    Rural = locale == "Rural",
    Suburban = locale == "Suburban"
  )

clust_dems = c("student_count", "FRPL_percent", "non_white_percent", "Urban", "Suburban", "Rural")

hs %>% select(all_of(clust_names) | all_of(clust_dems)) %>%
  cor(use = "pairwise.complete.obs") -> hs_clust_cor

hs_clust_cor = hs_clust_cor[clust_names, clust_dems]

n_clust = 5
hs_clust_cor_long = hs_clust_cor %>% 
  as.data.frame %>%
  mutate(cluster = row.names(.)) %>%
  pivot_longer(-cluster)

labeler_5clust = function(x) {
  case_when(
    x == "MR1" ~ "Projects/Real World",
    x == "MR2" ~ "Equity & Social Justice",
    x == "MR3" ~ "Instruction Techniques",
    x == "MR4" ~ "College & Career Prep",
    x == "MR5" ~ "Misc.",
    TRUE ~ x
  )
}

hs_clust_cor_long %>%
  ggplot(aes(x = name, y = cluster, fill = value)) +
  geom_tile() +
  scale_x_discrete(expand = c(0, 0), limits = clust_dems, labels = label_dems) +
  geom_text(aes(label = scales::label_number(accuracy = 0.01)(value)), color = "gray20", size = 3.5) +
  scale_fill_cc_gradient +
  scale_y_discrete(expand = c(0, 0), labels = labeler_5clust) +
  coord_equal() +
  labs(
    title = sprintf(
      "Correlation between HS practice clusters\n and school demographics",
      n_clust
    ),
    fill = "Correlation",
    x = "Demographic/School Characteristic",
    y = ""
  ) +
  theme_cc_few +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    panel.grid = element_blank()
  ) -> hs_clust_plot

ggsave_cc(hs_clust_plot, file = "hs_cluster_demographics", dir = out_folder)



hs %>% select(school_id, school_name, all_of(clust_names)) %>%
  pivot_longer(all_of(clust_names), names_to = "Cluster", values_to = "Clust_Score") %>%
  mutate(Cluster_Label = labeler_5clust(Cluster)) %>%
  group_by(Cluster) %>%
  arrange(desc(Clust_Score), school_id) %>%
  #slice_head(n = 10) %>%
  mutate(rank_within_cluster = dense_rank(desc(Clust_Score))) %>%
  filter(rank_within_cluster <= 3) %>%
  ungroup() %>%
  select(Cluster, Cluster_Label, rank_within_cluster, school_name, school_id, Clust_Score) %>%
  write_tsv(file = paste0(out_folder, "top schools for each cluster.tsv"))

  # Try clustering schools??