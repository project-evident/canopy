library(stats)
library(here)

library(flextable)

library(ggplot2)
library(forcats)

library(dplyr)
library(tidyr)

library(ggcorrplot)
library(psych)
library(cluster)
library(igraph)
library(philentropy)
library(ggdendro)
library(heatmaply)
library(parameters)

library(readr)


load(here("data", "cleaned.rdata"))

source(here("R/branding.R"))


tagdem = conf_all_long %>%
  left_join(dems, by = "school_id")

dem_cols = c(
  "black_count",
  "black_percent",
  "non_white_percent",
  "FRPL_count",
  "FRPL_percent",
  "LEP_percent",
  "IDEA_percent",
  "charter",
  "level_elem",
  "level_middle", 
  "level_high",
  "locale_urban",
  "locale_suburban",
  "locale_rural"
)
# with 145 / 156 schools being "regular school" instead of Alternative, Career and Tech, Private, will ignore the
# school_type

# 21 schools missing charter school status - worth filling in


## For charter ####
charter_dat = tagdem %>%
  filter(!is.na(charter) & charter != "Not applicable")  %>%
  group_by(tag, charter) %>%
  summarize(n_schools = n(),
            n_schools_with_tag = sum(value),
            p_school_with_tag = mean(value),
            n_students_served = sum(CCD_student_count[value == 1], na.rm = TRUE)) %>%
  group_by(tag) %>%
  mutate(diff = p_school_with_tag[charter == "Yes"] - p_school_with_tag[charter == "No"]) %>%
  arrange(desc(diff)) %>%
  ungroup()

charter_dat %>%
  group_by(tag) %>%
  summarize(diff = mean(diff)) %>%
  ggplot(aes(x = diff)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_histogram(fill = cc_cols["green"], binwidth = 0.01) +
  labs(title = "Distribution of differences in tags between\ncharter and traditional schools",
       y = "Count of tags with each difference",
       x = "Percentage points difference\n(% charter with tag minus % traditional with tag") +
  geom_vline(xintercept = 0, color = cc_cols["red"], size = 2)
ggsave("graphs/charter_diff_hist.png", width = fig_width)

charter_dat %>%
  group_by(tag) %>%
  summarize(diff = mean(diff)) %>%
  pull(diff) %>% mean


charter_dat %>%
  #slice(1:24) %>%
  filter(diff >= quantile(diff, 0.85)) %>%
  mutate(tag = fct_reorder(tag, -diff)) %>%
  ggplot(aes(x = tag, y = p_school_with_tag, fill = charter)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = unname(cc_cols[c(1, 3)]), labels = c("Yes" = "Charter", "No" = "Traditional")) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(diff, accuracy = 1)), y = Inf, vjust = 1, color = "gray20") +
  labs(title = "Tags with largest differential between\ncharter and non-charter schools\n(top 15% differences)",
       x = "Tag", 
       y = "Percent of charter/non-charter schools with tag",
       fill = "") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0),
        panel.grid.major.x = element_blank())
ggsave("graphs/charter_largest_diff_tags.png", width = fig_width)

charter_dat %>%
  arrange(diff) %>%
  filter(diff <= quantile(diff, 0.15)) %>%
  mutate(tag = fct_reorder(tag, diff)) %>%
  ggplot(aes(x = tag, y = p_school_with_tag, fill = charter)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = unname(cc_cols[c(1, 3)]), labels = c("Yes" = "Charter", "No" = "Traditional")) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(diff, accuracy = 1)), y = Inf, vjust = 1, color = "gray20") +
  labs(title = "Tags with smallest differential between\ncharter and non-charter schools\n(bottom 15% differences)",
       x = "Tag", 
       y = "Percent of charter/non-charter schools with tag",
       fill = "") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0),
        panel.grid.major.x = element_blank()) 
ggsave("graphs/charter_smallest_diff_tags.png", width = fig_width)

# number of students served
charter_dat %>%
  #slice(1:24) %>%
  filter(diff >= quantile(diff, 0.85)) %>%
  mutate(tag = fct_reorder(tag, -diff)) %>%
  ggplot(aes(x = tag, y = n_students_served, fill = charter)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = unname(cc_cols[c(1, 3)]), labels = c("Yes" = "Charter", "No" = "Traditional")) +
  scale_y_continuous(labels = scales::comma_format()) +
  #geom_text(aes(label = n_schools_with_tag, group = charter), y = 0, position = position_dodge(width = 1), vjust = 0) +
  geom_text(aes(label = scales::percent(diff, accuracy = 1)), y = Inf, vjust = 1, color = "gray20") +
  labs(title = "Even when more charter schools have tags\ntraditional schools serve many more students",
       x = "Tag", 
       y = "Number of students served by school type",
       fill = "") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0),
        panel.grid.major.x = element_blank())  
ggsave("graphs/charter_largest_diff_tags_n.png", width = fig_width)


dems %>%
  filter(!is.na(charter) & charter != "Not applicable")  %>%
  group_by(charter) %>%
  summarize(race_count_missing = sum(is.na(total_race_count)), CCD_count_missing = sum(is.na(CCD_student_count)))
## only a few schools missing


## locale ####

locale_dat = tagdem %>%
  filter(!is.na(locale))  %>%
  group_by(tag, locale) %>%
  summarize(n_schools = n(),
            n_schools_with_tag = sum(value),
            p_school_with_tag = mean(value),
            n_students_served = sum(CCD_student_count[value == 1], na.rm = TRUE)) %>%
  group_by(tag) %>%
  mutate(sd = sd((p_school_with_tag))) %>%
  arrange(desc(sd)) %>%
  ungroup()


locale_dat %>%
  group_by(tag) %>%
  summarize(sd = mean(sd)) %>%
  summary

locale_dat %>%
  group_by(tag) %>%
  summarize(sd = mean(sd)) %>%
  ggplot(aes(x = sd)) +
  geom_histogram(fill = cc_cols["green"], binwidth = 0.01) +
  labs(title = "Distribution of differences in tags between locales",
       y = "Count of tags with each difference",
       x = "Percentage points difference\n(% charter with tag minus % traditional with tag") +
  geom_vline(xintercept = 0, color = cc_cols["red"], size = 2)
#ggsave("graphs/locale_diff_hist.png")

locale_dat %>%
  #slice(1:24) %>%
  filter( sd >= quantile(sd, 0.85)) %>%
  mutate(tag = fct_reorder(tag, -sd)) %>%
  ggplot(aes(x = tag, y = p_school_with_tag, fill = locale)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = unname(cc_cols[c(1, 2, 3)])#, labels = c("Yes" = "Charter", "No" = "Traditional")
                    ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  geom_text(aes(label = round(sd, 2)), y = Inf, vjust = 1, color = "gray20") +
  labs(title = "Tags with largest std deviation between locales\n(highest 15% std deviations)",
       x = "Tag", 
       y = "Percent of schools with tag",
       fill = "") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0),
        panel.grid.major.x = element_blank())
ggsave("graphs/locale_largest_sd_tags.png", width = fig_width)

locale_dat %>%
  arrange(sd) %>%
  filter(sd <= quantile(sd, 0.15)) %>%
  mutate(tag = fct_reorder(tag, sd)) %>%
  ggplot(aes(x = tag, y = p_school_with_tag, fill = locale)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = unname(cc_cols[c(1,2, 3)])) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = round(sd, 2)), y = Inf, vjust = 1, color = "gray20") +
  labs(title = "Tags with smallest std deviation\n(lowest 15% std deviations)",
       x = "Tag", 
       y = "Percent of schools with tag",
       fill = "") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0),
        panel.grid.major.x = element_blank()) 
ggsave("graphs/locale_smallest_sd_tags.png")


# # number of students served
## Not so interesting for locale  73 urban schools, 31 suburban, 39 rural
# locale_dat %>%
#   #slice(1:24) %>%
#   filter(sd >= quantile(sd, 0.85)) %>%
#   mutate(tag = fct_reorder(tag, -sd)) %>%
#   ggplot(aes(x = tag, y = n_students_served, fill = locale)) +
#   geom_col(position = "dodge") +
#   scale_fill_manual(values = unname(cc_cols[c(1, 2, 3)]), labels = c("Yes" = "Charter", "No" = "Traditional")) +
#   scale_y_continuous(labels = scales::comma_format()) +
#   #geom_text(aes(label = scales::percent(sd, accuracy = 1)), y = Inf, vjust = 1, color = "gray20") +
#   labs(title = "Even when charter schools have a higher percentage of tags\ntraditional schools serve many more students",
#        x = "Tag", 
#        y = "Number of students served by school type",
#        fill = "") +
#   theme(axis.text.x = element_text(angle = -40, hjust = 0),
#         panel.grid.major.x = element_blank())  
# #ggsave("graphs/locale_largest_diff_tags_n.png")


## GET e5 from efa.R
tagdem = tagdem %>% left_join(e5)

clust = reshape2::dcast(tagdem, school_id ~ Cluster, fun.aggregate = sum, value.var = "value")
clustdem = clust %>% left_join(dems)

with(clustdem, cor(Equity, black_percent, use = "pairwise.complete.obs"))


efa5_key = c(
  "MR3" = "Social-Emotional Learning & Equity",
  "MR1" = "Project-based Learning",
  "MR5" = "Flexible Pathways to College & Career",
  "MR4" = "Assessment & Student Success",
  "MR2" = "Blended Learning"
)

clust_names = unname(efa5_key)
#dem_names = setdiff(dem_cols)

clust_cors = clustdem %>%
  select(clust_names, dem_cols) %>%
  mutate(charter = ifelse(charter == "Yes", 1, 0)) %>%
  cor(use = "pairwise.complete.obs")

clust_cors = clust_cors[clust_names, dem_cols]

## add locale
## add non-white %
## one-hot encode level
## six-clust side by side

# Tag differences
## SPED/IDEA
## ELL

clust_cor_long = reshape2::melt(clust_cors)
names(clust_cor_long) = c("Cluster", "Demographic", "Correlation")

clust_cor_long = clust_cor_long %>%
  filter(Cluster %in% clust_names, Demographic %in% dem_cols) %>%
  mutate(Demographic = factor(Demographic, levels = dem_cols))

clust_plot = ggplot(clust_cor_long, aes(x = Demographic, y = Cluster, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), color = "gray70", size = 3) +
  scale_fill_distiller(type = "div", limits = c(-1, 1), expand = c(0, 0)) +
  labs(title = "Correlation between demographics\nand tag clusters",
       x = "Demographic/School Characteristic",
       fill = "Correlation") +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid = element_blank())

ggsave(here("graphs/Clusters and Demographics.png"), width = fig_width)
