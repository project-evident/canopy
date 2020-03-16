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
  geom_vline(xintercept = 0, color = "gray70", size = 1) +
  geom_histogram(fill = cc_cols["green"], binwidth = 0.01) +
  labs(title = "Distribution of differences in tags between\ncharter and traditional schools",
       y = "Count of tags with each difference",
       x = "Percentage points difference\n(% charter with tag minus % traditional with tag") +
  
  theme(panel.grid.major.x = element_blank()) +
  annotate(geom = "text", y = c(5, 5), x = c(-.15, .15),
           label = c("More common\nin traditional", "More common\nin charter"),
           color = "gray30", family = "GillSans-Light") 

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


## Urban vs Suburban  ####

suburb_dat = tagdem %>%
  filter(locale %in% c("Urban", "Suburban"))  %>%
  group_by(tag, locale) %>%
  summarize(n_schools = n(),
            n_schools_with_tag = sum(value),
            p_school_with_tag = mean(value),
            n_students_served = sum(CCD_student_count[value == 1], na.rm = TRUE)) %>%
  group_by(tag) %>%
  mutate(diff = p_school_with_tag[locale == "Urban"] - p_school_with_tag[locale == "Suburban"]) %>%
  arrange(desc(diff)) %>%
  ungroup()

suburb_dat %>%
  group_by(tag) %>%
  summarize(diff = mean(diff)) %>%
  ggplot(aes(x = diff)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = 0, color = "gray70", size = 1) +
  geom_histogram(fill = cc_cols["green"], binwidth = 0.01) +
  labs(title = "Distribution of differences in tags between\nurban and suburban schools",
       y = "Count of tags with each difference",
       x = "Percentage points difference\n(% urban with tag minus % suburban with tag") +
  
  theme(panel.grid.major.x = element_blank()) +
  annotate(geom = "text", y = c(5, 5), x = c(-.2, .2),
           label = c("More common\nin suburban", "More common\nin urban"),
           color = "gray30", family = "GillSans-Light") 
ggsave("graphs/urban_suburban_diff_hist.png", width = fig_width)



suburb_dat %>%
  #slice(1:24) %>%
  filter(diff >= quantile(diff, 0.85)) %>%
  mutate(tag = fct_reorder(tag, -diff)) %>%
  ggplot(aes(x = tag, y = p_school_with_tag, fill = locale)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = unname(cc_cols[c(1, 3)])) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(diff, accuracy = 1)), y = Inf, vjust = 1, color = "gray20") +
  labs(title = "Tags more common in urban than\nsuburban schools (top 15%)",
       x = "Tag", 
       y = "Percent of urban/suburban schools with tag",
       fill = "") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0),
        panel.grid.major.x = element_blank())
ggsave("graphs/urban_suburban_largest_diff_tags.png", width = fig_width)

suburb_dat %>%
  arrange(diff) %>%
  filter(diff <= quantile(diff, 0.15)) %>%
  mutate(tag = fct_reorder(tag, diff)) %>%
  ggplot(aes(x = tag, y = p_school_with_tag, fill = locale)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = unname(cc_cols[c(1, 3)])) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(-diff, accuracy = 1)), y = Inf, vjust = 1, color = "gray20") +
  labs(title = "Tags more common in suburban\n than urban school (top 15%)",
       x = "Tag", 
       y = "Percent of urban/suburban schools with tag",
       fill = "") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0),
        panel.grid.major.x = element_blank()) 
ggsave("graphs/suburban_urban_largest_diff_tags.png", width = fig_width)

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




## Other demo via function ####

# Tag differences
## SPED/IDEA
## ELL

## Idea 1: correlate tags with demo column, look at highest and lowest correlations
## Pros: easy to do, easy to present
## Cons: ignores correlation between demos, correlation coefficients aren't great for
##       categorical vs continuous var


## Idea 2: fit a model for each tag by each demo variables, look at coefficient estimates
## Pro: Appropriate model, comes with estimate and standard error (and p-value...)
## Con: more difficult to present/explain. Still ignores correlation between demos


## Idea 3: fit a big model for each tag by *all* demo variables together.
##        Look at coefficient estimates (or variable importance)
## Pro: begins to account for correlation. Should still remove highly correlated vars
##      (e.g., use only one of black_count and black_percent)
## Cons: To do well, should complicate even further, e.g., use LASSO for some shrinkage
##      Quite a bit harder to explain.

## Ignoring 1, implementing 2 and 3
library(purrr)
logistic_results = list()
for (col in c("black_count", "black_percent", "non_white_percent", "FRPL_count", "FRPL_percent",
              "LEP_percent", "IDEA_percent")) {
logistic_results[[col]] = 
  tagdem %>%
  group_by(tag) %>%
  mutate_at(vars(col), scale) %>%
  group_map(
    ~ broom::tidy(glm(as.formula(paste("value ~", col)), data = .x, family = binomial))[2, ] %>%
      cbind(.y)
  ) %>%
  bind_rows
}

logistic_all = bind_rows(logistic_results)

log_plot_dat = logistic_all %>%
  group_by(term) %>%
  arrange(estimate) %>%
  slice(1:5, (n()-4):n()) %>%
  ungroup() %>%
  mutate(tag = fct_reorder(tag, estimate, .fun = function(x) mean((x))),
         term = factor(term, levels = c("black_count", "black_percent", "non_white_percent",
                                        "FRPL_count", "FRPL_percent", "IDEA_percent", "LEP_percent"),
                       labels = c("Black (count)", "Black (percent)", "Non-White (percent)",
                                  "FRPL (count)", "FRPL (percent)", "IDEA (percent)", "LEP (percent)"))
         )

log_coef_plot = ggplot(log_plot_dat, aes(x = estimate, y = tag, color = term)) +
  geom_point(size = 2) + 
  labs(x = "Logistic regression coefficient",
       y = "Tag", 
       color = "Demographic",
       title = "Most (positive) and least (negative) relations\nbetween tags and continuous demograpics")
log_coef_plot


ggsave("graphs/demog_continuous_all.png", width = fig_width + 3, height = 9)


log_coef_plot_facet = ggplot(
  log_plot_dat, aes(x = estimate, y = reorder_within(tag, estimate, within = term, fun = mean),
                    color = term)) +
  geom_point(size = 3) + 
  labs(x = "Logistic regression coefficient",
       y = "Tag", 
       color = "Demographic",
       title = "Most (positive) and least (negative) relations\nbetween tags and continuous demograpics") +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_reordered() + 
  guides(color = FALSE)

log_coef_plot_facet
ggsave("graphs/demog_continuous_faceted.png", width = fig_width + 5, height = 9)


log_coef_bar_facet = ggplot(
  log_plot_dat, aes(y = estimate, x = reorder_within(tag, estimate, within = term, fun = mean),
                    fill = term)) +
  geom_col() + 
  labs(y = "Logistic regression coefficient",
       x = "Tag", 
       fill = "Demographic",
       title = "Most (positive) and least (negative) relations\nbetween tags and continuous demograpics") +
  facet_wrap(~ term, scales = "free_y") +
  scale_x_reordered() + 
  coord_flip() +
  guides(fill = FALSE)
log_coef_bar_facet
ggsave("graphs/demog_continuous_bar_faceted.png", width = fig_width + 5, height = 9)


one_vars = c("CCD_student_count", "charter_fl", "black_percent", "hispanic_percent", "non_white_percent",
             "locale_urban", "locale_rural", "locale_suburban", "FRPL_percent", "IDEA_percent", "LEP_percent", 
             "level_high", "level_middle", "level_elem")
logistic_one_dat = 
  tagdem %>%
  mutate(locale_urban = as.integer(locale == "Urban"),
         locale_suburban = as.integer(locale == "Suburban"),
         locale_rural = as.integer(locale == "Rural"),
         charter_fl = as.integer(charter == "Yes")) %>%
  select(value, tag, one_of(one_vars)) %>%
  na.omit() %>%
  group_by(tag) %>%
  mutate_at(vars(one_vars), scale)

x = filter(logistic_one_dat, tag == "equity")
glm(value  ~ CCD_student_count + charter_fl + black_percent + hispanic_percent +
      non_white_percent + locale_urban + locale_rural + locale_suburban +
      FRPL_percent + IDEA_percent + LEP_percent + level_high + level_middle + level_elem + 0,
    data = x, family = binomial)


glmnet(x = as.matrix(x[, -(1:2)]), y = x$value, family = "binomial", alpha = 0.5, intercept = FALSE)


logistic_one_coef = logistic_one_dat %>% 
  group_map(
    ~ broom::tidy(glm(
      value  ~ CCD_student_count + charter_fl + black_percent + hispanic_percent +
      non_white_percent + locale_urban + locale_rural + locale_suburban +
      FRPL_percent + IDEA_percent + LEP_percent + level_high + level_middle + level_elem + 0,
   data = .x, family = binomial(link = "logit"))) %>%
      cbind(.y)
  ) %>%
  bind_rows

logistic_one_coef %>% 
  arrange(estimate) %>%
  slice(1:20, (n() - 19):n())

logistic_one_coef %>%
  left_join(e56, by = "tag") %>%
  ggplot(aes(x = term, y = tag, fill = estimate)) +
    geom_tile() +
    facet_wrap(~clust_5, scales = "free") +
  theme(axis.text.x = element_text(angle = -40, hjust = 0, vjust = 0.5))


## Clusters ####
e56 = read_tsv(here("reporting/clusters.tsv"))

plot_cluster_cor = function(
  clust_dat = e56,
  tagdem = tagdem,
  dems = dems,
  clust_col = "clust_5",
  dem_cols = dem_cols
) {
  
  clust_dat = clust_dat[c("tag", clust_col)]
  names(clust_dat)[2] = "Cluster"
  clust_tagdem = tagdem %>% 
    left_join(clust_dat, by = "tag")

  clust = reshape2::dcast(clust_tagdem,
                          school_id ~ Cluster, fun.aggregate = sum, value.var = "value")
  clustdem = clust %>% left_join(dems, by = "school_id")
  
  clust_names = unique(clust_dat[["Cluster"]])
  n_clust = length(clust_names)
  
  clust_cors = clustdem %>%
    select(clust_names, dem_cols) %>%
    mutate(charter = ifelse(charter == "Yes", 1, 0)) %>%
    cor(use = "pairwise.complete.obs")
  
  clust_cors = clust_cors[clust_names, dem_cols]
  
  clust_cor_long = reshape2::melt(clust_cors)
  names(clust_cor_long) = c("Cluster", "Demographic", "Correlation")
  
  clust_cor_long = clust_cor_long %>%
    filter(Cluster %in% clust_names, Demographic %in% dem_cols) %>%
    mutate(Demographic = factor(Demographic, levels = dem_cols))
  
  if("General Approaches" %in% clust_names) {
    clust_cor_long$Cluster = relevel(clust_cor_long$Cluster, ref = "General Approaches")
  }
  
  clust_plot = ggplot(clust_cor_long, aes(x = Demographic, y = Cluster, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = round(Correlation, 2)), color = "gray60", size = 3) +
    scale_fill_distiller(type = "div", limits = c(-1, 1), expand = c(0, 0)) +
    labs(title = sprintf("Correlation between demographics\nand tag clusters (%s clusters)", n_clust),
         x = "Demographic/School Characteristic",
         fill = "Correlation") +
    coord_equal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.grid = element_blank())
  
  return(clust_plot)
}

clust_5_plot = plot_cluster_cor(
  clust_col = "clust_5",
  tagdem = tagdem,
  dems = dems,
  dem_cols = dem_cols
)

ggsave(here("graphs/Clusters (5) and Demographics.png"), clust_5_plot, width = fig_width + 3)

clust_6_plot = plot_cluster_cor(
  clust_col = "clust_6",
  tagdem = tagdem,
  dems = dems,
  dem_cols = dem_cols
)

ggsave(here("graphs/Clusters (6) and Demographics.png"), clust_6_plot, width = fig_width + 5)

