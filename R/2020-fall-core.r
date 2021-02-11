## Conditions that led to innovation (and ranking)

library(qualtRics)
library(tidyverse)
library(forcats)

source("R/branding.R")
source("R/import_2020_data.R")
out_dir = "reporting/2020/core/"

tags = read_csv("data/Canopy Tags Public Access.csv")
tag_vec = tags %>% pull(`Variable name`)


returner_raw = read_survey("data/School Survey_November 9, 2020_13.21.csv")

returner_ranks = returner_raw %>%
  filter(Finished, Status == "IP Address", !is.na(ExternalReference)) %>%
  select(sch_id = ExternalReference, matches("RANK", ignore.case = FALSE))

new_raw = read_survey("data/School Survey - New Nominees_November 9, 2020_13.22.csv")

new_ranks = new_raw %>%
  filter(Finished, Status == "IP Address", !is.na(ExternalReference)) %>%
  select(sch_id = ExternalReference, matches("RANK", ignore.case = FALSE))

all_ranks = bind_rows(returner_ranks, new_ranks)

## Rank labels
rank_labels = c(
  "cutting edge",
  "external catalyst",
  "lack of student agency",
  "lack of teacher agency",
  "change in demographics",
  "negative factors in school",
  "COVID building closures",
  "stakeholder demand",
  "systemic inequities",
  "other"
)

names(all_ranks)[-1] = rank_labels

all_ranks_binary = all_ranks
all_ranks_binary[-1] = !is.na(all_ranks_binary[-1]) 

all_ranks_long = pivot_longer(all_ranks, -sch_id, names_to = "condition", values_to = "rank", values_drop_na = TRUE)

all_ranks_long_agg = all_ranks_long %>%
  count(condition, rank) %>%
  mutate(condition = fct_reorder(condition, -n, .fun = sum))

all_ranks_plot = all_ranks_long_agg %>%
  filter(rank < 6) %>%
  ggplot(aes(x = rank, y = n)) +
  geom_col(fill = cc_cols["green"]) +
  labs(
    x = "School-reported ranking",
    y = "Number of schools",
    title = "Conditions leading schools to adopt their current models"
  ) +
  facet_wrap(~condition) +
  scale_y_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.1)),
    breaks = c(0, 20, 40, 60)
  ) +
  bar_theme + 
  theme(
    panel.border = element_rect(colour = "gray40"),
    strip.text = element_text(size = rel(0.8))
  )

ggsave_cc(all_ranks_plot, "all rankings", dir = out_dir)


ranks_cor = cor(all_ranks[-1], method = "spearman", use = "pairwise.complete.obs")

ranks_cor_plot = ggcorrplot::ggcorrplot(ranks_cor, lab = TRUE, ggtheme = theme_cc) +
  labs(fill = "Correlation",
       title = "Correlations between driving conditions") +
  scale_fill_cc_gradient +
  theme(panel.grid.major = element_blank())

ggsave_cc(ranks_cor_plot, "ranking correlations", dir = out_dir)


## ranking similarities
all_ranks_long %>% 
  filter(rank < 6) %>%
  mutate(sch_order = factor(sch_id),
         sch_order = fct_reorder(sch_order, -rank, mean)) %>%
ggplot(aes(y = condition, x = sch_order, fill = factor(rank))) +
  geom_tile() +
  scale_y_discrete(limits = rev(levels(all_ranks_long_agg$condition))) +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(
    values = scales::seq_gradient_pal(cc_cols["red"], cc_cols["dark blue"])(seq(0, 1, length.out= 5))
  ) +
  theme(panel.grid.major = element_blank())

## TODO 
# table/heatmap rows - factors, cols - ranks - values: counts / percents


## 
bin_rank_plot = all_ranks_long %>%
  count(condition, name = "n_ranked", sort = TRUE) %>%
  mutate(
    condition = factor(condition, levels = unique(condition)),
    p_ranking = n_ranked / n_distinct(all_ranks_long$sch_id)
  ) %>%
ggplot() +
  geom_col(aes(x = condition, y = p_ranking), fill = cc_cols["green"]) +
  bar_theme + 
  bar_y_scale_percent +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))) +
  labs(
    x = "",
    y = "Percent of schools",
    title = "Percent of schools choosing to rank each\nmotivating condition "
  )
ggsave_cc(bin_rank_plot, file = "ranked percentages", dir = out_dir)


############################
#### CORE TAGS ANALYSIS ####

source("R/import_2020_data.R")

t1_tags = tags %>% filter(Tier == "General Approach") %>%
  select(`Tag name`, `Variable name`)

## Only "central" tags were prompted for duration
# ## diagnosing duration issues on core tags:
# 
# core_missing_duration = sch_tags_long_all %>%
#   filter(tag %in% t1_tags$`Variable name`) %>%
#   group_by(tag) %>%
#   count(val) %>%
#   pivot_wider(id_cols = tag, names_from = val, values_from = n, values_fill = 0)
# core_missing_duration
# # # A tibble: 7 x 7
# # # Groups:   tag [7]
# #   tag                    `1` `1-2 years` `3-4 years` `5+ years` `Less than a year`  `NA`
# #   <chr>                <int>       <int>       <int>      <int>              <int> <int>
# # 1 anti_racist_action      74           4           3          6                  4    53
# # 2 blended_learning        77           7          12         11                  5    32
# # 3 competency_education    61           6          11         19                  2    45
# # 4 design_equity           73           6          14          6                  3    42
# # 5 expanded_success        74           3           3         10                  0    54
# # 6 pbl                     44           8          15         41                  1    35
# # 7 sel                     75           8          15         29                  3    14
# 
# # write_csv(core_missing_duration, paste0(out_dir, "core tags duration missingness.csv"))
# 
# 
# all_missing_duration = sch_tags_long_all %>%
#   #filter(tag %in% t1_tags$`Variable name`) %>%
#   group_by(tag) %>%
#   count(val) %>%
#   pivot_wider(id_cols = tag, names_from = val, values_from = n, values_fill = 0)
# #View(all_missing_duration)
# #write_csv(all_missing_duration, paste0(out_dir, "all tags duration missingness.csv"))


## Tags most likely to be "central"

central_tags = sch_tags_long_all %>%
  group_by(tag) %>%
  filter(!is.na(val)) %>%
  mutate(is_central = if_else(val %in% "1", "Not central", "Central")) %>%
  filter("Central" %in% is_central) %>%
  count(is_central) %>%
  mutate(
    total_tagged = sum(n),
    p_central = n[is_central == "Central"] / total_tagged
  ) %>%
  arrange(desc(p_central), desc(total_tagged), tag)

central_plot = central_tags %>%
  ungroup() %>%
  filter(is_central == "Central") %>%
  slice(1:10) %>%
  mutate(
    tag_label = label_tags(tag),
    tag_label = factor(tag_label, levels = unique(tag_label)),
    is_central = factor(is_central, levels = c("Not central", "Central"))
  ) %>%
  ggplot(aes(x = tag_label, y = p_central)) +
  geom_bar(stat = "identity", width = 0.8, fill = cc_cols["purple"]) +
  # scale_fill_manual(
  #   values = c("gray80", unname(cc_cols[c("purple")])),
  #   breaks = c("Not central", "Central")
  # ) +
  # guides(fill = guide_legend(reverse=TRUE)) +
  labs(
    title = 'Top 10 practices indicated as central to a school\'s model',
    x = "",
    y = 'Percent of schools selecting this practice\nindicating is as "central to their model"',
    fill = "Implemented practice\ncentral to school model"
  ) +
  bar_y_scale_percent +
  bar_theme + 
  #scale_y_continuous(labels = scales::percent, expand = expansion(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)))

ggsave_cc(central_plot, file = "tags most commonly central percent", dir = out_dir)

## Chelsea question - are "sel" and "sel_integrated" co-occurring as core tags?
sel_table_central = sch_tags_long_all %>%
  group_by(tag) %>%
  #filter(!is.na(val)) %>%
  mutate(is_central = if_else(val == "1", "Not central", "Central")) %>%
  filter(tag %in% c("sel", "sel_integrated")) %>%
  pivot_wider(id_cols = school_id, names_from = tag, values_from = is_central) %>%
  mutate(across(-school_id, ~coalesce(., "Not tagged"))) %>%
  count(sel, sel_integrated)
write_csv(sel_table_central, paste0(out_dir,"sel vs sel integrated core.csv"))

  
sel_table_tag = sch_tags_long_all %>%
  group_by(tag) %>%
  #filter(!is.na(val)) %>%
  mutate(is_tagged = !is.na(val)) %>%
  filter(tag %in% c("sel", "sel_integrated")) %>%
  pivot_wider(id_cols = school_id, names_from = tag, values_from = is_tagged) %>%
  #mutate(across(-school_id, ~coalesce(., "Not tagged"))) %>%
  count(sel, sel_integrated)
write_csv(sel_table_tag, paste0(out_dir,"sel vs sel integrated tagging.csv"))

all_sel_tags = sch_tags_long_all %>%
  group_by(tag) %>%
  mutate(is_tagged = !is.na(val)) %>%
  filter(str_detect(tag, "sel")) %>%
  pivot_wider(id_cols = school_id, names_from = tag, values_from = is_tagged) %>%
  #mutate(across(-school_id, ~coalesce(., "Not tagged"))) %>%
  count(across(-school_id))


all_sel_core = sch_tags_long_all %>%
  group_by(tag) %>%
  mutate(is_central = if_else(val == "1", "Not central", "Central")) %>%
  filter(str_detect(tag, "sel")) %>%
  pivot_wider(id_cols = school_id, names_from = tag, values_from = is_central) %>%
  mutate(across(-school_id, ~coalesce(., "Not tagged"))) %>%
  count(across(-school_id))


central_count_plot = central_tags %>%
  ungroup() %>%
  slice(1:20) %>%
  mutate(
    tag_label = label_tags(tag),
    tag_label = factor(tag_label, levels = unique(tag_label)),
    is_central = factor(
      is_central, 
      levels = c("Not central", "Central"),
      labels = c("Practice selected", "Practice indicated as central"))
  ) %>%
  ggplot(aes(x = tag_label, y = n, fill = is_central)) +
  geom_bar(stat = "identity", width = 0.8, color = "gray40") +
  scale_fill_manual(
    values = c("gray80", unname(cc_cols[c("purple")])),
    breaks = c("Practice selected", "Practice indicated as central")
  ) +
  #guides(fill = guide_legend(reverse=TRUE)) +
  labs(
    title = 'Top 10 practices indicated as central to school model',
    x = "",
    y = 'Number of schools',
    fill = "Practice "
  ) +
  bar_theme + 
    scale_y_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.20)),
    breaks = scales::breaks_extended(Q = c(1, 5, 2, 4, 3))
)  +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    #legend.position = "top",
    legend.title = element_blank(),
    legend.position = c(0.79, 0.88)
    #legend.box.background = element_rect(size = 0.8, color = "gray40")
  )

ggsave_cc(central_count_plot, file = "tags most commonly central count", dir = out_dir)


## Core Durations ####

durations = durations %>% 
  group_by(tag) %>%
  mutate(n_all_durations = sum(n)) %>%
  ungroup() %>%
  mutate(tag_rank = dense_rank(-n_all_durations)) %>%
  mutate(
    tag_label = factor(label_tags(tag)),
    tag_label = fct_reorder(tag_label, -n, sum),
    tag_label_n = sprintf("%s\n(N = %d)", tag_label, n_all_durations),
    tag_label_n = fct_reorder(tag_label_n, -n, sum)
  ) 

top_dur = durations %>%
  distinct(tag, n_all_durations) %>%
  top_n(n = 10, wt = n_all_durations) %>%
  left_join(durations) %>%
  mutate(tag_label = factor(tag_label, levels = levels(central_count_plot$data$tag_label)))
  
top_dur %>%
  ggplot(aes(x = duration, y = n)) +
  geom_col(fill = cc_cols["green"]) +
  facet_wrap(~tag_label_n, nrow = 2) +
  bar_y_scale_count +
  bar_theme +
  labs(
    y = "Number of schools",
    x = "Time practice has been implemented",
    title = "Time schools have been implementing core practices"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    strip.text = element_text(size = rel(0.8))
  ) ->
  top_dur_facet
top_dur_facet
ggsave_cc(top_dur_facet, file = "durations for core tags - small multiples", dir = out_dir, fig_width = 13)

top_dur %>%
  ggplot(aes(x = duration, y = n, fill = tag_label)) +
  geom_col(position = "dodge", color = "gray20") +
  #facet_wrap(~tag_label_n) +
  bar_y_scale_count +
  bar_theme +
  labs(
    y = "Number of schools",
    x = "Time practice has been implemented",
    title = "Time schools have been implementing core practices",
    fill = "Core practice"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    legend.text = element_text(size = rel(0.8)),
    legend.position = c(0.25, 0.7)
  ) ->
  top_dur_dodge
ggsave_cc(top_dur_dodge, file = "durations for core tags - dodged bars", dir = out_dir)

top_dur %>%
  ggplot(aes(x = duration, y = n, color = tag_label, group = tag_label)) +
  geom_line(size = 2) +
  geom_point() + 
  #facet_wrap(~tag_label_n) +
  bar_y_scale_count +
  bar_theme +
  labs(
    y = "Number of schools",
    x = "Time practice has been implemented",
    title = "Time schools have been implementing core practices"
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))) ->
  top_dur_line
## terrible - don't save

## alternate?
top_dur %>%
  ggplot(aes(x = tag_label, y = n, fill = duration)) +
  geom_col() +
  bar_y_scale_count +
  coord_flip() +
  facet_grid(cols = vars(duration)) +
  scale_fill_manual(values = unname(cc_cols[1:4])) +
  labs(
    y = "Number of schools",
    x = "",
    fill = "History of implementation",
    #x = "Time practice has been implemented",
    title = "Time schools have been implementing core practices"
  ) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))) ->
  top_dur_facet_alt
top_dur_facet_alt
ggsave_cc(top_dur_facet_alt, file = "durations for core tags - multiples by duration", dir = out_dir,
          fig_width = 15, fig_height = 5)


top_dur %>%
  ggplot(aes(x = tag_label, y = n, fill = duration)) +
  geom_col() +
  bar_y_scale_count +
  bar_theme + 
  #coord_flip() +
  #facet_grid(cols = vars(duration)) +
  scale_fill_manual(values = unname(cc_cols[1:4])) +
  labs(
    y = "Number of schools",
    x = "",
    fill = "Time implemented",
    #x = "Time practice has been implemented",
    title = "How long schools report implementing core practices"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    legend.position = c(.84, .78),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 20, unit = "pt")
  ) ->
  top_dur_stack
top_dur_stack
ggsave_cc(top_dur_stack, file = "durations for core tags - stacked", dir = out_dir)


top_dur %>%
  ggplot(aes(x = tag_label, y = n, fill = duration)) +
  geom_col(position = "dodge") +
  bar_y_scale_count +
  bar_theme + 
  #coord_flip() +
  #facet_grid(cols = vars(duration)) +
  scale_fill_manual(values = unname(cc_cols[1:4])) +
  labs(
    y = "Number of schools",
    x = "",
    fill = "History of implementation",
    #x = "Time practice has been implemented",
    title = "Time schools have been implementing core practices"
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))) ->
  top_dur_dodge_duration
top_dur_dodge_duration
ggsave_cc(top_dur_dodge_duration, file = "durations for core tags - dodged durations", dir = out_dir)


## Tier 1 Durations ####

t1_durations = durations %>%
  filter(tag %in% t1_tags$`Variable name`) %>%
  ungroup() %>%
  CC


t1_duration_plot = ggplot(t1_durations, aes(x = duration, y = n)) +
  geom_col(fill = cc_cols["green"]) +
  facet_wrap(~tag_label) +
  bar_y_scale_count +
  bar_theme +
  labs(
    y = "Number of schools",
    x = "Time practice has been implemented",
    title = "Time schools have been implementing Tier 1 practices\nmost central to their school model"
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)))

ggsave_cc(t1_duration_plot, file = "Tier 1 practices with implentation times", dir = out_dir)

## TODO - find other ways to look at this
## also - percent of schools by time (x% of schools selecting this tag as core have been doing it for this long)

## Simple t1 counts
t1_counts = sch_tags_long_all %>%
  filter(tag %in% t1_tags$`Variable name`) %>%
  mutate(tagged = !is.na(val)) %>%
  filter(tagged) %>%
  group_by(tag) %>%
  summarize(
    n_schools = n_distinct(school_id),
    p_schools = n_schools / n_distinct(sch_tags$school_id)
  )


t1_count_plot = ggplot(t1_counts, aes(x = fct_reorder(tag, -p_schools), y = p_schools)) +
  geom_col(fill = cc_cols[1]) +
  bar_y_scale_percent +
  bar_theme +
  scale_x_tag() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))) +
  labs(x = "", y = "Percent of schools",
       title = "Percent of schools indicating each general practice"
       )

ggsave_cc(t1_count_plot, "t1 tags percents", dir = out_dir)
  




## MODELING ####
## Look at top two ranks - cutting edge and systemic equities
## As well as 7 core tags
## See how demos relate

library(purrr)

continuous_cols = c(
  paste0(c("black", "hispanic", "IDEA", "FRPL"), "_percent"),
  "student_count"
)

colMeans(is.na(sch_demo[continuous_cols]))
# black_percent hispanic_percent     IDEA_percent     FRPL_percent    student_count 
#     0.2291667        0.1944444        0.4652778        0.3402778        0.1805556 
## dropping IDEA and FRPL for too much missingness

continuous_cols = setdiff(continuous_cols, c("IDEA_percent", "FRPL_percent"))

sch_demo = sch_demo %>% mutate(
  across(any_of(continuous_cols), scale, .names = "{.col}_scaled")
)

sch_demo$charter_fl = as.integer(sch_demo$type == "Charter")
logistic_data =
  left_join(sch, sch_demo, by = "school_id") %>%
  left_join(
    ## Ranking cols
    all_ranks_binary %>%
      mutate(school_id = as.character(sch_id)) %>%
      select(
        school_id, 
        cutting_edge = `cutting edge`,
        systemic_inequities = `systemic inequities`
    ),
    by = "school_id"
  )

scaled_cols = paste0(continuous_cols, "_scaled")
features = c(scaled_cols, "elementary", "middle", "high", "charter_fl", "locale")
responses = c(t1_tags$`Variable name`, "cutting_edge", "systemic_inequities")
formulas = sprintf("%s ~ %s", responses, paste(features, collapse = " + "))

library(rstanarm)

bayes_mods = list()

for(i in seq_along(formulas)) {
  message("\n\n\nSTARTING MODEL ", i, " OF ", length(formulas), "\n\n\n")
  bayes_mods[[responses[i]]] = stan_glm(
    formulas[i],
    data = logistic_data,
    family = binomial(link = "logit"),
    prior = student_t(
      df = 7,
      location = 0,
      scale = 2.5
    )
  )
}

library(broom)
bayes_tidy = lapply(bayes_mods, broom.mixed::tidy) %>%
  bind_rows(.id = "response") %>%
  filter(term != "(Intercept)") %>%
  dplyr::mutate(
    nice_tag = label_tags(response), 
    term = str_replace(term, "_scaled|TRUE|_fl", ""),
    term = if_else(term %in% c("middle", "high", "elementary"), paste0("level_", term), term),
    term = str_replace(term, "elementary", "elem"),
    term = str_replace(term, "locale", "locale_"),
    term = if_else(str_detect(term, "locale"), tolower(term), term),
    nice_demog = factor(label_dems(term), levels = unique(dem_labs_rv))
  )


### Plotting

plot_bayes_coefs = function(bayes_tidy, title = "Coefficients") {
  bayes_facet_dat = bayes_tidy %>% group_by(nice_demog) %>%
    arrange(desc(abs(estimate)))# %>%
  bayes_facet = 
  ggplot(bayes_facet_dat,
    aes(y = exp(estimate),
        x = reorder_within(nice_tag, estimate, within = nice_demog, fun = mean),
    )) +
  geom_col() + 
  labs(y = "Odds multiplier",
       x = "", 
       #fill = "Demographic",
       title = title) +
  facet_wrap(~ nice_demog, scales = "free_y") +
  scale_y_continuous(
    trans = "log",
    breaks = c(.125, .25, .5, 1, 2, 4, 8),
    labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
    expand = expansion(0, .1)
  ) +
  scale_x_reordered() + 
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0))

  bayes_facet
}

plot_bayes_coefs(bayes_tidy %>% filter(response %in% t1_tags$`Variable name`),
                 title = "Core tags by demographics") %>%
  ggsave_cc(file = "odds ratios core tags", dir = out_dir, write_data = FALSE)


plot_bayes_coefs(bayes_tidy %>% filter(!response %in% t1_tags$`Variable name`),
                 title = "Common drivers by demographics") %>%
  ggsave_cc(file = "odds ratios common drivers", dir = out_dir, write_data = FALSE)



bayes_tidy %>% 
  mutate(
    odds_multiplier = exp(estimate),
    om_l95 = exp(estimate - 1.96 * std.error),
    om_u95 = exp(estimate + 1.96 * std.error),
    om_l80 = exp(estimate - qnorm(0.9) * std.error),
    om_u80 = exp(estimate + qnorm(0.9) * std.error),
  ) %>%
  select(-estimate, -std.error) %>%
write_csv(paste0(out_dir, "logistic core tags estimates.csv"))

# ## Specific plots ####
# 
# # Let's look at the Hispanic data
# 
# summary(logistic_data$hispanic_percent)
# 
# ggplot(logistic_data, aes(x = hispanic_percent, y = as.integer(synchronous_online))) +
#   geom_point() +
#   bar_y_scale_percent
# 
# hisp = logistic_data %>%
#   mutate(
#     hispanic_bin = cut(hispanic_percent, breaks = seq(0, 1, by = 0.1))
#   ) %>%
#   group_by(hispanic_bin) %>%
#   summarize(
#     n_schools = n(),
#     n_students = sum(student_count),
#     n_sch_with_tag = sum(synchronous_online),
#     n_stu_with_tag = sum(student_count * synchronous_online)
#   )


## DONE interactive all-tag correlation for 2020 - just for Chelsea

file.copy(from = list.files(path = out_dir, full.names = TRUE),
          to = "G:\\Shared drives\\Proj Evident - Clay Christensen Institute\\Fall 2020\\blog 3 - core",
          overwrite = TRUE)

