## 2020 Fall COVID Tag Analysis

source("R/branding.R")
out_dir = "reporting/2020/covid/"

library(googlesheets4)
sch_raw = read_sheet(ss = "10wBWoUNIIEGfJ91yUEHsuSp2FRd4F5o8krSsvB0NMvA",
                 sheet = "Fall 2020 School Data", 
                 col_types = "c")

n_sch = sch %>% pull(school_id) %>% n_distinct()

#dict = read_sheet(ss = "10wBWoUNIIEGfJ91yUEHsuSp2FRd4F5o8krSsvB0NMvA", sheet = "Data Dictionary")
# google sheets bad - get tags from AirTable
tags = read_csv("data/Canopy Tags Public Access.csv")
tag_vec = tags %>% pull(`Variable name`)
cov_tags = filter(tags, Cluster == "COVID-19") %>% pull(`Variable name`)

sch = sch_raw %>% 
  select(school_id, any_of(tag_vec)) %>% 
  mutate(across(any_of(tag_vec), ~ !is.na(.)))

sch_tags_all = sch %>%
  pivot_longer(
    cols = any_of(tag_vec),
    names_to = "tag",
    values_to = "val"
  )

sch_tags = sch_tags_all %>%
  filter(val) %>%
  select(-val)

sch_cov = sch_tags %>%
  filter(tag %in% cov_tags)


sch_demo = sch_raw %>%
  select(school_id, matches("_percent"), matches("_grade"), locale, type, student_count, school_state) %>%
  mutate(across(matches("_percent"), ~ as.numeric(str_replace(., "%", "")) / 100)) %>%
  mutate(
    elementary = `1_grade` == "Yes" | `2_grade` == "Yes" | `3_grade` == "Yes" | `4_grade` == "Yes",
    middle = `7_grade` == "Yes",
    high = `10_grade` == "Yes" | `11_grade` == "Yes" | `12_grade` == "Yes",
    student_count = as.numeric(student_count),
    locale = factor(locale, levels = c("Urban", "Suburban", "Rural"))
  )


## Graph 1 - Breakdown of COVID modalities
modalities = c("fully_remote", "fully_in_person", "hybrid")



wide_mode_dat = sch_cov %>%
  filter(tag %in% modalities) %>%
  mutate(val = 1) %>% 
  pivot_wider(id_cols = "school_id", names_from = tag, values_from = val, values_fill = 0)

venn_dat = as.integer(venn_dat[[1]]) * venn_dat[-1]
venn_dat = map(venn_dat, ~.[. != 0])

venn_cols = c("green", "light blue", "purple")
venn_labs = c("fully_in_person" = "Fully\nIn-Person", "hybrid" = "Hybrid", "fully_remote" = "Fully Remote")
VennDiagram::venn.diagram(
  venn_dat %>% set_names(., nm = venn_labs[names(.)]),
  filename = paste0(out_dir, "modality_venn.png"),
  col = venn_cols,
  fill = alpha(venn_cols, 0.3),
  main = "Modalities used by Canopy schools",
  main.fontfamily = "Lato",
  #sub.fontfamily = "Lato",
  cat.fontfamily = "Lato",
  fontfamily = "Lato",
  height = 2100, width = 2100
)

venn_obj = VennDiagram::venn.diagram(
  venn_dat %>% set_names(., nm = venn_labs[names(.)]),
  filename = NULL,
  col = venn_cols,
  fill = alpha(venn_cols, 0.3),
  main = "Modalities used by Canopy schools",
  main.fontfamily = "Lato",
  #sub.fontfamily = "Lato",
  cat.fontfamily = "Lato",
  fontfamily = "Lato",
  height = 2100, width = 2100
)

### This is the way to save grid Venn objects!
ggsave_cc(venn_obj, dir= out_dir, file = "venn", write_data = FALSE, fig_width = 5, fig_height = 5)



venn_pct_obj = VennDiagram::venn.diagram(
  venn_dat %>% set_names(., nm = venn_labs[names(.)]),
  filename = NULL,
  col = venn_cols,
  fill = alpha(venn_cols, 0.3),
  print.mode = "percent",
  sigdigs = 2,
  main = "Modalities used by Canopy schools",
  main.fontfamily = "Lato",
  #sub.fontfamily = "Lato",
  cat.fontfamily = "Lato",
  fontfamily = "Lato",
  height = 2100, width = 2100
)

### This is the way to save grid Venn objects!
ggsave_cc(venn_pct_obj, dir= out_dir, file = "venn_percent", write_data = FALSE, fig_width = 5, fig_height = 5)

## Covid Modalities by demographics
## FRPL and possible also race/ethnicity and locale (urban/sub/rural), level (ES/MS/HS




## Stats - various kinds of staggered schedules
# breakdown by ES, MS, HS
staggered_sched = c("half_days", "alternating_days", "alternating_weeks")

sch_lev = sch_demo %>% select(school_id, elementary, middle, high) %>%
  pivot_longer(-school_id, names_to = "level") %>%
  filter(value) %>%
  select(-value) %>%
  mutate(level = factor(level, levels = c("elementary", "middle", "high")))

sch_lev_agg = sch_lev %>%
  count(level) %>% mutate(tag = NA)

lev_stagger = sch_cov %>% filter(tag %in% staggered_sched) %>%
  inner_join(sch_lev, by = "school_id") %>%
  count(tag, level, name = "n_lev_tag") %>%
  mutate(tag = factor(tag, levels = c("half_days", "alternating_days", "alternating_weeks"))) %>%
  ggplot(aes(tag, y = n_lev_tag, fill = level)) +
  geom_col(position = "dodge") +
  bar_y_scale_count +
  scale_x_tag() +
  labs(
    y = "Number of schools",
    x = "",
    title = "Number of Canopy schools with staggered schedules",
    fill = ""
  ) +
  scale_fill_level() +
  #facet_grid(~level) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 8, r = 8, b = 8, l = 40, unit = "pt"),
        legend.position = c(.1, .9))
lev_stagger

ggsave_cc(lev_stagger, dir = out_dir, file = "stagger by level")



## General - all COVID tags (updated from first post)
sch_cov_agg = sch_cov %>%
  group_by(tag) %>%
  summarize(n = n(), prop = n / n_sch) %>%
  mutate(tag = reorder(tag, -n))
cov_barplot = ggplot(sch_cov_agg, aes(x = tag, y = prop)) + 
  geom_col(fill = cc_cols["light blue"]) +
  bar_y_scale_percent +
  bar_theme +
  labs(
    main = "COVID-19 related practices in Canopy Schools",
    y = "Percent of schools tagged",
    x = ""
  ) +
  scale_x_tag() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 8, r = 8, b = 8, l = 40, unit = "pt"))
ggsave_cc(cov_barplot, file = "covid practices updated", dir = out_dir)



## Other tidbits?
## which tags are highest correlated with modalities?

tag_cor = sch %>% 
  select(any_of(tag_vec)) %>%
  cor(use = "pairwise.complete")



cov_cor = tag_cor[!(rownames(tag_cor) %in% cov_tags), cov_tags]

ss_cor = rowSums(cov_cor^2)
ss_cor_cov = colSums(cov_cor^2)

n_cor_tags = 20
top_cor = cov_cor[order(-ss_cor), ]
top_cor = top_cor[, order(-colSums(top_cor[1:n_cor_tags, ]^2))]

cov_cor_plot = ggcorrplot::ggcorrplot(t(top_cor[n_cor_tags:1, ]), lab = TRUE) +
  labs(fill = "Correlation",
       title = "Practices most correlated with\nCOVID-19 related practices") +
  scale_fill_cc_gradient +
  scale_x_tag() +
  scale_y_tag()
cov_cor_plot

ggsave_cc(cov_cor_plot, file = "COVID tag correlations", dir = out_dir,
          fig_height = 12, fig_width = 13)


corr_cov_only = tag_cor[cov_tags, cov_tags]
cov_only_corr_plot = ggcorrplot::ggcorrplot(corr_cov_only, lab = TRUE, show.diag = FALSE, hc.order = T) +
  labs(fill = "Correlation",
       title = "Correlation between COVID-19 related practices") +
  scale_fill_cc_gradient +
  scale_x_tag() +
  scale_y_tag()

ggsave_cc(cov_only_corr_plot, file = "COVID tag correlations with themselves", dir = out_dir,
          fig_height = 12, fig_width = 13)


## Demogrpahics ####

library(purrr)

continuous_cols = c(
  paste0(c("black", "hispanic", "IDEA", "FRPL"), "_percent"),
  "student_count"
)

colMeans(is.na(sch_demo[continuous_cols]))
# black_percent hispanic_percent     IDEA_percent     FRPL_percent    student_count 
#     0.2291667        0.1944444        0.4652778        0.3402778        0.1805556 
## dropping IDEA for too much missingness. Will try with and without FRPL

continuous_cols = setdiff(continuous_cols, "IDEA_percent")

sch_demo = sch_demo %>% mutate(
  across(any_of(continuous_cols), scale, .names = "{.col}_scaled")
)

sch_demo$charter_fl = as.integer(sch_demo$type == "Charter")
logistic_data = left_join(sch, sch_demo, by = "school_id")

scaled_cols = paste0(continuous_cols, "_scaled")
features = c(scaled_cols, "elementary", "middle", "high", "charter_fl", "locale")
responses = cov_tags
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
  mutate(
    #term = str_replace(term, "charter_fl", "charter"), # Correcting typo without re-running models
    nice_tag = label_tags(response),
    term = str_replace(term, "_scaled|TRUE|_fl", ""),
    term = if_else(term %in% c("middle", "high", "elementary"), paste0("level_", term), term),
    term = str_replace(term, "elementary", "elem"),
    term = str_replace(term, "locale", "locale_"),
    term = if_else(str_detect(term, "locale"), tolower(term), term),
    nice_demog = factor(label_dems(term), levels = unique(dem_labs_rv))
  )


### Plotting

bayes_facet_dat = bayes_tidy %>% group_by(nice_demog) %>%
  arrange(desc(abs(estimate)))# %>%
  #slice(1:10)

bayes_facet = 
  ggplot(bayes_facet_dat,
    aes(y = exp(estimate),
        x = reorder_within(nice_tag, estimate, within = nice_demog, fun = mean),
    )) +
  geom_col() + 
  labs(y = "Odds multiplier",
       x = "", 
       #fill = "Demographic",
       title = "COVID tags by demographics") +
  facet_wrap(~ nice_demog, scales = "free_y") +
  scale_y_continuous(
    trans = "log",
    breaks = c(.125, .25, .5, 1, 2, 4, 8),
    labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
    expand = expansion(0, 0)
  ) +
  scale_x_reordered() + 
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0))

bayes_facet
ggsave_cc(bayes_facet, file = "Logistic COVID Tags small multiples", dir = out_dir, fig_width = 17, write_data = FALSE)

bayes_tidy %>% 
  mutate(
    odds_multiplier = exp(estimate),
    om_l95 = exp(estimate - 1.96 * std.error),
    om_u95 = exp(estimate + 1.96 * std.error),
    om_l80 = exp(estimate - qnorm(0.9) * std.error),
    om_u80 = exp(estimate + qnorm(0.9) * std.error),
  ) %>%
  select(-estimate, -std.error) %>%

write_csv(paste0(out_dir, "logistic COVID tags estimates.csv"))

## Specific plots ####

# Let's look at the Hispanic data

summary(logistic_data$hispanic_percent)

ggplot(logistic_data, aes(x = hispanic_percent, y = as.integer(synchronous_online))) +
  geom_point() +
  bar_y_scale_percent

hisp = logistic_data %>%
  mutate(
    hispanic_bin = cut(hispanic_percent, breaks = seq(0, 1, by = 0.1))
  ) %>%
  group_by(hispanic_bin) %>%
  summarize(
    n_schools = n(),
    n_students = sum(student_count),
    n_sch_with_tag = sum(synchronous_online),
    n_stu_with_tag = sum(student_count * synchronous_online)
  )


# Generic approach

demo_long = logistic_data %>% select(
  any_of(tag_vec),
  school_id,
  ends_with("_percent"), student_count, locale, charter_fl, elementary, middle, high
) %>%
  mutate(across(ends_with("_percent"), cut,
                breaks = seq(0, 1, by = 0.2),
                labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))) %>%
  pivot_longer(cols = !c(school_id, any_of(tag_vec)))

## TODO - make most common locale the first factor

## CLEAN-UP ####

# remove venn logs if present
venn_logs = list.files(out_dir, pattern = ".*\\.log$")
if(length(venn_logs)) {file.remove(paste0(out_dir, venn_logs))}

## move files
file.copy(from = list.files(path = out_dir, full.names = TRUE),
          to = "G:\\Shared drives\\Proj Evident - Clay Christensen Institute\\Fall 2020\\blog 2 - covid response",
          overwrite = TRUE)
