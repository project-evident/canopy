library(tidyverse)
library(googlesheets4)

source("R/import_tags.r")
source("R/import_all.r")
source("R/branding.R")
source("R/import_all.r")
tags = import_tags()
tag_vec = tags$tag

out_folder = "reporting/2021Q1/blog1/"
sheet_id = "10wBWoUNIIEGfJ91yUEHsuSp2FRd4F5o8krSsvB0NMvA"
tab = "Winter 2020-21 School Data"

sch_raw = read_sheet(ss = sheet_id,
                 sheet = tab, 
                 col_types = "c")

cov21 = read_sheet(ss = sheet_id,
                 sheet = "Winter 2020-21 COVID Updates") %>%
  mutate(school_id = as.character(school_id)) %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, .)))

n_sch = sch_raw %>% pull(school_id) %>% n_distinct()

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

## Top T1 (general approach) tags

sch_tags %>%
  inner_join(tags %>% filter(tier == "General Approach") %>% select(tag)) %>%
  count(tag) %>%
  mutate(
    pct = n / n_sch,
    tag = reorder(factor(tag), -pct)
  ) %>%
  ggplot(aes(x = tag, y = pct)) + 
  geom_col(fill = cc_cols["green"]) + 
  bar_y_scale_percent +
  scale_x_discrete(labels = label_tags) +
  labs(x = "", y = "Percent of schools\ntagged in January 2021", 
       title = "General Approaches in Canopy Schools") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 8, r = 8, b = 8, l = 40, unit = "pt")) ->
  t1_pct

ggsave_cc(t1_pct, file = "Tier 1 Pct 2021 Q1", dir = out_folder, write_data = TRUE)

## TODO similar plot for Chelsea with different cohorts side-by-side



## Venn Diagrams  ####

covid_tags = tags %>% filter(cluster == "COVID-19") %>% select(tag)
modalities = c("fully_remote", "fully_in_person", "hybrid")

sch_cov = sch_tags %>%
  filter(tag %in% modalities)

venn_dat = sch_cov %>%
  mutate(val = 1) %>% 
  pivot_wider(id_cols = "school_id", names_from = tag, values_from = val, values_fill = 0) %>%
  ## add in the covid updaters
  bind_rows(cov21 %>% select(school_id, all_of(modalities))) %>%
  select(-school_id) %>%
  mutate(across(everything(), as.logical)) %>%
  rename_with(.fn = label_tags)


library(eulerr)

euler(venn_dat) %>% plot(
  quantities = TRUE,
  fills = list(fill = cc_cols[c(1, 3, 4)], alpha = 0.5),
  labels = list(fontfamily = "Lato Light"),
  main = list(label = "January 2021", fontfamily = "Freight")
)  -> venn_jan21

ggsave_cc(venn_jan21, file = "Venn COVID Jan 21", dir = out_folder, write_data = FALSE)

fall20 = read_sheet(ss = sheet_id,
                 sheet = "Fall 2020 School Data", 
                 col_types = "c")

fall20 %>% select(all_of(modalities)) %>% 
  mutate(across(everything(), ~ !is.na(.))) %>%
  rename_with(.fn = label_tags) -> fall20_covid
fall20_covid  
  euler() %>%
  plot(
    quantities = TRUE,
    fills = list(fill = cc_cols[c(1, 3, 4)], alpha = 0.5),
    labels = list(fontfamily = "Lato Light"),
    main = list(label = "September 2020", fontfamily = "Freight")
)  -> venn_sept20
ggsave_cc(venn_sept20, file = "Venn COVID Sept 20", dir = out_folder, write_data = FALSE)


## COVID intersection: ####
covid_intersection = intersect(fall20$school_id, cov21$school_id)
length(covid_intersection)



fall20 %>% select(school_id, all_of(modalities)) %>% 
  filter(school_id %in% covid_intersection) %>%
  mutate(across(all_of(modalities), ~ !is.na(.))) %>%
  rename_with(.fn = label_tags, .cols = -school_id) %>%
  mutate(time = "September 2020") -> cov20_venn

cov21 %>% select(school_id, all_of(modalities)) %>% 
  filter(school_id %in% covid_intersection) %>%
  #mutate(across(all_of(modalities), ~ !is.na(.))) %>%
  rename_with(.fn = label_tags, .cols = -school_id) %>%
  mutate(time = "January 2021") -> cov21_venn


make_venn = function(
  data, 
  title,
  label = TRUE,
  legend = !label
) {
  data %>%
  select(all_of(label_tags(modalities))) %>%
  euler() %>%
  plot(
    quantities = label,
    fills = list(fill = cc_cols[c(1, 3, 4)], alpha = 0.5),
    labels = if(label) {list(fontfamily = "Lato Light")} else {NULL},
    main = list(label = title, fontfamily = "Freight"),
    legend = legend
  )
}

cov20_venn %>%
  select(all_of(label_tags(modalities))) %>%
  euler() %>%
  plot(
    quantities = TRUE,
    fills = list(fill = cc_cols[c(1, 3, 4)], alpha = 0.5),
    labels = list(fontfamily = "Lato Light"),
    main = list(label = "September 2020", fontfamily = "Freight")
)  -> venn_sept20_intersect


cov21_venn %>%
  select(all_of(label_tags(modalities))) %>%
  euler() %>%
  plot(
    quantities = TRUE,
    fills = list(fill = cc_cols[c(1, 3, 4)], alpha = 0.5),
    labels = list(fontfamily = "Lato Light"),
    main = list(label = "January 2021", fontfamily = "Freight")
)  -> venn_jan21_intersect


ggsave_cc(
  gridExtra::grid.arrange(venn_sept20_intersect, venn_jan21_intersect, nrow = 1),
  file = "Venn Intersection side-by-side labeled", dir = out_folder, write_data = FALSE
)

ggsave_cc(
  gridExtra::grid.arrange(
    make_venn(cov20_venn, title = "September 2020", label = FALSE, legend = FALSE), 
    make_venn(cov21_venn, title = "January 2021", label = FALSE, legend = FALSE),
    nrow = 1),
  file = "Venn Intersection side-by-side unlabeled", dir = out_folder, write_data = FALSE
)

ggsave_cc(
    make_venn(cov20_venn, title = "September 2020", label = FALSE, legend = TRUE),
  file = "Venn Intersection Sept 2020 legend", dir = out_folder, write_data = FALSE
)

ggsave_cc(
    make_venn(cov20_venn, title = "September 2020", label = TRUE, legend = FALSE),
  file = "Venn Intersection Sept 2020", dir = out_folder, write_data = FALSE
)

ggsave_cc(
    make_venn(cov21_venn, title = "January 2021", label = TRUE, legend = FALSE),
  file = "Venn Intersection Jan 2021", dir = out_folder, write_data = FALSE
)


## Central Practices ####

sch_raw %>%
  select(school_id, any_of(tag_vec)) %>%
  pivot_longer(
    cols = any_of(tag_vec),
    names_to = "tag",
    values_to = "val"
  ) %>%
  filter(!is.na(val)) %>%
  mutate(
    is_central = if_else(val %in% "1", "Not central", "Central")
  ) %>%
  group_by(tag) %>%
  filter("Central" %in% is_central) %>%
  count(is_central) %>%
  mutate(
    total_tagged = sum(n),
    p_central = n[is_central == "Central"] / total_tagged,
    n_central = n[is_central == "Central"]
  ) %>%
  arrange(desc(p_central), desc(total_tagged), tag) ->
  central_tags

central_count_plot = central_tags %>%
  arrange(desc(n_central), desc(p_central)) %>%
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
    y = 'Number of schools\ntagged in January 2021',
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
    legend.position = c(0.79, 0.88),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 20, unit = "pt")
    #legend.box.background = element_rect(size = 0.8, color = "gray40")
  )

ggsave_cc(central_count_plot, file = "central tags Jan 21", dir = out_folder)

sch_raw %>%
  select(school_id, any_of(tag_vec)) %>%
  pivot_longer(
    cols = any_of(tag_vec),
    names_to = "tag",
    values_to = "val"
  ) %>%
  filter(!is.na(val) & val != "1") %>%
  count(val) %>%
  mutate(val = relevel(factor(val), "Less than a year")) %>%
  ggplot(aes(x = val, y = n)) +
  geom_col(fill = cc_cols["light blue"]) +
  bar_y_scale_count + 
  bar_theme +
  labs(
    title = "How longs schools have implemented\ntheir central practices",
    x = "", y = ""
  ) -> central_duration_plot

ggsave_cc(central_duration_plot, file = "central durations Jan 21", dir = out_folder)


## Central tags data summary:

central_tags %>%
  filter(is_central == "Central") %>%
  arrange(desc(n_central)) %>%
  View()


## All canopy
canopy = readRDS(most_recent_file("canopy_all", path = "data/"))


canopy %>% group_by(year) %>%
  count(type) %>% 
  na.omit() %>%
  ggplot(aes(x = year, y = n, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_type +
  bar_y_scale_count + 
  labs(
    x = "Canopy survey year",
    y = "",
    title = "Number of Canopy schools by type",
    fill = "School Type"
  ) +
  bar_theme -> canopy_charter
ggsave_cc(canopy_charter, file = "canopy schools type", dir = out_folder)


canopy %>% group_by(year) %>%
  count(type) %>% 
  mutate(prop = n / sum(n)) %>%
  #na.omit() %>%
  ggplot(aes(x = year, y = prop, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_type +
  bar_y_scale_percent+ 
  labs(
    x = "Canopy survey year",
    y = "",
    title = "Percent of Canopy schools by type",
    fill = "School Type"
  ) +
  bar_theme -> canopy_charter_pct
canopy_charter_pct
ggsave_cc(canopy_charter, file = "canopy schools type", dir = out_folder)


canopy %>% group_by(year) %>%
  count(locale) %>% 
  na.omit() %>%
  ggplot(aes(x = year, y = n, fill = locale)) +
  geom_col(position = "dodge") +
  scale_fill_locale +
  bar_y_scale_count + 
  labs(
    x = "Canopy survey year",
    y = "",
    title = "Number of Canopy schools by locale",
    fill = "School Locale"
  ) +
  bar_theme -> canopy_locale
ggsave_cc(canopy_locale, file = "canopy schools locale", dir = out_folder)


canopy %>% 
  ggplot(aes(x = year, y = FRPL_percent, fill = year)) +
  geom_violin() + 
  geom_point(color = "gray40", position = position_jitter(width = 0.1)) +
  bar_y_scale_percent +
  labs(x = "Canopy survey year", title = "Percent FRPL of Canopy Schools", y = "") + 
  scale_fill_manual(values = alpha(unname(cc_cols[c(1, 2, 4)]), 0.3)) -> canopy_frpl_violin
ggsave_cc(canopy_frpl_violin, file = "canopy frpl violinplot", dir = out_folder)


library(glue)
canopy_year_metric = function(
  data, metric,
  save = FALSE,
  print = FALSE
) {
  data %>% 
    ggplot(aes(x = year, y = .data[[metric]], fill = year)) +
    geom_violin() + 
    geom_point(color = "gray40", position = position_jitter(width = 0.1)) +
    bar_y_scale_percent +
    bar_theme +
    labs(x = "Canopy survey year", title = glue("{metric} of Canopy Schools"), y = "") + 
    scale_fill_manual(values = alpha(unname(cc_cols[c(1, 2, 4)]), 0.3)) -> plot
  if(print) print(plot)
  if(save) ggsave_cc(plot, file = glue("canopy {metric} violinplot"), dir = out_folder)
  invisible(plot)
}

canopy_year_metric(canopy, "FRPL_percent", save = FALSE, print = TRUE)
canopy_year_metric(canopy, "white_percent", save = TRUE, print = TRUE)
canopy_year_metric(canopy, "black_percent", save = TRUE, print = TRUE)
canopy_year_metric(canopy, "hispanic_percent", save = TRUE, print = TRUE)
canopy_year_metric(canopy, "IDEA_percent", save = TRUE, print = TRUE)
canopy_year_metric(canopy, "LEP_percent", save = TRUE, print = TRUE)


canopy %>%
  filter(year == "2021") %>%
  ggplot(aes(x = leader_tenure)) +
  geom_bar(fill = cc_cols["purple"]) +
  bar_y_scale_count +
  bar_theme +
  labs(
    x = "Leader Tenure",
    y = "Count of school leaders",
    title = "Leader tenure in 2021 Canopy schools"
  ) -> leader_tenure
ggsave_cc(leader_tenure, file = "leader tenure", dir = out_folder)
