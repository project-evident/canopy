# 1. Two charts for Aurora Institute webinar in March (needed as soon as you're
# able to tackle them) Updated "How long schools report implementing core
# practices" (like here) including all 222 schools for which we have 2020-21
# data. (Let me know if you have deep concerns about lumping the Fall 2020 and
# Winter 2020-21 data together for this - but given how we've seen that core
# practices typically have longer tenures, it feels reasonable to me to consider
# all schools that we have semi-recent data on. -> Is there a way to create a
# chart with the "top 10" core practices and implementation tenure, like the
# chart in the blog post above, as well as a long version that shows the "long
# tail" of all practices that any school cites as core? Even wondering if
# there's a dynamic way to do that where users could scroll from side to side,
# or zoom in and out? That latter part (the dynamic version) is totally not
# required, but we are thinking of using it for a webinar coming up on March 10,
# so just curious!

library(tidyverse)
library(googlesheets4)

source("R/import_tags.r")
source("R/import_all.r")
source("R/branding.R")
source("R/import_all.r")
tags = import_tags()
tag_vec = tags$tag

out_folder = "reporting/2021Q1/aurora/"

canopy = import_all()

canopy %>% filter(year == "2020") %>%
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
  filter(is_central == "Central")  %>%
  mutate(
    tag_label = fct_reorder(label_tags(tag), school_id, length, .desc = TRUE),
    duration = case_when(val == "Less than a year" ~ "< 1 year", TRUE ~ val)
  ) ->
  canopy_central

ggplot(canopy_central) +
  aes(x = tag_label, fill = duration) +
  geom_bar() +
  bar_theme +
  bar_y_scale_count +
  labs(
    x = "", y = "Number of schools in 2020 and 2021",
    title = "How long school report implementing core practices",
    fill = "Time implemented"
  ) +
  scale_fill_manual(values = unname(cc_cols[1:4])) +
  #scale_x_discrete(labels = label_tags) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    legend.position = c(.84, .78),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 20, unit = "pt")
  ) -> durations_all

library(plotly)
durations_all_interactive = ggplotly(durations_all, dynamicTicks = TRUE)

library(htmlwidgets)
partial_bundle(durations_all_interactive) %>% 
  saveWidget(file = paste(here::here(), out_folder, "long_tail.html", sep = "/"))



## Static version

canopy_central %>%
  count(tag_label, duration) -> canopy_central_count
canopy_central_count %>%   
  group_by(tag_label) %>%
  summarize(n_this_tag = sum(n)) %>%
  mutate(rank_this_tag = min_rank(-n_this_tag)) %>%
  left_join(canopy_central_count, .) -> canopy_central_count

canopy_central_count %>% filter(rank_this_tag <= 10) %>%
ggplot(aes(x = tag_label, y = n, fill = duration)) +
  geom_col() +
  bar_y_scale_count +
  bar_theme + 
  #coord_flip() +
  #facet_grid(cols = vars(duration)) +
  scale_fill_manual(values = unname(cc_cols[1:4])) +
  labs(
    y = "Number of schools\n(2020 and 2021)",
    x = "",
    fill = "Time implemented",
    #x = "Time practice has been implemented",
    title = "How long schools report implementing\ntop 10 core practices"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    legend.position = c(.84, .75),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 20, unit = "pt")
  ) ->
  top_dur_stack
top_dur_stack
ggsave_cc(top_dur_stack, file = "durations for core tags 2020-2021", dir = out_folder)

