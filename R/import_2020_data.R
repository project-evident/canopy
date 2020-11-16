# import 2020 data
library(tidyverse)

tags = read_csv("data/Canopy Tags Public Access.csv")
tag_vec = tags %>% pull(`Variable name`)

sch_raw = read_tsv("data/Canopy_2020_CSV - Fall 2020 School Data.tsv",
                   col_types = cols(.default = "c"))

sch_tag_wide = sch_raw %>% 
  select(school_id, any_of(tag_vec))

sch_tags_long_all = sch_tag_wide %>%
  pivot_longer(
    cols = any_of(tag_vec),
    names_to = "tag",
    values_to = "val"
  )

sch_tags_long = sch_tags_long_all %>%
  filter(!is.na(val)) %>%
  select(-val)


durations = sch_tags_long_all %>%
  group_by(tag) %>%
  count(val) %>%
  filter(val != "1") %>%
  mutate(
    duration = case_when(val == "Less than a year" ~ "< 1 year", TRUE ~ val),
    prop_within_tag = n / sum(n)
  ) %>%
  select(-val)
