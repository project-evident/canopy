## Anti-racist action vs not

#library(qualtRics)
library(tidyverse)
library(forcats)

source("R/branding.R")
out_dir = "reporting/2020/equity/"

tags = read_csv("data/Canopy Tags Public Access.csv")
tag_vec = tags %>% pull(`Variable name`)


library(googlesheets4)
sch_raw = read_sheet(ss = "10wBWoUNIIEGfJ91yUEHsuSp2FRd4F5o8krSsvB0NMvA",
                 sheet = "Fall 2020 School Data", 
                 col_types = "c")

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



ar_mod = build_model(logistic_data, response = "anti_racist_action")

ar_mod_plot = plot_one_mod(ar_mod, title = "Factors associated with\nanti-racist action")

tag_by_demographic(
  tag = "anti_racist_action",
  dem = "black_percent",
  n_bins = 4
)
## TODO try this with non_white_percent

tag_by_demographic(
  tag = "design_equity",
  dem = "black_percent",
  n_bins = 4
)


tag_by_demographic(
  tag = "design_equity",
  dem = "hispanic_percent",
  n_bins = 4
)

## TODO tags by locale - bar chart