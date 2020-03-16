library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(stringr)

confirmed_raw = read_xlsx(here("data", "The-Canopy-Dataset.xlsx"),
                       sheet = "Confirmed Schools"
                    )

# Careful! This is thes schools that were nominated but not confirmed!!
nominated_no_conf = read_xlsx(here("data", "The-Canopy-Dataset.xlsx"),
                       sheet = "Nominated Schools"
                    )

dict_raw = read_xlsx(here("data", "The-Canopy-Dataset.xlsx"),
                       sheet = "Data Dictionary"
                    )

nom_only = read_xlsx(here("data", "Nomination Data Only 2019-03-20.xlsx"),
                       sheet = "Sharable Version Deduped"
                    )



dems = select(
    confirmed_raw,
    school_id,
    nces_id,
    level,
    school_type,
    charter,
    CCD_student_count:total_race_count,
    locale,
    FRPL_count,
    FRPL_percent,
    IDEA_percent,
    LEP_percent
  ) %>%
  mutate(
    non_white_count = CCD_student_count - white_count,
    non_white_percent = non_white_count / CCD_student_count,
    level_high = as.integer(str_detect(level, "High")),
    level_middle = as.integer(str_detect(level, "Middle")),
    level_elem = as.integer(str_detect(level, "Elementary")),
    locale_urban = as.integer(locale == "Urban"),
    locale_suburban = as.integer(locale == "Suburban"),
    locale_rural = as.integer(locale == "Rural"),
  )

# extract tags from dictionary
t1 = dict_raw %>% 
  filter(`Type of data` == "Tag - general approach") %>%
  select(tag = Variable, lab = `Full tag name (tags only)`, desc = Description) %>%
  mutate(tier = 1)


t2 = dict_raw %>%
  filter(`Type of data` == "Tag - specific practice") %>%
  select(tag = Variable, lab = `Full tag name (tags only)`, desc = Description,
         t1_var = `Associated general approach tag (specific practice tags only)`,
         t1_alt = `Alternate tag category (specific practice tags only)`) %>%
  mutate(tier = 2)

t_var = c(t1$tag, t2$tag)

tags = bind_rows(t1, t2)

# create analytic sets
conf = confirmed_raw %>%
  mutate_at(t_var, ~coalesce(., 0))

conf_all_long = conf %>% select(t_var, school_id) %>%
  pivot_longer(cols = -school_id, names_to = "tag") %>%
  left_join(select(tags, tag, tier), by = "tag")


conf_long = conf %>% select(t2$tag, school_id) %>%
  pivot_longer(cols = t2$tag, names_to = "t2")

conf_only = conf %>% select(t_var, school_id) %>%
  pivot_longer(cols = t_var, names_to = "tag", values_to = "conf")

conf_cor = conf %>%
  select(t2$tag) %>%
  cor

all_wide = conf %>% 
  select(t2$tag, t1$tag)
all_cor = cor(all_wide)


write.table(conf_cor, here("data", "t2corr.txt"))

nom_no_conf = nominated_no_conf %>%
  mutate_at(t1$tag, ~coalesce(., 0))


stopifnot(all(t_var %in% names(nom_only)))

## Need to omit, rather than coalesce, instances where 
## T2 nominations were not solicited

nom_only_long = 
  nom_only %>%
  select(school_id, nom_approach, t_var) %>%
  pivot_longer(cols = t_var, names_to = "tag", values_to = "nom") %>% 
  mutate(
    tier = case_when(
      tag %in% t1$tag ~ "T1",
      tag %in% t2$tag ~ "T2",
      TRUE ~ "ERROR"),
    nom = case_when(
      nom == "x" ~ "1",
      is.na(nom) & tag %in% t1$tag ~ "0",
      is.na(nom) & tag %in% t2$tag & nom_approach == "T1_T2" ~ "0",
      TRUE ~ NA_character_
    ),
    nom = as.integer(nom)
  ) %>% 
  filter(!is.na(nom))

nom_conf = nom_only_long %>%
  inner_join(conf_only, by = c("tag", "school_id"))



save(t1, t2, t_var, conf, nom_no_conf, dict_raw, conf_long, conf_all_long, conf_cor, nom_conf, nom_only,
     all_wide, all_cor, tags, dems, 
     file = here("data", "cleaned.rdata"))
