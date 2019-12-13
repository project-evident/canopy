library(readxl)
library(here)
library(dplyr)
library(tidyr)

confirmed_raw = read_xlsx(here("data", "The-Canopy-Dataset.xlsx"),
                       sheet = "Confirmed Schools"
                    )

nominated_raw = read_xlsx(here("data", "The-Canopy-Dataset.xlsx"),
                       sheet = "Nominated Schools"
                    )

dict_raw = read_xlsx(here("data", "The-Canopy-Dataset.xlsx"),
                       sheet = "Data Dictionary"
                    )

# extract tags from dictionary
t1 = dict_raw %>% 
  filter(`Type of data` == "Tag - general approach") %>%
  select(var = Variable, lab = `Full tag name (tags only)`, desc = Description)


## TODO associate t2 labels with verbatim T1 tags. Ask Chelsea about this!
t2 = dict_raw %>%
  filter(`Type of data` == "Tag - specific practice") %>%
  select(var = Variable, lab = `Full tag name (tags only)`, desc = Description,
         t1_var = `Associated general approach tag (specific practice tags only)`,
         t1_alt = `Alternate tag category (specific practice tags only)`)


# create analytic sets
conf = confirmed_raw %>%
  mutate_at(union(t1$var, t2$var), ~coalesce(., 0))

conf_long = conf %>% select(t2$var, school_id) %>%
  pivot_longer(cols = t2$var, names_to = "t2")

conf_cor = conf %>%
  select(t2$var) %>%
  cor


nom = nominated_raw %>%
  mutate_at(t1$var, ~coalesce(., 0))


save(t1, t2, conf, nom, dict_raw, conf_long, conf_cor,
     file = here("data", "cleaned.rdata"))
