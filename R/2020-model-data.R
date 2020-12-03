library(googlesheets4)
library(qualtRics)
source("R/branding.R")
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


## Ranking data
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
  ) %>%
  mutate(
    locale_urban = as.integer(locale == "Urban"),
    locale_suburban = as.integer(locale == "Suburban"),
    locale_rural = as.integer(locale == "Rural")
  )

scaled_cols = paste0(continuous_cols, "_scaled")
features = c(scaled_cols, "elementary", "middle", "high", "charter_fl", "locale")


build_model = function(logistic_data, responses) {
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
  bayes_tidy
}


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


plot_one_mod = function(bayes_tidy, title = "") {
  bayes_facet_dat = bayes_tidy %>% group_by(nice_demog) %>%
    arrange(desc(abs(estimate)))# %>%
  ggplot(bayes_facet_dat,
    aes(y = exp(estimate),
        x = fct_reorder(nice_demog, estimate),
    )) +
  geom_col() + 
  labs(y = "Odds multiplier",
       x = "", 
       title = title) +
  scale_y_continuous(
    trans = "log",
    breaks = c(.125, .25, .5, 1, 2, 4, 8),
    labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
    expand = expansion(0, .1)
  ) +
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0))

}
