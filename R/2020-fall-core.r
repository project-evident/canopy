## Conditions that led to innovation (and ranking)

library(qualtRics)
library(tidyverse)
library(forcats)

source("R/branding.R")
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




file.copy(from = list.files(path = out_dir, full.names = TRUE),
          to = "G:\\Shared drives\\Proj Evident - Clay Christensen Institute\\Fall 2020\\blog 3 - core",
          overwrite = TRUE)

